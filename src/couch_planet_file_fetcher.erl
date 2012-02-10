%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet_image_persister

-module(couch_planet_file_fetcher).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

%% user interface
-export([preprocess_entries/2, spawn_link_file_fetcher/2]).

-include("couch_planet.hrl").

-import(couch_planet_utils, [get_http_header_value/2,
                             absolute_url_from_url_and_location/2]).

%% External API

%% @spec preprocess_entries([#entry{}], tid()) -> [{#entry{}, Urls::[string()]}]
preprocess_entries(Entries, StatusTable) ->
    lists:foldl(
        fun(Entry, Acc) ->
            Id = Entry#entry.id,
            Object0 = Entry#entry.object,
            {Summary, Urls0} = case couch_planet:get_app_env(fetch_images) of
            true -> process_img_tags(Id, Object0#object.summary);
            false -> {Object0#object.summary, []}
            end,
            Object1 = Object0#object{summary = Summary},
            {Object2, Urls1} = case couch_planet:get_app_env(fetch_audio) of
            true ->
                case Object1#object.objectType of
                <<"podcast">> ->
                    {Obj, Url} = process_enclosure(Id, Object1),
                    {Obj, [Url|Urls0]};
                _ ->
                    {Object1, Urls0}
                end;
            false ->
                {Object1, Urls0}
            end,
            case Urls1 of
            [] ->
                Acc;
            _ ->
                [{_Id, _TimeStamp, Rev}] = ets:lookup(StatusTable, Id),
                [{Entry#entry{rev = Rev, object = Object2}, Urls1}|Acc]
            end
        end, [], Entries).

%% @spec spawn_link_file_fetcher(ref(), tid()) -> ok
spawn_link_file_fetcher(Dets, StatusTable) ->
    Table = ets:new(file_attacher, [public, set]),
    spawn_link(
        fun() ->
            file_attacher(Dets, Table, StatusTable)
        end),
    ok.


%% Internal API

%% @spec file_attacher(ref(), tid(), tid()) -> none
file_attacher(Dets, Table, StatusTable) ->
    case dets:first(Dets) of
    '$end_of_table' ->
        timer:sleep(2000),
        file_attacher(Dets, Table, StatusTable);
    Key ->
        case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, <<>>}),
            {Url, EntryId} = Key,
            spawn(
                fun() ->
                    file_attacher(EntryId, ?b2l(Url), Dets, Table, StatusTable)
                end);
        _ ->
            timer:sleep(100)
        end,
        file_attacher(Key, Dets, Table, StatusTable)
    end.

%% @spec file_attacher({binary(), binary()}, ref(), tid(), tid()) -> none
file_attacher(CurrentKey, Dets, Table, StatusTable) ->
    case dets:next(Dets, CurrentKey) of
    '$end_of_table' ->
        timer:sleep(2000),
        file_attacher(Dets, Table, StatusTable);
    Key ->
        case ets:lookup(Table, Key) of
        [] ->
            ets:insert(Table, {Key, <<>>}),
            {Url, EntryId} = Key,
            spawn(
                fun() ->
                    file_attacher(EntryId, ?b2l(Url), Dets, Table, StatusTable)
                end);
        _ ->
            timer:sleep(100)
        end,
        file_attacher(Key, Dets, Table, StatusTable)
    end.

%% @spec file_attacher(binary(), string(), ref(), tid(), tid()) -> ok
file_attacher(EntryId, Url, Dets, Table, StatusTable) ->
    file_attacher(EntryId, Url, Dets, Table, StatusTable, 0).

file_attacher(EntryId, Url, Dets, Table, _StatusTable, ?MAX_TRIES) ->
    Key = {?l2b(Url), EntryId},
    ok = dets:delete(Dets, Key),
    true = ets:delete(Table, Key),
    error_logger:error_msg("get: ~p - max_retries~n", [Url]);
file_attacher(EntryId, Url, Dets, Table, StatusTable, ReqCount) ->
    Key = {?l2b(Url), EntryId},
    case ibrowse:send_req(Url, [], get, [], ?IBROWSE_OPTS) of
    {error, socket_closed_remotely} ->
        error_logger:error_msg("get: ~p - socket_closed_remotely~n", [Url]),
        ?exponential_backoff(ReqCount),
        file_attacher(EntryId, Url, Dets, Table, StatusTable, ReqCount + 1);
    {error, Reason} ->
        error_logger:error_msg("get: ~p - ~p~n", [Url, Reason]),
        timer:sleep(?LATER),
        file_attacher(EntryId, Url, Dets, Table, StatusTable, ReqCount);
    {ok, Code, _, _} when Code >= "400" andalso Code < "500" ->
        ok = dets:delete(Dets, Key),
        true = ets:delete(Table, Key),
        error_logger:error_msg("get: ~p - ~p~n", [Url, Code]);
    {ok, Code, _, _} when Code >= "500" ->
        error_logger:error_msg("get: ~p - ~p~n", [Url, Code]),
        ?exponential_backoff(ReqCount),
        file_attacher(EntryId, Url, Dets, Table, StatusTable, ReqCount + 1);
    {ok, Code, Headers, _} when Code == "301"; Code == "302"; Code == "303" ->
        case get_http_header_value("location", Headers) of
        undefined ->
            Key = {?l2b(Url), EntryId},
            ok = dets:delete(Dets, Key),
            true = ets:delete(Table, Key),
            error_logger:error_msg("get: ~p - redirect_without_location~n", [Url]);
        Location ->
            Location2 = absolute_url_from_url_and_location(Url, Location),
            file_attacher(EntryId, Location2, Dets, Table, StatusTable, ReqCount)
        end;
    {ok, "200", Headers, Body} ->
        ContentType = case get_http_header_value("content-type", Headers) of
        undefined -> "application/octet-stream";
        Value -> Value
        end,
        DocName = edoc_lib:escape_uri(?b2l(EntryId)),
        UrlHash = ?i2l(erlang:phash2(?l2b(Url))),
        AttUrl = couch_planet:get_app_env(db_url) ++ "/" ++ DocName ++ "/" ++ UrlHash,
        attach_file(EntryId, AttUrl, ContentType, Body, StatusTable),
        ok = dets:delete(Dets, Key),
        true = ets:delete(Table, Key),
        ok
    end.

attach_file(EntryId, AttUrl, ContentType, Data, StatusTable) ->
    case ets:lookup(StatusTable, EntryId) of
    [{_EntryId, _TimeStamp, Rev0}] when Rev0 =/= <<>> ->
        ReqHeaders = [{"If-Match", ?b2l(Rev0)}, {"Content-Type", ContentType}],
        case ibrowse:send_req(AttUrl, ReqHeaders, put, Data, ?IBROWSE_OPTS) of
        {error, Reason} ->
            error_logger:error_msg("put: ~p - ~p~n", [AttUrl, Reason]),
            timer:sleep(?LATER),
            attach_file(EntryId, AttUrl, ContentType, Data, StatusTable);
        {ok, "409", _, _} ->
            timer:sleep(crypto:rand_uniform(1000, 4000)), % just wait a bit
            DocUrl = string:substr(AttUrl, 1, string:rchr(AttUrl, $/) - 1),
            update_rev(EntryId, DocUrl, StatusTable),
            attach_file(EntryId, AttUrl, ContentType, Data, StatusTable);
        {ok, "201", Headers, _} ->
            % just let it crash if there's no ETag response header:
            Rev1 = ?l2b(get_http_header_value("etag", Headers)),
            % the revision numbers in the ETags are enclosed in double quotes,
            % which we need to remove
            Rev2 = re:replace(Rev1, <<"\"">>, <<>>, [global, {return, binary}]),
            true = ets:update_element(StatusTable, EntryId, {3, Rev2}),
            ok;
        {ok, Code, _, _} ->
            error_logger:error_msg("put: ~p - ~p~n", [AttUrl, Code]),
            timer:sleep(?LATER),
            attach_file(EntryId, AttUrl, ContentType, Data, StatusTable)
         end;
     _ ->
        ok
     end.

update_rev(EntryId, DocUrl, StatusTable) ->
    case ibrowse:send_req(DocUrl, [], head, [], ?IBROWSE_OPTS) of
    {error, Reason} ->
        error_logger:error_msg("head: ~p - ~p~n", [DocUrl, Reason]),
        timer:sleep(?LATER),
        update_rev(EntryId, DocUrl, StatusTable);
    {ok, "200", Headers, _} ->
        % just let it crash if there's no ETag response header:
        Rev1 = ?l2b(get_http_header_value("etag", Headers)),
        % the revision numbers in the ETags are enclosed in double quotes,
        % which we need to remove
        Rev2 = re:replace(Rev1, <<"\"">>, <<>>, [global, {return, binary}]),
        true = ets:update_element(StatusTable, EntryId, {3, Rev2}),
        ok;
    {ok, Code, _, _} ->
        error_logger:error_msg("head: ~p - ~p~n", [DocUrl, Code]),
        timer:sleep(?LATER),
        update_rev(EntryId, DocUrl, StatusTable)
     end.

%% @spec process_img_tags(binary(), binary()) -> {binary(), Urls::[binary()]}
process_img_tags(EntryId, Summary) ->
    process_img_tags(EntryId, Summary, {<<>>, []}).

%% @spec process_img_tags(binary(), binary(), {binary(), [string()]}) -> {binary(), Urls::[binary()]}
process_img_tags(EntryId, Data, {SummaryAcc, Urls}) ->
    case find_next_img_tag(Data) of
    false ->
        {<<SummaryAcc/binary,Data/binary>>, Urls};
    {StartOffs, Length} ->
        <<Prefix:StartOffs/binary,Url:Length/binary,Rest/binary>> = Data,
        DocName = ?l2b(edoc_lib:escape_uri(?b2l(EntryId))),
        UrlHash = ?l2b(?i2l(erlang:phash2(Url))),
        NewUrl = <<"../../../../",DocName/binary,"/",UrlHash/binary>>,
        process_img_tags(EntryId, Rest, {<<SummaryAcc/binary,Prefix/binary,NewUrl/binary>>, [Url|Urls]})
    end.

find_next_img_tag(Data) ->
    case get(img_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<img ([^>]* +)??src=('|\\\\\")(https?://.+?)('|\\\\\")( +[^>]*)??(>|/>)">>, [caseless, dotall]),
        put(img_regex, MP);
    MP ->
        ok
    end,
    case re:run(Data, MP, [{capture, [3]}]) of
    nomatch -> false;
    {match, [{StartOffs, Length}]} -> {StartOffs, Length}
    end.

%% @spec process_enclosure(binary(), #object{}) -> {#object{}, Url::binary()}
process_enclosure(EntryId, Object) ->
    DocName = ?l2b(edoc_lib:escape_uri(?b2l(EntryId))),
    Link = Object#object.link,
    UrlHash = ?l2b(?i2l(erlang:phash2(Link))),
    NewUrl = <<"../../../../",DocName/binary,"/",UrlHash/binary>>,
    {Object#object{link = NewUrl}, Link}.
