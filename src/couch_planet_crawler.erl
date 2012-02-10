%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet_crawler

-module(couch_planet_crawler).
-behaviour(gen_server).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

%% user interface
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("couch_planet.hrl").

-import(couch_planet_utils, [get_http_header_value/2,
                             absolute_url_from_url_and_location/2]).

%% External API

start_link(Url) ->
    gen_server:start_link(?MODULE, Url, []).

init(Url) ->
    spawn_link(fun() -> start(Url) end),
    {ok, 0}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Internal API

%% @spec start(string()) -> none() | {error, Reason}
start(Url) ->
    DbUrl = couch_planet:get_app_env(db_url),
    ViewUrl = DbUrl ++ "/_design/couch_planet/_view/update_status?key=\"" ++ Url ++ "\"",
    case ibrowse:send_req(ViewUrl, [], get, [], ?IBROWSE_OPTS) of
    {error, Reason} ->
        error_logger:error_msg("get: ~p - ~p~n", [ViewUrl, Reason]),
        timer:sleep(?LATER),
        start(Url);
    {ok, "200", _, Body} ->
        case parse_update_status_view(Body) of
        {error, Reason} ->
            error_logger:error_msg("cannot parse update status view for feed ~p - ~p~n", [Url, Reason]),
            timer:sleep(?LATER),
            start(Url);
        UpdateStatus ->
            StatusTable = ets:new(update_status, [public, set]),
            ets:insert(StatusTable, UpdateStatus),
            loop(Url, StatusTable)
        end;
    {ok, Code, _, _} ->
        error_logger:error_msg("get: ~p - ~p~n", [ViewUrl, Code]),
        timer:sleep(?LATER),
        start(Url)
    end.

%% @spec loop(string(), tid()) -> none()
loop(Url, StatusTable) ->
    case couch_planet:get_app_env(fetch_images) orelse couch_planet:get_app_env(fetch_audio) of
    true ->
        UrlHash = ?i2l(erlang:phash2(Url)),
        DetsFile = case os:type() of
        {win32, _} ->
            os:getenv("HOMEDRIVE") ++ os:getenv("HOMEPATH")
                ++ "\\.couch_planet_" ++ UrlHash;
        _ ->
            os:getenv("HOME") ++ "/.couch_planet_" ++ UrlHash
        end,
        case dets:open_file(make_ref(), [{file, DetsFile}|?DETS_OPTS]) of
        {ok, Dets} ->
            % we don't want the crawler processes to start all at the same time
            timer:sleep(crypto:rand_uniform(60000, 120000)),
            couch_planet_file_fetcher:spawn_link_file_fetcher(Dets, StatusTable),
            loop(Url, Dets, StatusTable, "");
        {error, Reason} ->
            exit("cannot open dets table - " ++ Reason)
        end;
    false ->
        loop(Url, nil, StatusTable, "")
    end.

%% @spec loop(string(), nil|ref(), tid(), string()) -> none()
loop(Url, Dets, StatusTable, DocHash) ->
    case get_doc_if_modified(Url, DocHash) of
    false ->
        NewDocHash = DocHash;
    {error, _Reason} ->
        NewDocHash = DocHash;
    {ok, {NewDocHash, ContentType, Doc}} ->
        case couch_planet_parser:new_entries(Doc, ContentType, Url, StatusTable) of
        {error, Reason} ->
            error_logger:error_msg("cannot parse feed from ~p - ~p~n", [Url, Reason]);
        [] ->
            ok;
        Entries ->
            do_bulk_docs_request(entries_to_json(Entries), StatusTable),
            case Dets =/= nil of
            true -> fetch_files(Entries, Dets, StatusTable);
            false -> ok
            end
        end
    end,
    timer:sleep(?UPDATE_INTERVAL),
    loop(Url, Dets, StatusTable, NewDocHash).

%% @spec do_bulk_docs_request(binary(), tid()) -> ok
do_bulk_docs_request(Json, StatusTable) ->
    BulkDocsUrl = couch_planet:get_app_env(bulk_docs_url),
    case ibrowse:send_req(BulkDocsUrl, [{"Content-Type", "application/json"}], post, Json, ?IBROWSE_OPTS) of
    {error, Reason} ->
        error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Reason]),
        timer:sleep(?LATER),
        do_bulk_docs_request(Json, StatusTable);
    {ok, "201", _, ResJson} ->
        case parse_bulk_docs_response(ResJson) of
        {error, Reason} ->
            error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Reason]);
        IdRevTuples ->
            lists:foreach(
                fun({Id, Rev}) ->
                    true = ets:update_element(StatusTable, Id, {3, Rev})
                end, IdRevTuples),
            ok
        end;
    {ok, Code, _, _} ->
        error_logger:error_msg("post: ~p - ~p~n", [BulkDocsUrl, Code]),
        timer:sleep(?LATER),
        do_bulk_docs_request(Json, StatusTable)
    end.

%% @spec parse_bulk_docs_response(binary()) -> [{binary(), binary()}] | {error, Reason}
parse_bulk_docs_response(B) ->
    lists:foldl(
        fun(E, Acc) ->
            {struct, TupleList} = E,
            case lists:keyfind(<<"id">>, 1, TupleList) of
            false ->
                {error, "no 'id' field in bulk update response"};
            {<<"id">>, Id} ->
                case lists:keyfind(<<"rev">>, 1, TupleList) of
                false ->
                    case lists:keyfind(<<"error">>, 1, TupleList) of
                    false ->
                        {error, "could not update"};
                    {<<"error">>, Error} ->
                        {error, "could not update: " ++ ?b2l(Error)}
                    end;
                {<<"rev">>, Rev} ->
                    [{Id, Rev}|Acc]
                end
            end
        end, [], mochijson2:decode(B)).

%% @spec parse_update_status_view(binary()) -> [{binary(), binary(), binary()}] | {error, Reason}
parse_update_status_view(B) ->
    {struct, L} = mochijson2:decode(B),
    case lists:keyfind(<<"rows">>, 1, L) of
    false ->
        {error, "no rows field in view 'update_status'"};
    {<<"rows">>, Rows} ->
        [{Title, TimeStamp, Rev} || {struct, [_Id, _Key, {<<"value">>, [Title, TimeStamp, Rev]}]} <- Rows]
    end.

%% @spec entries_to_json([#entry{}]) -> binary()
entries_to_json(Entries) ->
   entries_to_json(Entries, <<"{\"all_or_nothing\":false,\"docs\":[">>).

entries_to_json([H|T], Acc) ->
    case T of
    [] ->
        Entry = entry_to_json(H),
        <<Acc/binary,Entry/binary,"]}">>;
    _ ->
        Entry = entry_to_json(H),
        entries_to_json(T, <<Acc/binary,Entry/binary,",">>)
    end.

%% @spec entry_to_json(#entry{}) -> binary()
entry_to_json(Entry) ->
    #entry{id=Id, rev=Rev, title=Title, postedTime=PostedTime, type=Type,
           verb=Verb, object=Object, actor=Actor, provider=Provider} = Entry,
    #object{id=OId, link = OLink, summary=OSummary, objectType=OType} = Object,
    #actor{link=ALink, name=AName, objectType=AType} = Actor,
    #provider{id=PId, name=PName, objectType=PType} = Provider,
    B = case Rev of
    <<>> -> <<"{\"_id\":\"",Id/binary,"\",">>;
    _ -> <<"{\"_id\":\"",Id/binary,"\",\"_rev\":\"",Rev/binary,"\",">>
    end,
    <<B/binary,"\"title\":\"",Title/binary,"\",\"postedTime\":\"",PostedTime/binary,"\",\"type\":\"",Type/binary,"\",\"verb\":\"",Verb/binary,"\",\"object\":{\"id\":\"",OId/binary,"\",\"link\":\"",OLink/binary,"\",\"summary\":\"",OSummary/binary,"\",\"objectType\":\"",OType/binary,"\"},\"actor\":{\"link\":\"",ALink/binary,"\",\"name\":\"",AName/binary,"\",\"objectType\":\"",AType/binary,"\"},\"provider\":{\"id\":\"",PId/binary,"\",\"name\":\"",PName/binary,"\",\"objectType\":\"",PType/binary,"\"}}">>.

%% @spec get_doc_if_modified(string(), string()) -> false | {ok, {DocHash, ContentType, Doc}} | {error, Reason}
get_doc_if_modified(Url, DocHash) ->
    case ibrowse:send_req(Url, [], head, [], ?IBROWSE_OPTS) of
    {error, Reason} ->
        error_logger:error_msg("head: ~p - ~p~n", [Url, Reason]),
        {error, Reason};
    {ok, "200", Headers, _} ->
        DocHash1 = get_http_header_value("etag", Headers),
        case DocHash1 of
        DocHash ->
            false;
        _ ->
            case ibrowse:send_req(Url, [], get, [], ?IBROWSE_OPTS) of
            {error, Reason} ->
                error_logger:error_msg("get: ~p - ~p~n", [Url, Reason]),
                {error, Reason};
            {ok, "200", Headers1, Body} ->
                NewDocHash = case DocHash1 of
                undefined ->
                    % so, we need to compute a document hash code ourselves
                    ?i2l(erlang:phash2(Body));
                _ ->
                    DocHash1
                end,
                case NewDocHash of
                DocHash ->
                    false;
                _ ->
                    {ok, {NewDocHash, get_content_type(Headers1, Body), Body}}
                end;
            {ok, Code, _, _} ->
                error_logger:error_msg("get: ~p - ~p~n", [Url, Code]),
                {error, Code}
            end
        end;
    {ok, Code, Headers, _} when Code == "301"; Code == "302"; Code == "303" ->
        case get_http_header_value("location", Headers) of
        undefined ->
            error_logger:error_msg("head: ~p - ~p~n", [Url, Code]),
            {error, Code};
        Location ->
            Location2 = absolute_url_from_url_and_location(Url, Location),
            get_doc_if_modified(Location2, DocHash)
        end;
    {ok, Code, _, _} ->
        error_logger:error_msg("head: ~p - ~p~n", [Url, Code]),
        {error, Code}
    end.

%% @spec fetch_files([#entry{}], ref(), tid()) -> ok
fetch_files(Entries, Dets, StatusTable) ->
    EntryUrlsTuples = couch_planet_file_fetcher:preprocess_entries(Entries,
        StatusTable),
    case EntryUrlsTuples of
    [] ->
        ok;
    _ ->
        IdUrlsTuples = [{Entry#entry.id, Urls} || {Entry, Urls} <- EntryUrlsTuples],
        Tuples = lists:foldl(
            fun({Id, Urls}, Acc) ->
                lists:foldl(
                    fun(Url, Acc1) ->
                        [{{Url, Id}, <<>>}|Acc1]
                    end, Acc, Urls)
            end, [], IdUrlsTuples),
        ok = dets:insert(Dets, Tuples),
        dets:sync(Dets),
        Entries1 = [element(1, Tuple) || Tuple <- EntryUrlsTuples],
        do_bulk_docs_request(entries_to_json(Entries1), StatusTable)
    end.

get_content_type(Headers, Body) ->
    case get_content_type_from_headers(Headers) of
    undefined -> get_content_type_from_body(Body);
    ContentType -> ContentType
    end.

get_content_type_from_headers(Headers) ->
    case get_http_header_value("content-type", Headers) of
    undefined ->
        undefined;
    ContentType0 ->
        BreakPos = string:rstr(ContentType0, ";") - 1,
        ContentType = case BreakPos > 0 of
        true -> string:sub_string(ContentType0, 1, BreakPos);
        _ -> ContentType0
        end,
        case ContentType of
        "application/rss+xml" -> rss;
        "application/atom+xml" -> atom;
        _ -> undefined
        end
    end.

get_content_type_from_body(Doc) ->
    case re:run(Doc, <<"<feed[ >]">>, [{capture, none}]) of
    match ->
        atom;
    nomatch ->
        case re:run(Doc, <<"<channel[ >]">>, [{capture, none}]) of
        match -> rss;
        nomatch -> undefined
        end
    end.
