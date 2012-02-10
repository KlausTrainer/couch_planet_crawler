%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet_subscriptions_manager

-module(couch_planet_subscriptions_manager).
-behaviour(gen_server).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

%% user interface
-export([start_link/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, handle_cast/2,
         terminate/2, code_change/3]).

-include("couch_planet.hrl").

-import(couch_planet_utils, [get_http_header_value/2]).

%% External API

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    spawn_link(fun() -> start() end),
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

%% @spec start() -> none()
start() ->
    loop(sets:new(), "").

%% @spec loop(Config::set(), ETag::string()) -> none()
loop(Config0, ETag0) ->
    case read_config(ETag0) of
    {ok, not_modified} ->
        ETag1 = ETag0,
        Config1 = Config0;
    {ok, Urls, ETag1} ->
        Config1 = lists:foldl(
            fun(Url, AccIn) ->
                case sets:is_element(Url, Config0) of
                true ->
                    sets:add_element(Url, AccIn);
                false ->
                    couch_planet_sup:add_child(Url),
                    error_logger:info_msg("Added topic ~p.~n", [Url]),
                    sets:add_element(Url, AccIn)
                end
            end, sets:new(), Urls),
        TopicsToRemove = sets:to_list(sets:subtract(Config0, Config1)),
        lists:foreach(
            fun(Url) ->
                couch_planet_sup:remove_child(Url),
                error_logger:info_msg("Removed topic ~p.~n", [Url])
            end, TopicsToRemove)
    end,
    timer:sleep(?UPDATE_INTERVAL),
    loop(Config1, ETag1).

%% @spec read_config(string()) -> {ok, not_modified} | {ok, Urls::[string()]}
%% @doc Read the couch_planet configuration.
read_config(ETag) ->
    read_config(couch_planet:get_app_env(db_url) ++ "/subscriptions.json", ETag).

%% @spec read_config(string(), string()) -> {ok, not_modified} | {ok, Urls::[string()], ETag::string()}
%% @doc Read the couch_planet configuration from Url.
read_config(Url, ETag) ->
    case ibrowse:send_req(Url, [{"If-None-Match", ETag}], get, [], ?IBROWSE_OPTS) of
    {error, Reason} ->
        error_logger:error_msg("cannot read configuration~nget: ~p - ~p~n", [Url, Reason]),
        timer:sleep(?LATER),
        read_config(Url, ETag);
    {ok, "304", _, _} ->
        {ok, not_modified};
    {ok, "200", Headers, Body} ->
        case parse_config(Body) of
        {error, Reason} ->
            error_logger:error_msg("cannot parse configuration from ~p - ~p~n", [Url, Reason]),
            timer:sleep(?LATER),
            read_config(Url, ETag);
        Urls ->
            {ok, Urls, get_http_header_value("etag", Headers)}
        end;
    {ok, {{_, Code, _}, _Headers, _Body}} ->
        error_logger:error_msg("cannot read configuration~nget: ~p - ~p~n", [Url, Code]),
        timer:sleep(?LATER),
        read_config(Url, ETag)
    end.


%% Internal API

%% @spec parse_config(binary()) -> Urls::[string()] | {error, Reason}
parse_config(Config) ->
    {struct, L} = mochijson2:decode(Config),
    case lists:keyfind(<<"topics">>, 1, L) of
    false ->
        {error, "no topics"};
    {<<"topics">>, Topics} ->
        case parse_topics(Topics) of
        {error, Reason} -> {error, Reason};
        Urls -> Urls
        end
    end.

%% @spec parse_topics([tuple()]) -> Urls::[string()] | {error, Reason}
parse_topics(Topics) ->
    parse_topics(Topics, []).

parse_topics([], Acc) ->
    Acc;
parse_topics([H|T], Acc) ->
    {struct, L} = H,
    case lists:keyfind(<<"url">>, 1, L) of
    false -> {error, "url field missing in topic"};
    {<<"url">>, Url} -> parse_topics(T, [?b2l(Url)|Acc])
    end.
