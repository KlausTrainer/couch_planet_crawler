%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet

-module(couch_planet).
-author('Klaus Trainer <klaus_trainer@posteo.de>').

-include("couch_planet.hrl").

%% user interface
-export([start/0, start/1, stop/0]).
%% intermodule exports
-export([get_app_env/1, get_app_env/2]).


%% External API

%% @spec start([string()]) -> ok
%% @doc Start couch_planet.
start([DbUrl]) ->
    application:set_env(couch_planet, fetch_images, false),
    application:set_env(couch_planet, fetch_audio, false),
    start1(DbUrl);
start(Args) when length(Args) =:= 2->
    [A1, A2] = Args,
    case A1 of
    "fetch-images" ->
        application:set_env(couch_planet, fetch_images, true),
        application:set_env(couch_planet, fetch_audio, false),
        start1(A2);
    "fetch-audio" ->
        application:set_env(couch_planet, fetch_images, false),
        application:set_env(couch_planet, fetch_audio, true),
        start1(A2);
    _ ->
        application:set_env(couch_planet, fetch_images, false),
        application:set_env(couch_planet, fetch_audio, false),
        bye()
    end;
start(Args) when length(Args) =:= 3->
    [A1, A2, A3] = Args,
    case A1 of
    "fetch-images" when A2 =:= "fetch-audio" ->
        application:set_env(couch_planet, fetch_images, true),
        application:set_env(couch_planet, fetch_audio, true),
        start1(A3);
    "fetch-audio" when A2 =:= "fetch-images" ->
        application:set_env(couch_planet, fetch_images, true),
        application:set_env(couch_planet, fetch_audio, true),
        start1(A3);
    _ ->
        application:set_env(couch_planet, fetch_images, false),
        application:set_env(couch_planet, fetch_audio, false),
        bye()
    end.

%% @spec stop() -> ok
%% @doc Stop the couch_planet application and the calling process.
stop() ->
    stop("couch_planet stop requested").

%% @spec stop(string()) -> ok
%% @doc Stop the couch_planet server.
stop(Reason) ->
    error_logger:info_msg(io_lib:format("~p~n", [Reason])),
    application:stop(couch_planet).

%% @spec get_app_env(atom()) -> term()
%% @doc The official way to get the values set in couch_planet's configuration
%%      file. Will return `undefined' if that option is unset.
get_app_env(Opt) ->
    get_app_env(Opt, undefined).

%% @spec get_app_env(atom(), term()) -> term()
%% @doc The official way to get the values set in couch_planet's configuration
%%      file. Will return `Default' if that option is unset.
get_app_env(Opt, Default) ->
    case application:get_env(couch_planet, Opt) of
    {ok, Val} ->
        Val;
    _ ->
        case init:get_argument(Opt) of
        {ok, [[Val|_]]} -> Val;
        error -> Default
        end
    end.


%% Internal API

bye() ->
    exit("invalid design document URL").

%% @spec start1(string()) -> ok
start1(DbUrl0) ->
    DbUrl = string:strip(DbUrl0, right, $/),
    BulkDocsUrl = DbUrl ++ "/_bulk_docs",
    application:set_env(couch_planet, db_url, DbUrl),
    application:set_env(couch_planet, bulk_docs_url, BulkDocsUrl),
    application:start(couch_planet).

%% @spec start() -> ok
%% @doc Start the couch_planet server.
start() ->
    application:start(couch_planet).
