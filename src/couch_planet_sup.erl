%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc Supervisor for the couch_planet_subscriptions_manager process,
%%%      as well as for the couch_planet_crawler processes.

-module(couch_planet_sup).
-behaviour(supervisor).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

%% user interface
-export([start_link/0, add_child/1, remove_child/1]).
%% supervisor callbacks
-export([init/1]).


%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec add_child(Url::string()) -> ServerRet
add_child(Url) ->
    ChildSpec = {Url, {couch_planet_crawler, start_link, [Url]},
                 permanent, 5000, worker, [couch_planet_crawler]},
    supervisor:start_child(?MODULE, ChildSpec).

%% @spec remove_child(Url::string()) -> ok | {error, Reason}
remove_child(Url) ->
    supervisor:terminate_child(?MODULE, Url),
    supervisor:delete_child(?MODULE, Url).

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    {ok, {{one_for_one, 5, 10},
        [{couch_planet_subscriptions_manager, {couch_planet_subscriptions_manager, start_link, []},
         permanent, 5000, worker, [couch_planet_subscriptions_manager]}]}}.
