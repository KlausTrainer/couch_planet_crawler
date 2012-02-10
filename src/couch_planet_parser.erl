%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet_parser

-module(couch_planet_parser).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

-include("couch_planet.hrl").

%% user interface
-export([new_entries/4]).

% External API

%% @spec new_entries(binary(), atom(), string(), tid()) ->
%%           [#entry{}] | {error, Reason}
new_entries(_, ContentType, _, _) when ContentType =/= atom andalso ContentType =/= rss ->
    {error, lists:flatten(["unknown content type: " | atom_to_list(ContentType)])};
new_entries(Xml, ContentType, Url, StatusTable) ->
    Module = case ContentType of
    atom -> couch_planet_atom_parser;
    rss -> couch_planet_rss_parser
    end,
    Provider = #provider{id = ?l2b(Url), name = Module:title(Xml)},
    EntriesXml = Module:find_feed_entries(Xml),
    get_new_entries(Module, EntriesXml, Provider, StatusTable).


%% Internal API

%% @spec get_new_entries(atom(), [binary()], #provider{}, tid()) -> [#entry{}]
get_new_entries(Module, Entries, Provider, StatusTable) ->
    get_new_entries(Module, Entries, Provider, StatusTable, []).

get_new_entries(_Module, [], _Provider, _StatusTable, Acc) ->
    Acc;
get_new_entries(Module, [H|T], Provider, StatusTable, Acc) ->
    case Module:entry_link(H) of
    false ->
        % ignore entries without link
        get_new_entries(Module, T, Provider, StatusTable, Acc);
    Link ->
        case Module:entry_time(H) of
        false ->
            % ignore entries without time specification
            get_new_entries(Module, T, Provider, StatusTable, Acc);
        Time ->
            case ets:lookup(StatusTable, Link) of
            [{Link, Time0, Rev}] ->
                case Time0 of
                Time ->
                    get_new_entries(Module, T, Provider, StatusTable, Acc);
                _ ->
                    true = ets:update_element(StatusTable, Link, {2, Time}),
                    Entry0 = #entry{id = Link, rev = Rev, postedTime = Time,
                        provider = Provider},
                    Entry = Module:complete_entry(H, Entry0, Link),
                    get_new_entries(Module, T, Provider, StatusTable, [Entry|Acc])
                end;
            [] ->
                true = ets:insert(StatusTable, {Link, Time, <<>>}),
                Entry0 = #entry{id = Link, postedTime = Time,
                    provider = Provider},
                Entry = Module:complete_entry(H, Entry0, Link),
                get_new_entries(Module, T, Provider, StatusTable, [Entry|Acc])
            end
        end
    end.
