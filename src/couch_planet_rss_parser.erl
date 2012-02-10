%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc couch_planet_rss_parser

-module(couch_planet_rss_parser).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

-include("couch_planet.hrl").

%% user interface
-export([title/1, find_feed_entries/1,
         entry_link/1, entry_time/1, complete_entry/3]).

-import(couch_planet_utils, [xml2xml_text2json_string/1]).


% External API

%% @spec title(binary()) -> binary()
title(Xml) ->
    case get(title_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<title( +[^>]*)??>(.*?)</title>">>,
            [caseless, dotall]),
        put(title_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2], binary}]) of
    nomatch -> <<>>;
    {match, [Title]} -> xml2xml_text2json_string(Title)
    end.

%% @spec find_feed_entries(binary()) -> [binary()]
find_feed_entries(Xml) ->
    find_feed_entries(Xml, []).

%% @spec entry_link(binary()) -> binary() | false
entry_link(Xml) ->
    case entry_link_0(Xml) of
    false -> entry_enclosure(Xml);
    Value -> Value
    end.

%% @spec entry_time(binary()) -> binary() | false
entry_time(Xml) ->
    case entry_pub_date(Xml) of
    false -> entry_date(Xml);
    Value -> Value
    end.

%% @spec complete_entry(binary(), #entry{}, binary()) -> #entry{}
complete_entry(EntryData, Entry, Link) ->
    ObjectType = case get(has_enclosure) of
    true -> <<"podcast">>;
    _ -> <<"article">>
    end,
    Summary = entry_content(EntryData),
    ActorName = entry_author(EntryData),
    #entry{object=Object0, actor=Actor0} = Entry,
    Object = Object0#object{id = Link, link = Link, summary = Summary, objectType = ObjectType},
    Actor = Actor0#actor{name = ActorName},
    Entry#entry{title = title(EntryData), object = Object, actor = Actor}.


%% Internal API

find_feed_entries(Xml, Acc) ->
    case find_next_entry(Xml) of
    false ->
        Acc;
    {StartOffs, Length} ->
        Len = size(<<"</item>">>),
        <<_:StartOffs/binary,Value:Length/binary,_:Len/binary,Rest/binary>> = Xml,
        find_feed_entries(Rest, [Value|Acc])
    end.

find_next_entry(Xml) ->
    case get(entry_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<item( +[^>]*)??>(.*?)</item>">>,
            [caseless, dotall]),
        put(entry_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2]}]) of
    nomatch -> false;
    {match, [{StartOffs, Length}]} -> {StartOffs, Length}
    end.

%% @spec entry_pub_date(binary()) -> binary() | false
entry_pub_date(Xml) ->
    case get(entry_pub_date_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<pubDate( +[^>]*)??>(.*?)</pubDate>">>,
            [caseless, dotall]),
        put(entry_pub_date_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2], binary}]) of
    nomatch -> false;
    {match, [Value]} -> xml2xml_text2json_string(Value)
    end.

%% @spec entry_date(binary()) -> binary() | false
entry_date(Xml) ->
    case get(entry_date_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<([^>]+?:)??date( +[^>]*)??>(.*?)</\\1date>">>,
            [caseless, dotall]),
        put(entry_date_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [3], binary}]) of
    nomatch -> false;
    {match, [Value]} -> xml2xml_text2json_string(Value)
    end.

%% @spec entry_link_0(binary()) -> binary() | false
entry_link_0(Xml) ->
    case get(entry_link_0_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<link( +[^>]*)??>(.*?)</link>">>, [caseless, dotall]),
        put(entry_link_0_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2], binary}]) of
    nomatch -> false;
    {match, [Link]} -> xml2xml_text2json_string(Link)
    end.

%% @spec entry_enclosure(binary()) -> binary() | false
%% @doc If the return value is not `false', the flag `has_enclosure' is set in the process dictionary.
entry_enclosure(Xml) ->
    case get(entry_enclosure_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<enclosure ([^>]* +)??url=['\"](https?://.+?)['\"]( +[^>]*)??(>|/>)">>, [caseless, dotall]),
        put(entry_enclosure_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [2], binary}]) of
    nomatch ->
        false;
    {match, [Link]} ->
        put(has_enclosure, true),
        xml2xml_text2json_string(Link)
    end.

%% @spec entry_content(binary()) -> binary()
entry_content(Xml) ->
    case get(entry_content_0_regex) of
    undefined ->
        {ok, MP0} = re:compile(<<"<content:encoded>(.*?)</content:encoded>">>,
            [caseless, dotall]),
        put(entry_content_0_regex, MP0);
    MP0 ->
        ok
    end,
    case re:run(Xml, MP0, [{capture, [1], binary}]) of
    nomatch ->
        case get(entry_content_1_regex) of
        undefined ->
            {ok, MP1} = re:compile(<<"<description( +[^>]*)??>(.*?)</description>">>,
                [caseless, dotall]),
            put(entry_content_1_regex, MP1);
        MP1 ->
            ok
        end,
        case re:run(Xml, MP1, [{capture, [2], binary}]) of
        nomatch -> <<>>;
        {match, [Content]} -> xml2xml_text2json_string(Content)
        end;
    {match, [Content]} -> xml2xml_text2json_string(Content)
    end.

%% @spec entry_author(binary()) -> binary()
entry_author(Xml) ->
    case entry_author_0(Xml) of
    <<>> -> entry_author_1(Xml);
    Value -> Value
    end.

entry_author_0(Xml) ->
    case get(entry_author_0_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<([^>]+?:)??author( +[^>]*)??>(.*?)</\\1author>">>, [caseless, dotall]),
        put(entry_author_0_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [3], binary}]) of
    nomatch -> <<>>;
    {match, [Value]} -> xml2xml_text2json_string(Value)
    end.

entry_author_1(Xml) ->
    case get(entry_author_1_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<([^>]+?:)??creator( +[^>]*)??>(.*?)</\\1creator>">>, [caseless, dotall]),
        put(entry_author_1_regex, MP);
    MP ->
        ok
    end,
    case re:run(Xml, MP, [{capture, [3], binary}]) of
    nomatch -> <<>>;
    {match, [Value]} -> xml2xml_text2json_string(Value)
    end.
