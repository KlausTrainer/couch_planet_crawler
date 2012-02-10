%%% This file is part of the couch_planet package and is released under the
%%% Tumbolia Public License. See LICENSE for more details.
%%%
%%% @author Klaus Trainer <klaus_trainer@posteo.de>

%%% @doc Library of utility functions for couch_planet.

-module(couch_planet_utils).

-author('Klaus Trainer <klaus_trainer@posteo.de>').

%% user interface
-export([get_http_header_value/2, absolute_url_from_url_and_location/2,
         xml2xml_text2json_string/1]).

%% @spec get_http_header_value(Key::string(), [{string(), string()}]) ->
%%           undefined | string()
%% @doc The key must be lower case.
get_http_header_value(Key, Headers) ->
    get_http_header_value0(Key, lists:map(fun({K, V}) -> {string:to_lower(K), V} end, Headers)).

get_http_header_value0(_Key, []) ->
    undefined;
get_http_header_value0(Key, [{Key, Value}|_]) ->
    Value;
get_http_header_value0(Key, [_|T]) ->
    get_http_header_value0(Key, T).

%% @spec absolute_url_from_url_and_location(AbsoluteUrl::string(), AbsoluteOrRelativeUrl::string()) -> NewAbsoluteUrl::string()
%% @doc Some servers put relative URLs into the Location header.
%%      This is against the standard, but we want to handle that nevertheless.
absolute_url_from_url_and_location(Url, Location) ->
    case re:run(Location, "^https?://", [{capture, none}]) of
    match -> Location;
    nomatch -> re:replace(Url, "^(https?://[^/]+)", "\\1", [{return, list}]) ++ Location
    end.

%% @spec xml2xml_text2json_string(binary()) -> binary()
xml2xml_text2json_string(Xml) ->
    case get(cdata_regex) of
    undefined ->
        {ok, MP} = re:compile(<<"<!\\[CDATA\\[(.*?)\\]\\]>">>, [dotall]),
        put(cdata_regex, MP);
    MP ->
        ok
    end,
    Xml1 = case re:run(Xml, MP, [{capture, [1], binary}]) of
    nomatch -> Xml;
    {match, [Text]} -> Text
    end,
    case get(json_regex_0) of
    undefined ->
        {ok, MP0} = re:compile(<<"[\b\f\n\r\t]">>),
        put(json_regex_0, MP0);
    MP0 ->
        ok
    end,
    XmlText0 = re:replace(Xml1, MP0, <<"">>, [global, {return, binary}]),
    XmlText1 = re:replace(XmlText0, <<"&gt;">>, <<">">>, [global, {return, binary}]),
    XmlText2 = re:replace(XmlText1, <<"&lt;">>, <<"<">>, [global, {return, binary}]),
    XmlText3 = re:replace(XmlText2, <<"&amp;">>, <<"\\&">>, [global, {return, binary}]),
    JsonText0 = re:replace(XmlText3, <<"\"">>, <<"\\\\\"">>, [global, {return, binary}]),
    case get(json_regex_1) of
    undefined ->
        {ok, MP1} = re:compile(<<"(\\\\\\\\)+\"">>),
        put(json_regex_1, MP1);
    MP1 ->
        ok
    end,
    JsonText1 = re:replace(JsonText0, MP1, <<"\\1\\\\\"">>, [global, {return, binary}]),
    JsonText2 = re:replace(JsonText1, <<"\\\\u">>, <<"\\\\\\\\u">>, [global, {return, binary}]),
    case get(json_regex_2) of
    undefined ->
        {ok, MP2} = re:compile(<<"(\\\\\\\\)+\\\\u">>),
        put(json_regex_2, MP2);
    MP2 ->
        ok
    end,
    JsonText3 = re:replace(JsonText2, MP2, <<"\\1\\\\\\\\u">>, [global, {return, binary}]),
    case get(json_regex_3) of
    undefined ->
        {ok, MP3} = re:compile(<<"([^\\\\])\\\\([^\\\\\"])">>),
        put(json_regex_3, MP3);
    MP3 ->
        ok
    end,
    re:replace(JsonText3, MP3, <<"\\1\\2">>, [global, {return, binary}]).
