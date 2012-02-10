-define(UPDATE_INTERVAL, 180000).

-define(LATER, crypto:rand_uniform(20000, 90000)).

-define(MAX_TRIES, 8).

-define(IBROWSE_OPTS, [{connect_timeout, 16000}, {response_format, binary}]).

-define(DETS_OPTS, [{auto_save, 8000}, {ram_file, true}, {repair, force}]).

-define(exponential_backoff(Count),
    if ?MAX_TRIES div Count >= 4 ->
        timer:sleep(?LATER);
    ?MAX_TRIES div Count >= 2 ->
        timer:sleep(2 * ?LATER);
    true ->
        timer:sleep(4 * ?LATER)
    end).

-define(a2l(V), atom_to_list(V)).
-define(b2l(V), binary_to_list(V)).
-define(b2t(V), binary_to_term(V)).
-define(i2l(V), integer_to_list(V)).
-define(l2a(V), list_to_atom(V)).
-define(l2b(V), list_to_binary(V)).
-define(l2i(V), list_to_integer(V)).
-define(t2b(V), term_to_binary(V)).


-record(object, {
    id = <<>>,
    link = <<>>,
    summary = <<>>,
    objectType = <<>>
}).

-record(actor, {
    link = <<>>,
    name = <<>>,
    objectType = <<"author">>
}).

-record(provider, {
    id = <<>>,
    name = <<>>,
    objectType = <<"service">>
}).

-record(entry, {
    id = <<>>,
    rev = <<>>,
    title = <<>>,
    postedTime = <<>>,
    type = <<"Activity Stream">>,
    verb = <<"post">>,
    object = #object{},
    actor = #actor{},
    provider = #provider{}
}).
