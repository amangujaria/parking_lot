-module(sequence_tests).

-include_lib("eunit/include/eunit.hrl").

create_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(created, gen_server:call(sequence, {create_lot, 10}))]}.

find_empty_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(not_available, gen_server:call(sequence, find_empty))]}.
