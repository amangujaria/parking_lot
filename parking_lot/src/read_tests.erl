-module(read_tests).

-include_lib("eunit/include/eunit.hrl").

process_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    [?_assertEqual(created, read:process("create_parking_lot", ["1000"]))]}.

failure_test_() ->
    [?_assertEqual(error, read:process("create_parking_lot", [1000]))].
