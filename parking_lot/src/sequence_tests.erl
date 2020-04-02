-module(sequence_tests).

-include_lib("eunit/include/eunit.hrl").

create_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(created, gen_server:call(sequence, {create_lot, 10}))]}.

create_error_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(erroneous_msg, gen_server:call(sequence, {create_lot, "10"})),
    ?_assertEqual(erroneous_msg, gen_server:call(sequence, {create_lot, -1})),
    ?_assertEqual(erroneous_msg, gen_server:call(sequence, {create_lot, 0}))]}.

no_space_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot([2]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(not_available, gen_server:call(sequence, find_empty))]}.

find_vacant_spot_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot([2]), utils:park(["KA-01-1234", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(spot2, gen_server:call(sequence, find_empty))]}.

return_spot_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot([2]), utils:park(["KA-01-1234", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(spot2, gen_server:call(sequence, find_empty)),
     ?_assertEqual(returned, gen_server:call(sequence, {return, spot1})),
     ?_assertEqual(spot1, gen_server:call(sequence, find_empty))]}.
