-module(spot_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_spot_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertMatch({ok, P} when is_pid(P) == true, spot_sup:start_child(spot1))]}.
