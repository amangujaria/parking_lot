-module(spot_tests).

-include_lib("eunit/include/eunit.hrl").

parking_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [park_test_(),
     park_error_test_()]}.

wrong_input_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [wrong_input_when_parked_test_(),
     wrong_input_when_vacant_test_()]}.

park_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot), spot_sup:start_child(spot1) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(permitted, spot:generate_event(spot1, {"KA-01-HH-1234", "White"})),
     ?_assertEqual(permitted, spot:generate_event(spot1, vacate))]}.

park_error_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot), spot_sup:start_child(spot1) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(permitted, spot:generate_event(spot1, {"KA-01-HH-1234", "White"})),
     ?_assertEqual(denied, spot:generate_event(spot1, {"KA-01-HH-1234", "White"})),
     ?_assertEqual(permitted, spot:generate_event(spot1, vacate))]}.

no_effect_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot), spot_sup:start_child(spot1) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(already_vacant, spot:generate_event(spot1, vacate))]}.

wrong_input_when_parked_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot), spot_sup:start_child(spot1) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(permitted, spot:generate_event(spot1, {"KA-01-HH-1234", "White"})),
     ?_assertEqual(incomprehensible, spot:generate_event(spot1, randomevent)),
     ?_assertEqual(permitted, spot:generate_event(spot1, vacate))]}.

wrong_input_when_vacant_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot), spot_sup:start_child(spot1) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(incomprehensible, spot:generate_event(spot1, randomevent))]}.

nonexistent_spot_test_() ->
    {
    setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(noproc, spot:generate_event(spot777, {"KA-01-HH-1234", "White"})),
     ?_assertEqual(noproc, spot:generate_event(spot333, randomevent)),
     ?_assertEqual(noproc, spot:generate_event(spot111, vacate))]}.
