-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").

create_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(created, utils:create_parking_lot(["5"]))]}.

park_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["1"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(allowed, utils:park(["KA-01-1234", "Blue"]))]}.

park_nospace_created_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(not_allowed, utils:park(["KA-01-1234", "Blue"]))]}.

park_nospace_available_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["1"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(allowed, utils:park(["KA-01-1234", "Blue"])),
    ?_assertEqual(not_allowed, utils:park(["KA-01-1235", "Black"]))]}.

leave_occupied_space_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["1"]), utils:park(["KA-01-1234", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(success, utils:leave(["1"]))]}.

leave_unoccupied_space_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["2"]), utils:park(["KA-01-1234", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(failure, utils:leave(["2"]))]}.

reg_numbers_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(["KA-01-1235"], utils:registration_numbers_for_cars_with_colour(["White"]))]}.

reg_numbers_multiple_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(["KA-01-1234", "KA-01-1236"], utils:registration_numbers_for_cars_with_colour(["Blue"]))]}.

reg_numbers_failure_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual([], utils:registration_numbers_for_cars_with_colour(["Black"]))]}.

slot_numbers_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(["2"], utils:slot_numbers_for_cars_with_colour(["White"]))]}.

slot_numbers_multiple_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(["1", "3"], utils:slot_numbers_for_cars_with_colour(["Blue"]))]}.

slot_numbers_failure_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual([], utils:slot_numbers_for_cars_with_colour(["Black"]))]}.

matching_slot_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(2, utils:slot_number_for_registration_number(["KA-01-1235"]))]}.

matching_slot_failure_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["3"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(not_found, utils:slot_number_for_registration_number(["KA-01-1237"]))]}.

find_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot(["5"]), utils:park(["KA-01-1234", "Blue"]), utils:park(["KA-01-1235", "White"]), utils:park(["KA-01-1236", "Blue"]) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual([spot2], utils:find([id], [{colour, "White"}])),
    ?_assertEqual([spot1, spot3], utils:find([id], [{colour, "Blue"}])),
    ?_assertEqual([spot1], utils:find([id], [{colour, "Blue"}, {registration, "KA-01-1234"}])),
    ?_assertEqual([[spot1, "KA-01-1234"], [spot3, "KA-01-1236"]], utils:find([id, registration], [{colour, "Blue"}])),
    ?_assertEqual([spot4, spot5], utils:find([id], [{status, vacant}]))]}.
