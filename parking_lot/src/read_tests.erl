-module(read_tests).

-include_lib("eunit/include/eunit.hrl").

create_lot_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    [?_assertEqual(created, read:process("create_parking_lot", [1000]))]}.

create_lot_failure_test_() ->
    [?_assertEqual(not_allowed, read:process("create_parking_lot", ["1000"]))].

incorrect_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(error, read:process("leave", ["1", "2"])),
    ?_assertEqual(error, read:process("leave", [])),
    ?_assertEqual(error, read:process("status", ["one"])),
    ?_assertEqual(error, read:process("park", ["KA-01-1234"])),
    ?_assertEqual(error, read:process("park", ["KA-01-1234", "White", "extraparam"])),
    ?_assertEqual(error, read:process("registration_numbers_for_cars_with_colour", [])),
    ?_assertEqual(error, read:process("registration_numbers_for_cars_with_colour", ["White", "Blue"])),
    ?_assertEqual(error, read:process("slot_numbers_for_cars_with_colour", [])),
    ?_assertEqual(error, read:process("slot_numbers_for_cars_with_colour", ["White", "Blue"])),
    ?_assertEqual(error, read:process("slot_number_for_registration_number", [])),
    ?_assertEqual(error, read:process("slot_number_for_registration_number", ["KA-01-1234", "KA-01-1234"]))]}.

correct_test_() ->
    {setup,
    fun() -> application:ensure_all_started(parking_lot), utils:create_parking_lot([2]), spot_sup:start_child(spot1), spot_sup:start_child(spot2) end,
    fun(_) -> application:stop(parking_lot) end,
    [?_assertEqual(allowed, read:process("park", ["KA-01-1234", "White"])),
    ?_assertEqual(ok, read:process("status", [])),
    ?_assertEqual(["KA-01-1234"], read:process("registration_numbers_for_cars_with_colour", ["White"])),
    ?_assertEqual([], read:process("registration_numbers_for_cars_with_colour", ["Blue"])),
    ?_assertEqual([1], read:process("slot_numbers_for_cars_with_colour", ["White"])),
    ?_assertEqual([], read:process("slot_numbers_for_cars_with_colour", ["Blue"])),
    ?_assertEqual(1, read:process("slot_number_for_registration_number", ["KA-01-1234"])),
    ?_assertEqual(not_found, read:process("slot_number_for_registration_number", ["KA-01-1235"])),
    ?_assertEqual(success, read:process("leave", ["1"])),
    ?_assertEqual(failure, read:process("leave", ["2"])),
    ?_assertEqual(impossible, read:process("leave", ["3"]))]}.
