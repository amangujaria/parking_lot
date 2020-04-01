-module(utils).

-export([create_parking_lot/1, park/1, leave/1, status/1, registration_numbers_for_cars_with_colour/1, slot_numbers_for_cars_with_colour/1, slot_number_for_registration_number/1, find/2]).

-spec create_parking_lot([string()]) -> created.
create_parking_lot([Num]) ->
    NumInt = list_to_integer(Num),
    Existing = length(ets:tab2list(spots)),
    lists:map(fun(Elem) ->
        SpotId = list_to_atom("spot" ++ integer_to_list(Elem)),
        spot_sup:start_child(SpotId)
    end, lists:seq(Existing + 1, Existing + NumInt)),
    created = gen_server:call(sequence, {create_lot, NumInt}, infinity),
    io:format("Created a parking lot with ~p slots~n", [NumInt]),
    created.

-spec park(list(string())) -> allowed | not_allowed.
park([RegNum, Colour]) ->
    Spot = gen_server:call(sequence, find_empty, infinity),        
    if Spot =/= not_available ->
        permitted = spot:generate_event(Spot, {RegNum, Colour}),
        io:format("Allocated slot number: ~p~n", [list_to_integer(atom_to_list(Spot) -- "spot")]),
        allowed;
    true ->
        io:format("Sorry, parking lot is full~n", []),
        not_allowed
    end.

-spec leave([string()]) -> success | failure.
leave([Id]) ->
    Spot = list_to_atom("spot" ++ Id),
    case spot:generate_event(Spot, vacate) of
        permitted ->
	    returned = gen_server:call(sequence, {return ,Spot}, infinity),
	    io:format("Slot number ~p is free~n", [list_to_integer(Id)]),
            success;
        already_vacant -> io:format("Slot number ~p is already free~n", [list_to_integer(Id)]),
            failure
    end.

-spec status([]) -> ok.
status([]) ->
    State = utils:find([id, registration, colour], [{status, occupied}]),
    io:format("~p such spots ~n", [length(State)]),
    io:format("Slot No.\tRegistration No.\tColour~n", []),
    lists:foreach(fun([SpotId, Registration, Colour]) ->
        Id = list_to_integer(atom_to_list(SpotId) -- "spot"),
%%        io:format("~p\t\t~s\t\t~s~n", [Id, list_to_atom(Registration), list_to_atom(Colour)])
        io:format("~p\t\t~s\t\t~s~n", [Id, Registration, Colour])
    end, lists:usort(State)).

-spec registration_numbers_for_cars_with_colour([string()]) -> list(string()).
registration_numbers_for_cars_with_colour([Colour]) ->
    Numbers = find([registration], [{colour, Colour}]),
    if Numbers =/= [] ->
        DisplayVal = lists:foldl(fun(Elem, Acc) ->
            Elem ++ ", " ++ Acc
        end, hd(Numbers), tl(Numbers)),
        io:format("~s~n", [DisplayVal]),
        lists:usort(Numbers);
    true ->
        io:format("No ~p cars in parking lot currently~n", [Colour]),
        []
    end.

-spec slot_numbers_for_cars_with_colour([string()]) -> list(string()).
slot_numbers_for_cars_with_colour([Colour]) ->
    Slots = find([id], [{colour, Colour}]),
    if Slots =/= [] ->
        SlotNumbers = lists:map(fun(Slot) -> atom_to_list(Slot) -- "spot" end, lists:usort(Slots)),
        RevSlots = lists:reverse(SlotNumbers),
        DisplayVal = lists:foldl(fun(Elem, Acc) ->
            Elem ++ ", " ++ Acc
        end, hd(RevSlots), tl(RevSlots)),
        io:format("~s~n", [DisplayVal]),
        SlotNumbers;
    true ->
        io:format("No ~p cars in parking lot currently~n", [Colour]),
        []
    end. 

-spec slot_number_for_registration_number([string()]) -> integer() | not_found.
slot_number_for_registration_number([RegNum]) ->
    SlotRes = find([id], [{registration, RegNum}]),
    if SlotRes =/= [] ->
        SlotNum = list_to_integer(atom_to_list(hd(SlotRes)) -- "spot"),
        io:format("~p~n", [SlotNum]),
        SlotNum;
    true ->
        io:format("Not found~n", []),
        not_found
    end.

find_idx(Key) ->
    if  
        Key == id -> '$1';
        Key == status -> '$2';
        Key == registration -> '$3';
        Key == colour -> '$4';
        true -> incorrect_input
    end.

find(Requested, Matches) ->
    MatchHead = {'$1', '$2', '$3', '$4'},
    Guards = lists:filtermap(fun({Key, Value}) ->
        Idx = find_idx(Key),
        if Idx == incorrect_input -> false;
        true ->
            {true, {'==', Idx, Value}}
        end
    end, Matches),
    Result = if length(Requested) == 1 ->
        find_idx(hd(Requested));
    true ->
        lists:map(fun(Elem) ->
            find_idx(Elem)
        end, Requested)
    end,
    lists:sort(ets:select(spots,[{MatchHead, Guards, [Result]}])).
