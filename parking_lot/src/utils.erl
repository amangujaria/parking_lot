-module(utils).

-export([create_parking_lot/1, park/1, leave/1, status/1, registration_numbers_for_cars_with_colour/1, slot_numbers_for_cars_with_colour/1, slot_number_for_registration_number/1, find/2]).


-spec create_parking_lot([any()]) -> created | not_allowed.
create_parking_lot([Num]) when is_integer(Num) andalso Num > 0 ->
    Existing = length(ets:tab2list(spots)),
    lists:map(fun(Elem) ->
        SpotId = list_to_atom("spot" ++ integer_to_list(Elem)),
        spot_sup:start_child(SpotId)
    end, lists:seq(Existing + 1, Existing + Num)),
    created = gen_server:call(sequence, {create_lot, Num}),
    io:format("Created a parking lot with ~p slots~n", [Num]),
    created;

create_parking_lot([_Value]) ->
    not_allowed.

-spec park(list(string())) -> allowed | not_allowed.
park([RegNum, Colour]) ->
    Spot = gen_server:call(sequence, find_empty),        
    if Spot =/= not_available ->
        permitted = spot:generate_event(Spot, {RegNum, Colour}),
        io:format("Allocated slot number: ~p~n", [list_to_integer(atom_to_list(Spot) -- "spot")]),
        allowed;
    true ->
        io:format("Sorry, parking lot is full~n", []),
        not_allowed
    end.

-spec leave([string()]) -> success | failure | impossible.
leave([Id]) ->
    Spot = list_to_atom("spot" ++ Id),
    ExistsRes = ets:lookup(spots, Spot),
    if ExistsRes =/= [] ->
        case spot:generate_event(Spot, vacate) of
            permitted ->
	        returned = gen_server:call(sequence, {return ,Spot}),
	        io:format("Slot number ~p is free~n", [list_to_integer(Id)]),
                success;
            already_vacant -> io:format("Slot number ~p is already free~n", [list_to_integer(Id)]),
                failure
        end;
    true -> io:format("no such slot exists in the lot ~n", []),
        impossible
    end.

-spec status([]) -> ok.
status([]) ->
    State = utils:find([id, registration, colour], [{status, occupied}]),
    StateMod = lists:map(fun([Id, Reg, Colour]) -> 
        IdMod = list_to_integer(atom_to_list(Id) -- "spot"),
        [IdMod, Reg, Colour] 
    end, State),
    io:format("Slot No.\tRegistration No.\tColour~n", []),
    lists:foreach(fun([SpotId, Registration, Colour]) ->
        io:format("~p\t\t~s\t\t~s~n", [SpotId, Registration, Colour])
    end, lists:usort(StateMod)).

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

-spec slot_numbers_for_cars_with_colour([string()]) -> list(integer()).
slot_numbers_for_cars_with_colour([Colour]) ->
    Slots = find([id], [{colour, Colour}]),
    if Slots =/= [] ->
        SlotNumbers = lists:usort(lists:map(fun(Slot) -> list_to_integer(atom_to_list(Slot) -- "spot") end, Slots)),
        RevSlots = lists:reverse(SlotNumbers),
        DisplayVal = lists:foldl(fun(Elem, Acc) ->
            integer_to_list(Elem) ++ ", " ++ Acc
        end, integer_to_list(hd(RevSlots)), tl(RevSlots)),
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

find_idx(Key, Map) ->
    Idx = maps:get(Key, Map),
    list_to_atom("$" ++ Idx).

-spec find(list(atom()), list(tuple())) -> list().
find(Requested, Matches) ->
    Map = maps:from_list([{id, "1"}, {status, "2"}, {registration, "3"}, {colour, "4"}]),
    ReqMod = lists:filter(fun(Elem) -> maps:is_key(Elem, Map) end, Requested),
    MatchesMod = lists:filter(fun({Key, _}) -> maps:is_key(Key, Map) end, Matches),
    if length(Requested) == length(ReqMod) andalso length(Matches) == length(MatchesMod) ->
        MatchHead = {'$1', '$2', '$3', '$4'},
        Guards = lists:filtermap(fun({Key, Value}) ->
            Idx = find_idx(Key, Map),
%%            if Idx == incorrect_input -> false;
%%            true ->
                {true, {'==', Idx, Value}}
%%            end
        end, Matches),
        Result = if length(Requested) == 1 ->
            find_idx(hd(Requested), Map);
        true ->
            lists:map(fun(Elem) ->
                find_idx(Elem, Map)
            end, Requested)
        end,
        lists:sort(ets:select(spots,[{MatchHead, Guards, [Result]}]));
    true -> []
    end.
