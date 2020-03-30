-module(spot).

-behaviour(gen_fsm).

-type event() :: {string(), string()} | atom(). %%arrival/departure events
-type spot_id() :: atom(). %% FSMRef for the parking spot

-export([start_link/1, init/1, occupied/3, vacant/3, generate_event/2]).

-record(state, {id :: spot_id(),
                registration :: string(),
                colour :: string(),
                status :: atom()}).

start_link(Id) ->
    gen_fsm:start_link({local, Id}, ?MODULE, [Id], []).

init([Id]) ->
    ets:insert(spots, {Id, vacant, "", ""}),
    {ok, vacant, #state{id = Id, status = vacant}}.

vacant({RegNum, Colour}, _From, #state{id = Id}) ->
    NewState = #state{id = Id, registration = RegNum, colour = Colour, status = occupied},
    ets:insert(spots, {Id, occupied, RegNum, Colour}),
    {reply, permitted, occupied, NewState};

vacant(vacate, _From, State) ->
    {reply, already_vacant, vacant, State};

vacant(_Event, _From, State) ->
    {reply, incomprehensible, vacant, State}.

occupied(vacate, _From, #state{id = Id}) ->
    NewState = #state{id = Id, status = vacant},
    ets:insert(spots, {Id, vacant, "", ""}),
    {reply, permitted, vacant, NewState};

occupied({_Num, _Colour}, _From, State) ->
    {reply, denied, occupied, State};

occupied(_Event, _From, State) ->
    {reply, incomprehensible, occupied, State}.

-spec generate_event(spot_id(), event()) -> atom().
generate_event(Id, Event) ->
    try gen_fsm:sync_send_event(Id, Event) of
        Reply -> Reply
    catch
        _:_ -> noproc
    end.
