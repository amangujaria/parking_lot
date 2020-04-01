-module(sequence).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {vacant = [] :: list()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({create_lot, Capacity}, From, #state{vacant = Spots}) ->
    NewSpots = lists:map(fun(Id) -> list_to_atom("spot" ++ integer_to_list(Id)) end, lists:seq(length(Spots) + 1, length(Spots) + Capacity)),
    {reply, created, #state{vacant = Spots ++ NewSpots}};

handle_call(find_empty, From, #state{vacant = []} = State) ->
    {reply, not_available, State};

handle_call(find_empty, From, #state{vacant = [Spot | Tail]} = State) ->
    {reply, Spot, State#state{vacant = Tail}};

handle_call({return, Spot}, From, #state{vacant = Vacant} = State) ->
    {reply, returned, State#state{vacant = lists:usort([Spot | Vacant])}};

handle_call(Msg, From, State) ->
    {reply, ok, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    {noreply, State}.
