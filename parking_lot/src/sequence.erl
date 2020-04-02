-module(sequence).

-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {vacant = [] :: list()}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

handle_call({create_lot, Capacity}, _From, #state{vacant = Spots}) when is_integer(Capacity) andalso Capacity > 0 ->
    NewSpots = lists:map(fun(Id) -> list_to_atom("spot" ++ integer_to_list(Id)) end, lists:seq(length(Spots) + 1, length(Spots) + Capacity)),
    {reply, created, #state{vacant = Spots ++ NewSpots}};

handle_call(find_empty, _From, #state{vacant = []} = State) ->
    {reply, not_available, State};

handle_call(find_empty, _From, #state{vacant = [Spot | Tail]} = State) ->
    {reply, Spot, State#state{vacant = Tail}};

handle_call({return, Spot}, _From, #state{vacant = Vacant} = State) ->
    {reply, returned, State#state{vacant = lists:usort([Spot | Vacant])}};

handle_call(_Msg, _From, State) ->
    {reply, erroneous_msg, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.
