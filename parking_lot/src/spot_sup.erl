-module(spot_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1, start_spot/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = {simple_one_for_one, 1, 1},
    ChildSpecs = [{?MODULE, {spot_sup, start_spot, []}, permanent, brutal_kill, worker, dynamic}],
    ets:new(spots, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, {SupFlags, ChildSpecs}}.

start_child(Id) ->
    supervisor:start_child(?MODULE, [Id]).

start_spot(Id) ->
    spot:start_link(Id).
