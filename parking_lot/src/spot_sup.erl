-module(spot_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, start_child/1, start_spot/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {{simple_one_for_one, non_neg_integer(), pos_integer()}, list({atom(), {atom(), atom(), [any()]}, permanent, brutal_kill, supervisor | worker, dynamic | [atom()]})}}.
init([]) ->
    SupFlags = {simple_one_for_one, 1, 1},
    ChildSpecs = [{?MODULE, {spot_sup, start_spot, []}, permanent, brutal_kill, worker, dynamic}],
    ets:new(spots, [set, named_table, public, {read_concurrency, true}, {write_concurrency, true}]),
    {ok, {SupFlags, ChildSpecs}}.

-spec start_child(atom()) -> {ok, pid()}.
start_child(Id) ->
    supervisor:start_child(?MODULE, [Id]).

-spec start_spot(atom()) -> {ok, pid()}.
start_spot(Id) ->
    spot:start_link(Id).
