%%%-------------------------------------------------------------------
%% @doc parking_lot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(parking_lot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 1},
    ChildSpecs = [
        {spot_sup, {spot_sup, start_link, []}, permanent, brutal_kill, supervisor, [spot_sup]},
        {sequence, {sequence, start_link, []}, permanent, brutal_kill, worker, [sequence]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
