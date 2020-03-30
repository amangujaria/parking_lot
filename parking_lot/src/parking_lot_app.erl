%%%-------------------------------------------------------------------
%% @doc parking_lot public API
%% @end
%%%-------------------------------------------------------------------

-module(parking_lot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    parking_lot_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
