-module(main).

-export([start/1]).

-spec start(string()) -> ok.
start(FileName) ->
    application:ensure_all_started(parking_lot),
    if FileName =/= [''] ->
        spawn(fun() -> fread:start(FileName) end);
    true -> ok
    end,
    read:read_stdin().
