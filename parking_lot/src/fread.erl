-module(fread).

-export([start/1]).

start(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    read(File).

read(File) ->
    Txt = file:read(File,1024 * 1024),
    if Txt =/= eof ->
        {ok, Data} = Txt,
        Lines = string:tokens(Data, "\n"),
        lists:map(fun(Line) ->
            io:format("Line: ~p~n", [Line]),
            Line
        end, Lines),
        read(File);
    true -> ok
    end.
