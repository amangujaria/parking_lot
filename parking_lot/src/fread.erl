-module(fread).

-export([start/1]).

start(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    read(File).

read(File) ->
    Txt = file:read_line(File),
    case Txt of
        {ok, "\n"} -> read(File);
        {ok, Line} ->
            [FuncStr | Tail] = string:tokens(Line -- "\n", " "),
            if FuncStr == "create_parking_lot" ->
                read:process(FuncStr, Tail);
            true ->
                spawn(fun() -> read:process(FuncStr, Tail) end)
            end,
            read(File);
        _ -> ok
    end.
