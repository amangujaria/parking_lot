-module(fread).

-export([start/1]).

-spec start(string()) -> ok.
start(FileName) ->
    {ok, File} = file:open(FileName, [read]),
    read(File).

read(File) ->
    Txt = file:read_line(File),
    case Txt of
        {ok, "\n"} -> read(File);
        {ok, Line} ->
            [FuncStr | Tail] = string:tokens(Line -- "\n", " "),
            if FuncStr == "create_parking_lot" andalso length(Tail) == 1 ->
                [Elem] = Tail,
                read:process(FuncStr, [list_to_integer(Elem)]);
            FuncStr == "create_parking_lot" ->
                ok;
            true ->
                spawn(fun() -> read:process(FuncStr, Tail) end)
            end,
            read(File);
        _ -> ok
    end.
