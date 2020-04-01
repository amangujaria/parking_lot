-module(read).
-compile(export_all).

read_stdin() ->
    case io:get_line("") of
        {error, _Error} ->
            init:stop();
        eof ->
            init:stop();
        Line ->
            Tokens = string:tokens(Line -- "\n", " "),
            if Tokens =/= [] ->
                [FuncStr | Tail] = string:tokens(Line -- "\n", " "),
                if FuncStr == "create_parking_lot" ->
                    process(FuncStr, Tail);
                true ->
                    spawn(fun() -> process(FuncStr, Tail) end)
                end;
            true -> ok
            end,
            read_stdin()
    end.

process(FuncStr, Args) ->
    Func = list_to_atom(FuncStr),
    try erlang:apply(utils, Func, [Args]) of
        Res -> Res
    catch
        _:_ -> error
    end.
