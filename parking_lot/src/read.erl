-module(read).

-export([read_stdin/0, process/2]).

-spec read_stdin() -> ok.
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
                    [Elem] = Tail,
                    process(FuncStr, [list_to_integer(Elem)]);
                true ->
                    spawn(fun() -> process(FuncStr, Tail) end)
                end;
            true -> ok
            end,
            read_stdin()
    end.

-spec process(string(), list()) -> any().
process(FuncStr, Args) ->
    Func = list_to_atom(FuncStr),
    try erlang:apply(utils, Func, [Args]) of
        Res -> Res
    catch
        _:_ -> error
    end.
