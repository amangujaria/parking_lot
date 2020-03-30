-module(read).
-compile(export_all).

read_stdin() ->
    case io:get_line("") of
        {error, Error} ->
            init:stop();
        eof ->
            init:stop();
        Line ->
            io:format("Received: ~p~n", [Line]),
            Tokens = string:tokens(Line -- "\n", " "),
            read_stdin()
    end.
