-module(fread_tests).

-include_lib("eunit/include/eunit.hrl").

file_read_test_() ->
    [?_assertEqual(ok, fread:start("../bin/testfile"))].
