-module(erl_make).
-export([make/1, test/1]).

make(Mode) ->
    %% make:all will look at the Emakefile to determine what to compile
    case make:all([{d, Mode}]) of
        error ->
			halt("Build FAILED"),
			error;
        _ ->
			io:format("Build SUCCESSFUL~n")
    end.

test(Suite) ->
    io:format("Running suite ~p ~n", [Suite]),
    ct:run_test([{suite,Suite}, {logdir,"ebin"}]).
