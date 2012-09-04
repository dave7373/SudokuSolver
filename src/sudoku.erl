%% @author David Almroth
%%
%% This is a sudoku solving program.
%% It can handle sudoku puzzles that are 4 x 4 or 9 x 9 or 16 x 16
%% That is the same as N=4, N=9 or N=16. 
%% N is the size of the sudoku matrix.

-module(sudoku).

-export([solve/1, test4/0, test9_hard/0, test9_easy/0]).
-include_lib("eunit/include/eunit.hrl").


%% These test functions are just a workaround for the eunit stacktrace bug in R15B01
%% http://stackoverflow.com/questions/10509238/not-output-exception-stack-trace-in-eunit
test4() ->
    test([0,0,0,0,
	 0,0,0,0,
	 0,0,0,0,
	 0,0,0,0]).

test9_hard() ->
    test([0,0,0,0,8,0,0,0,0,
	  0,0,0,0,0,0,8,7,1,
	  0,0,0,4,0,3,0,0,0,
	  3,0,0,0,6,0,0,0,0,
	  0,0,0,0,9,0,0,4,8,
	  0,5,0,0,1,0,0,6,2,
	  0,7,9,0,0,0,0,0,0,
	  0,0,0,0,0,0,0,1,4,
	  0,6,8,9,3,0,0,0,0]).
 
test9_easy() ->
    test([0,7,0,0,0,9,6,2,5,
	  0,6,0,0,8,2,0,0,0,
	  0,0,1,0,0,0,8,0,9,
	  0,0,8,0,7,0,9,0,0,
	  0,2,9,0,0,0,7,6,8,
	  0,0,6,0,9,3,0,5,4,
	  0,8,0,9,0,0,1,7,0,
	  1,4,7,6,0,0,0,9,0,
	  5,0,3,1,0,0,0,8,6]).
	 
test(InitList) ->
    %%fprof:trace(start),    
    io:format("~p~n", [sudoku:solve(InitList)]),
    %%fprof:trace(stop),
    ok.

% This is a sequential brute force sudoku solving algorithm
solve(InitList) ->
    Matrix = matrix:new(InitList),
    %% Starts the job with position = 0 trying value = 1
    Solution = internal_solve(Matrix, 0, 1, matrix:get_n(Matrix)),
    case Solution of 
	{fail, FailMatrix} -> {fail, matrix:to_list(FailMatrix)};
	{solution, SolutionMatrix} -> {solution, matrix:to_list(SolutionMatrix)}
    end.
	    
%% Operates on one position in the sudoku matrix
internal_solve(Matrix, Position, _TryValue, N) when Position >= N*N -> 
    {solution, Matrix};
internal_solve(Matrix, _Position, Value, N) when Value > N -> 
    {fail, Matrix};
internal_solve(Matrix, Position, TryValue, N) ->
    debug_work_progress(false, Matrix, Position, TryValue),
    case matrix:has_initial_value(Position, Matrix) of
	true -> internal_solve(Matrix, Position+1, 1, N);
	false -> try_value(Matrix, Position, TryValue, N)
    end.

try_value(Matrix, Position, TryValue, N) ->
    ResultTryValue = 
	case matrix:set(Position, TryValue, Matrix) of
	    not_allowed ->
		{fail, Matrix};		
	    UpdatedMatrix ->
		internal_solve(UpdatedMatrix, Position+1, 1, N)
	end,
    case ResultTryValue of 
	{fail, _Ignore} -> internal_solve(Matrix, Position, TryValue+1, N);    
	{solution, SolutionMatrix} -> {solution, SolutionMatrix} 
    end.

%% TODO: make debugging a preprocessor ifdef
debug_work_progress(false, _Matrix, _Position, _TryValue) ->
    nop;
debug_work_progress(true, Matrix, Position, TryValue) ->
    {L1, _L2} = lists:split(Position,  matrix:to_list(Matrix)),
    io:format("Working: ~p TryValue=~p~n", [L1, TryValue]).
    








