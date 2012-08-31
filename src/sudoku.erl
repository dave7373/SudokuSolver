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
    io:format("~p~n", [sudoku:solve(InitList)]).

% This is a brute force sudoku solving algorithm
solve(InitList) ->
    Matrix = matrix:new(InitList),
    %% Starts the job with position = 0 trying value = 1
    Solution = internal_solve_position(Matrix, 0, 1, matrix:get_n(Matrix)),
    case Solution of 
	{fail, FailMatrix} -> {fail, matrix:to_list(FailMatrix)};
	{solution, SolutionMatrix} -> {solution, matrix:to_list(SolutionMatrix)}
    end.
	    
%% Operates on one position in the sudoku matrix
internal_solve_position(Matrix, Position, _TryValue, N) when Position >= N*N -> 
    {solution, Matrix};
internal_solve_position(Matrix, Position, TryValue, N) -> 
    case matrix:has_initial_value(Position, Matrix) of
	true -> internal_solve_position(Matrix, Position+1, 1, N);
	false -> internal_solve_value_sequential(Matrix, Position, TryValue, N)		    
    end.  

%% Will try values from 1 to N for a specified position to see if a 
%% solution can be found.
internal_solve_value_sequential(Matrix, _Position, Value, N) when Value > N -> 
    {fail, Matrix};
internal_solve_value_sequential(Matrix, Position, TryValue, N) ->
    debug_work_progress(false, Matrix, Position, TryValue),
    %%TODO remove nested case
    case matrix:set(Position, TryValue, Matrix) of
	{not_allowed, _Pos, _Val} ->
	    internal_solve_value_sequential(Matrix, Position, TryValue+1, N);
	UpdatedMatrix ->
	    case internal_solve_position(UpdatedMatrix, Position+1, 1, N) of
		{fail, _FailedMatrix} -> 
		    internal_solve_value_sequential(Matrix, Position, TryValue+1, N);
		{solution, SolutionMatrix} -> 
		    {solution, SolutionMatrix} 
	    end
    end.

debug_work_progress(false, _Matrix, _Position, _TryValue) ->
    nop;
debug_work_progress(true, Matrix, Position, TryValue) ->
    {L1, _L2} = lists:split(Position,  matrix:to_list(Matrix)),
    io:format("Working: ~p TryValue=~p~n", [L1, TryValue]).
    








