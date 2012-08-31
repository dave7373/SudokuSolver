%% @author David Almroth
%%
%% This is a sudoku solving program.
%% It can handle sudoku puzzles that are 4 x 4 or 9 x 9 or 16 x 16
%% That is the same as N=4, N=9 or N=16. 
%% N is the size of the sudoku matrix.

-module(sudoku).

-export([solve/1, test4/0, test9/0]).
-include_lib("eunit/include/eunit.hrl").


%% These test functions are just a workaround for the eunit stacktrace bug in R15B01
%% http://stackoverflow.com/questions/10509238/not-output-exception-stack-trace-in-eunit
test4() ->
    InitList = [0,0,0,0,
		0,0,0,0,
		0,0,0,0,
		0,0,0,0],
    io:format("~p~n", [sudoku:solve(InitList)]),
    ok.

test9() ->
    InitList = [9,2,3,4,5,6,7,8,1,
		0,0,1,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,9,
		0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0,
		0,0,0,0,0,0,0,0,0],
    io:format("~p~n", [sudoku:solve(InitList)]),
    ok.


% This is a brute force sudoku solving algorithm
solve(InitList) ->
    Matrix = matrix:new(InitList),
    %%Start job with position = 0 and value = 1
    Solution = internal_solve_position(Matrix, 0, 1, matrix:get_n(Matrix)),
    case Solution of 
	{fail, FailMatrix} -> {failure, matrix:to_list(FailMatrix)};
	{solution, SolutionMatrix} -> {solution, matrix:to_list(SolutionMatrix)}
    end.
	    
%% Operates on one position in the sudoku matrix
internal_solve_position(Matrix, Position, _TestValue, N) when Position >= N*N -> 
    {solution, Matrix};
internal_solve_position(Matrix, Position, TestValue, N) ->
    %%io:format("testing pos ~p~n", [Position]),
    case matrix:has_initial_value(Position, Matrix) of
	true -> 
	    internal_solve_position(Matrix, Position+1, 1, N);
	false -> 
	    internal_solve_value_sequential(Matrix, Position, TestValue, N)		    
    end.

%% Will try values from 1 to N for a specified position to see if a 
%% solution can be found.
internal_solve_value_sequential(Matrix, _Position, Value, N) when Value > N -> 
    {fail, Matrix};
internal_solve_value_sequential(Matrix, Position, TestValue, N) ->
    %%io:format("testing pos ~p=~p~n", [Position, TestValue]),
    Result = case matrix:set(Position, TestValue, Matrix) of
		 {not_allowed, _Pos, _Val} ->
		     internal_solve_value_sequential(Matrix, Position, TestValue+1, N);
		 UpdatedMatrix ->
		     internal_solve_position(UpdatedMatrix, Position+1, 1, N)
	     end,
    case Result of
	{fail, _Ignore} ->
	    internal_solve_value_sequential(Matrix, Position, TestValue+2, N);
	{solution, SolvedMatrix} ->
	    {solution, SolvedMatrix}
    end.


%% internal_solve_value_parallel(Matrix, _Position, Value, N) when Value > N -> 
%%     {fail, Matrix};
%% internal_solve_value_parallel(Matrix, Position, TestValue, N) ->
%%     %%io:format("testing pos ~p=~p~n", [Position, TestValue]),
%%     Result = case matrix:set(Position, TestValue, Matrix) of
%% 		 {not_allowed, _Pos, _Val} ->
%% 		     internal_solve_value_sequential(Matrix, Position, TestValue+1, N);
%% 		 UpdatedMatrix ->
%% 		     internal_solve_position(UpdatedMatrix, Position+1, 1, N)
%% 	     end,
%%     case Result of
%% 	{fail, _Ignore} ->
%% 	    internal_solve_value_sequential(Matrix, Position, TestValue+2, N);
%% 	{solution, SolvedMatrix} ->
%% 	    {solution, SolvedMatrix}
%%     end.







