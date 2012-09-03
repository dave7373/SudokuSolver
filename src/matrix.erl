%% @author David Almroth
%% This is the ADT Matrix that can be used to solve sudoku problems.
%%
%% The matrix has rows, columns and boxes.
%% The key to understanding the sudoku matrix is this: 
%% Every row, column and box can have only one occurance 
%% of the values 1,2,3,4,5,6,7,8,9.
%% 
%% Rows, columns and boxes are numbered from 0 to N.
%% N is the size of the matrix.
%% Positions for a specific number are numbered from 0 to N*N-1.
%%
%% A normal 9 x 9 matrix then has 9 rows (0,1,2..8) and 9 columns (0,1,2..8) 
%% and 9 boxes (0,1,2...8). It also has 81 numbers with position (0,1,2..80)

-module(matrix).

-export([new/1, to_list/1, set/3, get/2, get_n/1, has_initial_value/2, empty/1]).
-include_lib("eunit/include/eunit.hrl").


new(InitList) when is_list(InitList) ->
    Content = lists:foldl(
		fun(X, Acc) -> dict:store(X-1, lists:nth(X, InitList), Acc) end, 
		dict:new(), 
		lists:seq(1,length(InitList))),
    Matrix = new(Content, trunc(math:sqrt(length(InitList)))),
    lists:foldl(
      fun(X, Acc) -> 
	      case lists:nth(X, InitList) of 
		  0 -> Acc;
		  _NotZero -> set(X-1, lists:nth(X, InitList), Acc) 
	      end
      end,
      Matrix, 
      lists:seq(1,length(InitList))).
    
new(InitDict, N) ->
    ?assert((N == 4) or (N == 9) or (N == 16)),
    ?assertEqual(N*N, dict:size(InitDict)),
    Rows = get_dict_with_n_sets(N),
    Cols = get_dict_with_n_sets(N),
    Boxes = get_dict_with_n_sets(N),
    {Rows, Cols, Boxes, InitDict, N}.	


to_list(Matrix) ->
    {_Rows, _Cols, _Boxes, Content, _N} = Matrix,
    lists:foldl(fun(X, Acc) -> [dict:fetch(dict:size(Content) - X, Content) | Acc] end, 
		[], lists:seq(1, dict:size(Content))).

%% Returns a dictionary with N sets. The keys are numbered 0,1,..(N-1)	
get_dict_with_n_sets(N) ->
    lists:foldl(fun(X, Acc) -> dict:store(X, [], Acc) end, dict:new(), lists:seq(0,N-1)).

%% Returns  ModifiedMatrix or not_allowed if value is not allowed.
set(Pos, Value, Matrix) ->
    {Rows, Cols, Boxes, Content, N} = Matrix,
    Row = dict:fetch(row(Pos, N), Rows),
    Col = dict:fetch(col(Pos, N), Cols),
    Box = dict:fetch(box(Pos, N), Boxes),
    case {lists:member(Value, Row), lists:member(Value, Col), lists:member(Value, Box)}  of 
	{false, false, false} -> 
	    %%io:format("setting pos-~p=~p~n", [Pos, Value]),
	    NewRows = dict:store(row(Pos, N), [Value | Row], Rows),
	    NewCols = dict:store(col(Pos, N), [Value | Col], Cols),
	    NewBoxes = dict:store(box(Pos, N), [Value | Box], Boxes),
	    NewContent = dict:store(Pos, Value, Content),
	    {NewRows, NewCols, NewBoxes, NewContent, N};
	_Other ->
	    {not_allowed, Pos, Value}
    end.

get(Pos, Matrix) ->
    {_Rows, _Cols, _Boxes, Content, _N} = Matrix,
    {ok, Value} = dict:find(Pos, Content),
    Value.

%%The N-value for a 9 x 9 matrix is 9
get_n(Matrix) ->
    {_Rows, _Cols, _Boxes, _Content, N} = Matrix,
    N.

has_initial_value(Pos, Matrix)	->
    0 /= get(Pos, Matrix).


row(X, N) ->
    X div N.

col(X, N) ->
    X rem N.

box(X, N) ->
    (row(X,N) div sqrt(N) * sqrt(N)) + col(X,N) div sqrt(N).

sqrt(N) ->
    case N of
	4 -> 2;
	9 -> 3;
	16 -> 4
    end.

empty(N) when is_integer(N) ->
    Content = lists:foldl(fun(X, Acc) -> dict:store(X, 0, Acc) end, 
			  dict:new(), lists:seq(0,N*N-1)),
    new(Content, N).

