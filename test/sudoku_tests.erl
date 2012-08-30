-module(sudoku_tests).

-include_lib("eunit/include/eunit.hrl").

solve_sudoku_1_test() ->
    InitList =  [1,2,3,4,
		 0,0,0,0,
		 0,0,0,0,
		 0,0,0,0],
    Solution1 = [1,2,3,4,
		 3,4,1,2,
		 2,1,4,3,
		 4,3,2,1],
    %%?debugVal(sudoku:solve(InitList)),
    Result = sudoku:solve(InitList),
    ?assertEqual(Solution1, Result),
    ok.

solve_sudoku_2_test() ->
    InitList =  [1,0,0,0,
		 0,2,0,0,
		 0,0,1,0,
		 0,0,0,2],
    Solution1 = [1,3,2,4,
		 4,2,3,1,
		 2,4,1,3,
		 3,1,4,2],
    %%?debugVal(sudoku:solve(InitList)),
    Result = sudoku:solve(InitList),
    ?assertEqual(Solution1, Result),
    ok.
