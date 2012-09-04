%% @author David Almroth
-module(matrix_tests).

-include_lib("eunit/include/eunit.hrl").

get_set_test() ->
    Matrix = matrix:empty(9),

    Matrix2 = matrix:set(0,1, Matrix),
    Result = matrix:get(0, Matrix2),
    ?assertEqual(1, Result),

    Matrix3 = matrix:set(80,9, Matrix2),
    Result2 = matrix:get(80, Matrix3),
    ?assertEqual(9, Result2),

    %%Box test
    ?assertMatch(not_allowed, matrix:set(0,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(1,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(2,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(9,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(10,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(11,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(18,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(19,1, Matrix2)),
    ?assertMatch(not_allowed, matrix:set(20,1, Matrix2)),

    %%Row test
    ?assertMatch(not_allowed, matrix:set(8,1, Matrix2)),

    %%Col test
    ?assertMatch(not_allowed, matrix:set(27,1, Matrix2)),
    ok.

init_list_4_test() ->
    InitList =  [1,2,3,4,
		 0,0,0,0,
		 0,0,0,0,
		 0,0,0,0],
    Matrix = matrix:new(InitList),

    %%Set one value
    Matrix2 = matrix:set(4,3, Matrix),
    Result = matrix:get(4, Matrix2),
    ?assertEqual(3, Result),

    %%Make sure the matrix can be transformed back to the same list
    ResultList = matrix:to_list(Matrix),
    %%?debugVal(ResultList),
    ?assertEqual(InitList, ResultList),
    ok.

