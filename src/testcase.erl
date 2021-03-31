-module(testcase).
-export([exact_cover/0,twobytwo/0,sudoku/0]).

%% Finding exact cover
exact_cover() ->
    TC = 
        [[3,5,6],
        [1,4,7],
        [2,3,6],
        [1,4],
        [2,7],
        [4,5,7]],
    Cons = [fun(L) -> lists:any(fun(N) -> N == V end,L) end || V <- [1,2,3,4,5,6,7]],
    {TC,Cons}.

%% Find correct 2x2 sudoku board
twobytwo() ->
    TC = 
        [[{1,1,1}],
        [{1,1,2}],
        [{1,2,1}],
        [{1,2,2}],
        [{2,1,1}],
        [{2,1,2}],
        [{2,2,1}],
        [{2,2,2}]],
    Cons = cons_1() ++ cons_2() ++ cons_3(),
    {TC,Cons}.

cons_row_col(R,C) -> fun({Row,Col,_}) -> (Row == R) andalso (Col == C) end.
cons_row_val(R,V) -> fun({Row,_,Val}) -> (Row == R) andalso (Val == V) end.
cons_col_val(C,V) -> fun({_,Col,Val}) -> (Col == C) andalso (Val == V) end.

cons_1() -> [fun(L) -> lists:any(cons_row_col(R,C),L) end || R <- [1,2], C <- [1,2]].
cons_2() -> [fun(L) -> lists:any(cons_row_val(R,V),L) end || R <- [1,2], V <- [1,2]].
cons_3() -> [fun(L) -> lists:any(cons_col_val(C,V),L) end || C <- [1,2], V <- [1,2]].


%% Finding correct 9x9 sudoku with pre-written sudoku board
%% Add pre-written board(CSV) in "./data" folder
sudoku() -> sudoku("./data/data.csv").
sudoku(Name) ->
    Board = sudoku:read_board(Name),
    PossibleCases = lists:flatten(lists:map(fun(N) ->
            sudoku:possible_case_row(N,Board)
        end,lists:seq(1,9))),
    Cons = cons(row_col) ++ cons(row_val) ++ cons(col_val) ++ cons(box_val),
    {[Board|[[P] || P <- PossibleCases]],Cons}.

cons(row_col) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({Row,Col,_}) -> (Row == R) andalso (Col == C) end,L) end || R <- P, C <- P];

cons(row_val) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({Row,_,Val}) -> (Row == R) andalso (Val == V) end,L) end || R <- P, V <- P];

cons(col_val) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({_,Col,Val}) -> (Col == C) andalso (Val == V) end,L) end || C <- P, V <- P];

cons(box_val) ->
    P = lists:seq(1,9),
    IsOnBox = fun(N,Row,Col) ->
        Preset = sudoku:index_preset(box,N),
        lists:any(fun({R,C}) -> (R == Row) andalso (C == Col) end,Preset)
    end,
    [fun(L) ->
        lists:any(fun({Row,Col,Val}) -> IsOnBox(B,Row,Col) andalso (Val == V) end,L) end || B <- P, V <- P].
