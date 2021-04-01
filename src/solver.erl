-module(solver).
-export([solve/1,solve/2,constraints/0,constraints/1,possible_case/1]).

solve(Name) ->
    Board = sudoku:read_board(Name),
    PossibleCases = possible_case(Board),
    knuth:knuth({[Board | [[X] || X <- PossibleCases]],constraints()}).

solve(F,Name) ->
    Board = sudoku:read_board(Name),
    PossibleCases = possible_case(Board),
    knuth:knuth(F,{[Board | [[X] || X <- PossibleCases]],constraints()}).

constraints() ->
    lists:foldl(fun(Method,Acc) ->
        Acc ++ constraints(Method)
    end,[],[row_col,row_val,col_val,box_val]).

constraints(row_col) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({{Row,Col},_}) -> (Row == R) andalso (Col == C) end,L) end || R <- P, C <- P];

constraints(row_val) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({{Row,_},Val}) -> (Row == R) andalso (Val == V) end,L) end || R <- P, V <- P];

constraints(col_val) ->
    P = lists:seq(1,9),
    [fun(L) -> lists:any(fun({{_,Col},Val}) -> (Col == C) andalso (Val == V) end,L) end || C <- P, V <- P];

constraints(box_val) ->
    P = lists:seq(1,9),
    IsOnBox = fun(N,Row,Col) ->
        Preset = sudoku:index_preset(box,N),
        lists:any(fun({R,C}) -> (R == Row) andalso (C == Col) end,Preset)
    end,
    [fun(L) ->
        lists:any(fun({{Row,Col},Val}) -> IsOnBox(B,Row,Col) andalso (Val == V) end,L) end || B <- P, V <- P].


possible_case(Board) ->
    lists:flatten(lists:map(fun(N) ->
        possible_case_row(N,Board)
    end,lists:seq(1,9))).

possible_case_row(N,Board) ->
    Row = sudoku:extract_row(N,Board),
    [{{N,C},V} || C <- missing_column(Row), V <- missing_value(Row)].
possible_case_column(N,Board) ->
    Column = sudoku:extract_column(N,Board),
    [{{R,N},V} || R <- missing_row(Column), V <- missing_value(Column)].


missing_row(L) ->
    lists:foldl(fun({{R,_},_},Acc) -> lists:delete(R,Acc) end,lists:seq(1,9),L).
missing_column(L) ->
    lists:foldl(fun({{_,C},_},Acc) -> lists:delete(C,Acc) end,lists:seq(1,9),L).
missing_value(L) ->
    lists:foldl(fun({_,V},Acc) -> lists:delete(V,Acc) end,lists:seq(1,9),L).
