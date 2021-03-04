-module(si_get).
-export([get/2,get_fun/1]).


%% extraction functions
-spec get(list(), tuple()) -> list().
get(L,{row,N}) -> get_row(N,L);
get(L,{column,N}) -> get_column(N,L);
get(L,{square,N}) -> get_square(N,L).

-spec get_fun(atom()) -> function().
get_fun(Method) ->
    fun(N,L) -> get(L,{Method,N}) end.

%% horizontal
-spec get_row(integer()) -> list().
get_row(Index) when Index > 0 ->
    lists:map(fun(N) -> N + ((Index-1)*9) end,lists:seq(1,9)).

-spec get_row(integer(), list()) -> list().
get_row(Index,L) when Index > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_row(Index)).

%% vertical
-spec get_column(integer()) -> list().
get_column(Index) when Index > 0 ->
    lists:map(fun(N) -> Index + ((N-1)*9) end,lists:seq(1,9)).

-spec get_column(integer(), list()) -> list().
get_column(Index,L) when Index > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_column(Index)).

%% 3*3 square
square_start(N) when N > 0 ->
    (((N-1) rem 3) * 3) + (((N-1) div 3) * 27).
square_cell(N) when N > 0 ->
    (((N-1) rem 3) + 1) + (((N-1) div 3) * 9).
get_square(Index) when Index > 0 ->
    lists:map(fun(N) -> square_start(Index) + square_cell(N) end,lists:seq(1,9)).
get_square(Index,L) when Index > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_square(Index)).