-module(si_get).
-export([get/2,get_fun/1]).


%% extraction functions
get(L,{row,N}) -> get_row(N,L);
get(L,{column,N}) -> get_column(N,L);
get(L,{square,N}) -> get_square(N,L).

get_fun(Method) ->
    fun(N,L) -> get(L,{Method,N}) end.

%% horizontal
get_row(I) when I > 0 ->
    lists:map(fun(N) -> N + ((I-1)*9) end,lists:seq(1,9)).
get_row(I,L) when I > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_row(I)).
%% vertical
get_column(I) when I > 0 ->
    lists:map(fun(N) -> I + ((N-1)*9) end,lists:seq(1,9)).
get_column(I,L) when I > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_column(I)).
%% 3*3 square
square_start(N) when N > 0 ->
    (((N-1) rem 3) * 3) + (((N-1) div 3) * 27).
square_cell(N) when N > 0 ->
    (((N-1) rem 3) + 1) + (((N-1) div 3) * 9).
get_square(I) when I > 0 ->
    lists:map(fun(N) -> square_start(I) + square_cell(N) end,lists:seq(1,9)).
get_square(I,L) when I > 0 ->
    lists:map(fun(N) -> lists:nth(N,L) end,get_square(I)).