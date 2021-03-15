-module(util).
-export([qsort/1,msort/1,chunks/2,half/1,perms/1]).

-spec qsort(list()) -> list().
qsort([]) -> [];
qsort([Pivot|L]) ->
    qsort([X || X <- L, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- L, X >= Pivot]).

msort([]) -> [];
msort([N]) -> [N];
msort([N1,N2]) ->
    merge([N1],[N2]);
msort(L) ->
    [L1,L2] = half(L),
    merge(msort(L1),msort(L2)).

merge([],L2) -> L2;
merge(L1,[]) -> L1;
merge([H1|T1],[H2|T2]) ->
    case H1 =< H2 of
        true -> [H1 | merge(T1,[H2|T2])];
        false -> [H2 | merge([H1|T1],T2)]
    end.

-spec replace(integer(),term(),list()) -> list().
replace(N,D,L) ->
    lists:sublist(L,N-1) ++ [D] ++ lists:nthtail(N,L).

-spec factorial(integer()) -> integer().
factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

-spec perms(list()) -> list().
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

%% code from "stack overflow"
%% https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang/15008490
-spec chunks(list(),integer()) -> list().
chunks(List,Len) ->
    LeaderLength = case length(List) rem Len of
        0 -> 0;
        N -> Len - N
    end,
    Leader = lists:duplicate(LeaderLength,undefined),
    chunks(Leader ++ lists:reverse(List),[],0,Len).

chunks([],Acc,_,_) -> Acc;
chunks([H|T],Acc,Pos,Max) when Pos==Max ->
    chunks(T,[[H] | Acc],1,Max);
chunks([H|T],[HAcc | TAcc],Pos,Max) ->
    chunks(T,[[H | HAcc] | TAcc],Pos+1,Max);
chunks([H|T],[],Pos,Max) ->
    chunks(T,[[H]],Pos+1,Max).

-spec fibonacci(integer()) -> integer().
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) when N > 1 ->
    fibonacci(N-1) + fibonacci(N-2).

half([]) -> [];
half(L) ->
    Len = length(L) div 2,
    [lists:sublist(L,Len),lists:sublist(L,Len+1,Len+1)].
