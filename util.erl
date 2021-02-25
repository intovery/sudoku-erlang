-module(util).
-compile(export_all).

qsort([]) ->
    [];
qsort([Pivot|L]) ->
    qsort([X || X <- L, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- L, X >= Pivot]).

range(S,E) when S == E -> [];
range(S,E) -> [S|range(S+1,E)].

reduce(_,Acc,[]) -> Acc;
reduce(F,Acc,[H|T]) -> reduce(F,F(Acc,H),T).

replace(N,D,L) ->
    lists:sublist(L,N-1) ++ [D] ++ lists:nthtail(N,L).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

lists_to_map(L_key,L_values) when length(L_key) == length(L_values) ->
    Len = length(L_key),
    lists:map(
        fun(N) ->
            lists:map(fun(V) -> #{lists:nth(N+1,L_key)=>V} end,lists:nth(N+1,L_values))
        end,util:range(0,Len)).

key_list_match({A,L}) ->
    lists:map(fun(X) -> #{A=>X} end,L).

div_with_range(D, N) ->
    lists:map(fun(X) -> (N div D) * X end,util:range(1,D+1)).

lift(L) -> [[X] || X <- L].

accfunc(Cond) ->
    fun(D) -> accfunc(Cond,[D]) end.
accfunc(Cond,Acc) ->
    case Cond(Acc) of
        true -> Acc;
        false -> fun(X) -> accfunc(Cond,[X|Acc]) end
    end.

%% code from "stack overflow"
%% https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang/15008490
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

current_time() ->
    {H,M,S} = time(),
    timer:hms(H,M,S).

measure_time(F) ->
    Old = current_time(),
    R = F(),
    New = current_time(),
    {(New - Old),R}.

divide_conquer(Subject,F_divide,F_execute,F_combine) ->
    L = F_divide(Subject),
    io:format("LENGTH => ~p~n",[length(L)]),
    F_combine(lists:map(fun(X) -> F_execute(X) end,L)).
