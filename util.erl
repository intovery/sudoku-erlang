-module(util).
-compile(export_all).

qsort([]) ->
    [];
qsort([Pivot|L]) ->
    qsort([X || X <- L, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- L, X >= Pivot]).

range(S,E) ->
    case S == E of
        false -> [S|range(S+1,E)];
        true -> []
    end.

reduce(_,Acc,[]) -> Acc;
reduce(F,Acc,[H|T]) -> reduce(F,F(Acc,H),T).

replace(N,D,L) ->
    lists:sublist(L,N) ++ [D] ++ lists:nthtail(N+1,L).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

conditional(O) ->
    fun
        (true) -> maps:get(true,O);
        (false) -> maps:get(false,O)
    end.
conditional(T,F) ->
    conditional(#{true=>T,false=>F}).

print() ->
    receive
        {_,L} ->
            io:format("received => ~p~n",[L]),
            print();
        terminate ->
            ok
    end,
    io:format("process terminated.~n").

perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

% lists_to_map({A1,L1},{A2,L2}) when length(L1) == length(L2) -> 
%     Len = length(L1),
%     lists:map(fun(X) -> #{
%         A1=>lists:nth(X+1,L1),
%         A2=>lists:nth(X+1,L2)
%     } end,util:range(0,Len)).

lists_to_map(L_key,L_values) when length(L_key) == length(L_values) ->
    % R = lists:map(fun(X) -> #{H1=>X} end, H2),
    % lists:map(fun(X) -> maps:merge(X,lists_to_map))
    Len = length(L_key),
    lists:map(
        fun(N) ->
            lists:map(fun(V) -> #{lists:nth(N+1,L_key)=>V} end,lists:nth(N+1,L_values))
        end,util:range(0,Len)).

key_list_match({A,L}) ->
    lists:map(fun(X) -> #{A=>X} end,L).

base_range(Decimal,Base) ->
    base_range(Decimal,1,Base).

base_range(Decimal,Acc,Base) ->
    case Decimal < round(math:pow(Base,Acc)) of
        true -> Acc;
        false -> base_range(Decimal,Acc+1,Base)
    end.

div_with_range(D, N) ->
    lists:map(fun(X) -> (N div D) * X end,util:range(1,D+1)).

lift(L) ->
    lists:map(fun(X) -> [X] end, L).

accfunc(Cond) ->
    fun(D) ->
        accfunc(Cond,[D])
    end.
accfunc(Cond,Acc) ->
    case Cond(Acc) of
        true -> Acc;
        false -> fun(X) -> accfunc(Cond,[X|Acc])
        end
    end.
