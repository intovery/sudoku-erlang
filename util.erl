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