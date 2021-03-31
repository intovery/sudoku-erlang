-module(knuth).
-export([knuth/1,knuth/2]).


knuth({Groups,Cons}) ->
    reset_knuth_ETS(ets:whereis(process_to_knuth_atom(self()))),
    F = fun(R) ->
            ets:insert(process_to_knuth_atom(self()),{result,R}) 
        end,
    knuth(F,{Groups,Cons}),
    lists:map(fun({result,L}) ->
        L
    end,ets:lookup(process_to_knuth_atom(self()),result)).

knuth(F,{Groups,Cons}) ->
    knuth(F,[],evaluate(Groups,Cons),Cons).

knuth(F,Acc,Evaluations,Cons) ->
    case is_cover(Acc,Cons) of
        true -> F([G || {G,_} <- Acc]);
        false ->
            % PC = PassedConstraint
            LeastPC = lists:nth(1,least_passed_constraints(Evaluations,Cons)),
            FilteredGroups = lists:filter(fun({_,Cs}) -> lists:member(LeastPC,Cs) end,Evaluations),
            lists:foreach(fun({Group,PCs}) ->
                NextEvaluations = lists:filter(fun({_,Cs}) -> 
                    not lists:any(fun(PC) -> lists:member(PC,Cs) end,PCs)
                end,Evaluations),
                NextCons = remove(PCs,Cons),
                knuth(F,[{Group,PCs} | Acc],NextEvaluations,NextCons)
            end,FilteredGroups)
    end.


process_to_knuth_atom(Pid) ->
    list_to_atom(pid_to_list(Pid) ++ "_knuth").


reset_knuth_ETS(undefined) ->
    ets:new(process_to_knuth_atom(self()),[bag,named_table]);

reset_knuth_ETS(_Ref) -> 
    ets:delete(process_to_knuth_atom(self())),
    ets:new(process_to_knuth_atom(self()),[bag,named_table]).


least_passed_constraints(Evaluations,Constraints) ->
    PassedConstraints = lists:flatten([Cs || {_,Cs} <- Evaluations]),
    Zip = lists:map(fun(C) ->
        {C,frequency(PassedConstraints,C)}
    end,Constraints),
    Min = lists:min([X || {_,X} <- Zip]),
    [C || {C,Freq} <- Zip, Freq == Min].


frequency(L,Target) -> length([E || E <- L, E == Target]).


remove(X,L) -> [Y || Y <- L, not lists:member(Y,X)].


is_cover(Acc,Cons) ->
    L = lists:flatten([C || {_,C} <- Acc]),
    lists:all(fun(C) -> lists:member(C,L) end,Cons).


evaluate(Gs,Cons) ->
    lists:map(fun(G) -> {G,lists:filter(fun(C) -> C(G) end,Cons)} end,Gs).
