-module(knuth).
-export([knuth/1]).

-export_type([
    group/0,
    constraint/0,
    exact_cover_request/0,
    treatment_fun/0,
    evaluation/0]).

-type group() :: list().
-type constraint() :: fun((list()) -> boolean()).

-type exact_cover_request() :: {list(group()),list(constraint())}.

-type treatment_fun() :: fun((list(group())) -> term()).

-type evaluation() :: {group(),list(constraint())}.


-spec knuth(exact_cover_request()) -> ok.
knuth({Groups,Cons}) ->
    knuth([],evaluate(Groups,Cons),Cons).

-spec knuth(list(group()),list(evaluation()),list(constraint())) -> ok.
knuth(Acc,Evaluations,Cons) ->
    case is_cover(Acc,Cons) of
        true -> [{result,[G || {G,_} <- Acc]}];
        false ->
            % PC = PassedConstraint
            case least_passed_constraints(Evaluations,Cons) of
                [] -> ok;
                [LeastPC|_] ->
                    FilteredGroups = lists:filter(fun({_,Cs}) -> lists:member(LeastPC,Cs) end,Evaluations),
                    lists:foldl(fun({Group,PCs},ResultAcc) ->
                        NextEvaluations = lists:filter(fun({_,Cs}) -> 
                            not lists:any(fun(PC) -> lists:member(PC,Cs) end,PCs)
                        end,Evaluations),
                        NextCons = remove(PCs,Cons),
                        ResultAcc ++ knuth([{Group,PCs} | Acc],NextEvaluations,NextCons)
                    end,[],FilteredGroups)
            end
    end.


-spec least_passed_constraints(list(evaluation()),list(constraint())) -> list(constraint()).
least_passed_constraints(Evaluations,Constraints) ->
    PassedConstraints = lists:flatten([Cs || {_,Cs} <- Evaluations]),
    Zip = lists:map(fun(C) ->
        {C,frequency(PassedConstraints,C)}
    end,Constraints),
    Min = lists:min([X || {_,X} <- Zip]),
    [C || {C,Freq} <- Zip, Freq == Min].


-spec frequency(list(),term()) -> integer().
frequency(L,Target) -> length([E || E <- L, E == Target]).


-spec remove(list(),list()) -> list().
remove(X,L) -> [Y || Y <- L, not lists:member(Y,X)].


-spec is_cover(list(group()),list(constraint())) -> boolean().
is_cover(Acc,Cons) ->
    L = lists:flatten([C || {_,C} <- Acc]),
    lists:all(fun(C) -> lists:member(C,L) end,Cons).


-spec evaluate(list(group()),list(constraint())) -> list(evaluation()).
evaluate(Gs,Cons) ->
    lists:map(fun(G) -> {G,lists:filter(fun(C) -> C(G) end,Cons)} end,Gs).
