-module(solver).
-compile(export_all).

testcase() ->
    {[1,2,3,4,5,6,7],
        [[3,5,6],
        [1,4,7],
        [2,3,6],
        [1,4],
        [2,7],
        [4,5,7]]}.

testcase(multiple) ->
    {[1,2,3,4],
        [[1,2],
        [3,4],
        [1],
        [2],
        [3],
        [4],
        [1,2,3,4]]
    }.

% frequency(Elements,L) ->
%     Freq = maps:from_list(lists:map(fun(Elem) ->
%         {Elem,0}
%     end,Elements)),
%     lists:foldl(fun(E,Acc) ->
%         Acc#{E := (maps:get(E,Acc) + 1)}
%     end,Freq,L).

frequency(Elements,L) ->
    Freq = maps:from_list([{E,0} || E <- Elements]),
    lists:foldl(fun(X,Acc) ->
        Acc#{X := (maps:get(X,Acc) + 1)}
    end,Freq,lists:flatten(L)).

frequency(L) ->
    lists:map(fun(Element) ->
        {Element,length([E || E <- L, E == Element])}
    end,list_of_elements(L)).

list_of_elements(L) ->
    lists:foldl(fun(X,Acc) ->
        case lists:member(X,Acc) of
            true -> Acc;
            false -> [X|Acc]
        end
    end,[],L).

tag_index(L) ->
    lists:map(fun({Idx,Suffix}) ->
        {util:integer_to_atom(Idx),Suffix}
    end,lists:zip(lists:seq(1,length(L)),L)).

solve({L,Suffixes}) ->
    Storage = spawn(solver,simple_storage,[[]]),
    bruteforce(fun(R) -> Storage ! {append,R} end,tag_index(Suffixes),L),
    fold_suffix_combinations(call_return(Storage)).

solve_knuth({L,Suffixes}) ->
    Storage = spawn(solver,simple_storage,[[]]),
    knuth(fun(R) -> Storage ! {append,R} end,tag_index(Suffixes),L),
    fold_suffix_combinations(call_return(Storage)).

bruteforce(F,L_Suffix,L_Element) ->
    bruteforce(F,[],L_Suffix,L_Element).

bruteforce(F,Acc_Suffix,L_Suffix,L_Element) ->
    case is_exact_cover(Acc_Suffix,L_Element) of
        true -> F([L || {_,L} <- Acc_Suffix]);
        false ->
            lists:foreach(fun(Suffix) ->
                bruteforce(F,[Suffix|Acc_Suffix],lists:delete(Suffix,L_Suffix),L_Element)
            end,L_Suffix)
    end.

knuth(F,L_Suffix,L_Element) ->
    knuth(F,[],L_Suffix,L_Element).

knuth(F,Acc_Suffix,[],L_Element) ->
    case is_exact_cover(Acc_Suffix,L_Element) of
        true -> F([L || {_,L} <- Acc_Suffix]);
        false -> ok
    end;

knuth(F,Acc_Suffix,L_Suffix,L_Element) ->
    case is_exact_cover(Acc_Suffix,L_Element) of
        true -> F([L || {_,L} <- Acc_Suffix]);
        false ->
            Frequency = frequency(lists:merge([X || {_,X} <- L_Suffix])),
            Min = lists:min([Freq || {_,Freq} <- Frequency]),
            MinElements = [{E,Freq} || {E,Freq} <- Frequency, Freq == Min],
            Filtered = lists:filter(fun({_,G}) ->
                lists:all(fun(N) ->
                    not lists:member(N,G)
                end,MinElements)
            end,L_Suffix),
            lists:foreach(fun(Suffix) ->
                knuth(F,[Suffix|Acc_Suffix],lists:delete(Suffix,Filtered),L_Element)
            end,Filtered)
    end.

is_exact_cover(Acc_Group,L_Element) ->
    {_,Flatten} = flatten_group(Acc_Group),
    lists:sort(Flatten) == lists:sort(L_Element).


flatten_group(L_Group) ->
    {lists:flatten([G || {G,_} <- L_Group]),lists:flatten([E || {_,E} <- L_Group])}.

simple_storage(State) ->
    receive
        {append,D} -> simple_storage([D|State]);
        {return,Pid} ->
            Pid ! {result,State},
            ok
    end.

call_return(Pid) ->
    Pid ! {return,self()},
    receive
        {result,R} -> R;
        _ -> err
    end.

fold_suffix_combinations(L) ->
    lists:foldl(fun(X,Acc) ->
        Sorted = lists:sort(X),
        case lists:member(Sorted,Acc) of
            true -> Acc;
            false -> [Sorted | Acc]
        end
    end,[],L).
