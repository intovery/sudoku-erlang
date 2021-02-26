-module(si).
-import(util,[range/2, reduce/3]).
-compile(export_all).

%% check duplication
is_duplicated(L) ->
    Origin = length(lists:filter(fun(X) -> X/=0 end,L)),
    Sorted = length(lists:usort(lists:filter(fun(X) -> X/=0 end,L))),
    Origin /= Sorted.

%% judge functions
is_complete(F_extract,B) ->
    lists:all(fun(N) -> not is_duplicated(F_extract(N,B)) end,lists:seq(1,9)).

is_verified(B) ->
    lists:all(fun(F) -> 
        is_complete(F,B) 
    end,[si_get:get_fun(column), si_get:get_fun(square)]).

missings(L_Exists) ->
    missings(lists:seq(1,9),L_Exists).
missings(L,[]) -> L;
missings(L,[H|T]) -> missings(lists:delete(H,L),T).

fill(Row,Elems) ->
    fill(Row,Elems,[]).

fill([],_,Acc) -> lists:reverse(Acc);
fill([Row_H|Row_T],[],Acc) ->
    fill(Row_T,[],[Row_H|Acc]);
fill([0|Row_T],[Elems_H|Elems_T],Acc) ->
    fill(Row_T,Elems_T,[Elems_H|Acc]);
fill([Row_H|Row_T],[Elems_H|Elems_T],Acc) ->
    fill(Row_T,[Elems_H|Elems_T],[Row_H|Acc]).

match(Rows,Perms) ->
    util:reduce(fun(Acc,N) ->
        util:replace(N,fill(lists:nth(N,Rows),lists:nth(N,Perms)),Acc)
    end,Rows,lists:seq(1,length(Perms))).

analyze() -> analyze("./data/data.csv").
analyze(Name) ->
    % Board = si_read:read_board(file:read_file(Name)),
    % Rows = lists:map(fun(N) -> si_get:get(Board,{row,N}) end,lists:seq(1,9)),
    % Elems = lists:map(fun(N) ->
    %             util:perms(missings(lists:nth(N,Rows)))
    %         end,lists:seq(1,9)),
    % {Board,Rows,Elems}.
    si_read:read_board(file:read_file(Name)).

build(Rows,Perms) ->
    build(Rows,Perms,{1,[]},[]).
build(Rows,[],{_Cur,Acc_Perms},Acc_Results) ->
    P = lists:reverse(Acc_Perms),
    Board = lists:flatten(match(Rows,P)),
    case is_verified(Board) of
        true -> [Board|Acc_Results];
        false -> Acc_Results
    end;
build(Rows,[H|T],{Cur,Acc_Perms},Acc_Results) ->
    util:reduce(fun(List_Result,Perm) ->
        P = lists:reverse([Perm|Acc_Perms]),
        case is_verified(lists:flatten(match(Rows,P))) of
            true -> build(Rows,T,{Cur+1,[Perm|Acc_Perms]},List_Result);
            false -> List_Result
        end
    end,Acc_Results,H).

filter_bad_perms(Rows,Perms) ->
    lists:map(fun({N,Row,RowPerms}) ->
        lists:filter(fun(Perm) ->
            Replaced = util:replace(N,fill(Row,Perm),Rows),
            is_verified(lists:flatten(Replaced))
        end,RowPerms)
    end,lists:zip3(lists:seq(1,9),Rows,Perms)).

solve(Name) ->
    {Board,Rows,Perms} = analyze(Name),
    % Results = build(Rows,Perms),
    {Board,interpret(Board)}.

mix([CL1,CL2,CL3,CL4,CL5,CL6,CL7,CL8,CL9]) ->
    [[C1,C2,C3,C4,C5,C6,C7,C8,C9] ||
        C1 <- CL1,
        C2 <- CL2,
        C3 <- CL3,
        C4 <- CL4,
        C5 <- CL5,
        C6 <- CL6,
        C7 <- CL7,
        C8 <- CL8,
        C9 <- CL9].

mix(L1,L2) -> [[E1,E2] || E1 <- L1, E2 <- L2].

interpret(Board) ->
    Rows = lists:map(fun(N) -> si_get:get(Board,{row,N}) end, lists:seq(1,9)).