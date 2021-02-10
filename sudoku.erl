-module(sudoku).
-import(util,[range/2, reduce/3]).
-import(proc,[storage/1]).
-compile(export_all).

delete_CRLF(L) when length(L) == 1 -> [L];
delete_CRLF(L) ->
    [[hd(L)],[lists:last(L)]].

%% eliminate "\r\n" that has been included from file:read_file/1
stabilize(L) -> util:reduce(fun(Acc,X) -> Acc ++ delete_CRLF(X) end, [], L).

%% convert file binary to list
read_board(F) ->
    {ok,Bin} = F,
    L = string:tokens(binary_to_list(Bin),","),
    lists:map(fun(X) -> list_to_integer(X) end, stabilize(L)).

%% check duplication
is_duplicated(L) ->
    Origin = length(lists:filter(fun(X) -> X/=0 end,L)),
    Sorted = length(lists:usort(lists:filter(fun(X) -> X/=0 end,L))),
    Origin /= Sorted.

%% extraction functions
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

%% judge functions
is_complete(F_extract,B) ->
    lists:all(fun(N) -> not is_duplicated(F_extract(N,B)) end,lists:seq(1,9)).

is_verified(B) ->
    lists:all(fun(F) -> 
        is_complete(F,B) 
    end,[fun get_column/2, fun get_square/2]).

missings(L) ->
    lists:filter(fun(N) ->
        not lists:member(N,L)
    end,lists:seq(1,9)).

fill(Row,Elems) ->
    fill(Row,Elems,[]).

fill([],_,Acc) ->
    lists:reverse(Acc);
fill([Row_H|Row_T],[],Acc) ->
    fill(Row_T,[],[Row_H|Acc]);
fill([Row_H|Row_T],[Elems_H|Elems_T],Acc) ->
    case Row_H == 0 of
        true -> fill(Row_T,Elems_T,[Elems_H|Acc]);
        false -> fill(Row_T,[Elems_H|Elems_T],[Row_H|Acc])
    end.

match_row_elems(Rows,Elems) ->
    lists:map(fun(N) -> 
        lists:map(fun(X) -> fill(X,lists:nth(N,Rows)) end,lists:nth(N,Elems))
    end,util:range(1,10)).

match(Rows,Perms) ->
    util:reduce(fun(Acc,N) ->
        util:replace(N,fill(lists:nth(N,Rows),lists:nth(N,Perms)),Acc)
    end,Rows,lists:seq(1,length(Perms))).

analyze() ->
    analyze("./data.csv").
analyze(Name) ->
    Board = read_board(file:read_file(Name)),
    Rows = lists:map(fun(N) -> get_row(N,Board) end,util:range(1,10)),
    Elems = lists:map(fun(N) ->
        util:perms(missings(lists:nth(N,Rows)))
        end,lists:seq(1,9)),
    {Board,Rows,Elems}.

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
            Replaced = util:replace(N,fill(Perm,Row),Rows),
            is_verified(lists:flatten(Replaced))
        end,RowPerms)
    end,lists:zip3(lists:seq(1,9),Rows,Perms)).

solve(Name) ->
    {Board,Rows,Perms} = analyze(Name),
    Results = build(Rows,Perms),
    {Board,Results}.
