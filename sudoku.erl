-module(sudoku).
-import(util,[range/2, reduce/3]).
-import(proc,[storage/1]).
-compile(export_all).

%% check string includes "\r\n"
is_carriage_return(L) ->
    length(L) /= 1.

%% eliminate "\r\n" that has been included from file:read_file/1
stabilize(L) ->
    Result = util:reduce(
        fun(Acc,X) -> 
            case is_carriage_return(X) of
                true -> util:reduce(fun(Acc_,X_) -> [X_|Acc_] end, Acc, string:tokens(X,"\r\n"));
                false -> [X|Acc]
            end
        end, [], L),
    lists:reverse(Result).

%% convert file binarg8y to list
read_board(F) ->
    {ok,Bin} = F,
    L = string:tokens(binary_to_list(Bin),","),
    lists:reverse(util:reduce(fun(Acc,X) -> [list_to_integer(X)|Acc] end,[],stabilize(L))).

%% check duplication
is_duplicated(L) ->
    Origin = length(lists:filter(fun(X) -> X/=0 end,L)),
    Sorted = length(lists:usort(lists:filter(fun(X) -> X/=0 end,L))),
    Origin /= Sorted.

%% convert X,Y indexing to list indexing
xy_to_list(X,0) -> X;
xy_to_list(X,Y) ->
    (Y*9)+X.

%% extraction functions
%% horizontal
get_row(Y,L) -> 
    lists:map(fun(X) -> lists:nth(xy_to_list(X,Y-1),L) end, lists:seq(1,9)).
%% vertical
get_column(X,L) ->
    lists:map(fun(Y) -> lists:nth(xy_to_list(X,Y-1),L) end, lists:seq(1,9)).
%% 3*3 square
get_square(N,L) ->
    BigX = (N-1) rem 3,
    BigY = (N-1) div 3,
    S = xy_to_list(BigX * 3, BigY * 3) + 1,
    Index = lists:map(fun(X) -> 
        S + ((X-1) div 3) * 9 + ((X-1) rem 3) end, lists:seq(1,9)),
    lists:map(fun(X) -> lists:nth(X,L) end, Index).

%% judge functions

%% get list from board, than verify
is_complete(F_extract,B) ->
    lists:all(fun(N) -> not is_duplicated(F_extract(N,B)) end,lists:seq(1,9)).

is_verified(B) ->
    lists:all(fun(F) -> is_complete(F,B) end,
    [fun get_column/2, fun get_square/2]).

missings(L) ->
    util:reduce(fun(Acc,X) ->
        case lists:member(X,L) of
            true -> Acc;
            false -> [X|Acc]
        end
    end,[],util:range(1,10)).

index_of_missing(L) ->
    util:reduce(fun(Acc,X) -> 
        case lists:nth(X,L) of
            0 -> [X|Acc];
            _ -> Acc
        end
    end,[],util:range(1,10)).

fill(Elems,Row) ->
    Indexs = lists:sort(index_of_missing(Row)),
    util:reduce(fun(Acc,N) ->
        I = lists:nth(N,Indexs),
        E = lists:nth(N,Elems),
        util:replace(I,E,Acc)
    end,Row,util:range(1,length(Indexs))).

fill_test(Row,Elems) ->
    fill_test(Row,Elems,[]).

fill_test([],_,Acc) ->
    lists:reverse(Acc);
fill_test([Row_H|Row_T],[],Acc) ->
    fill_test(Row_T,[],[Row_H|Acc]);
fill_test([Row_H|Row_T],[Elems_H|Elems_T],Acc) ->
    case Row_H == 0 of
        true -> fill_test(Row_T,Elems_T,[Elems_H|Acc]);
        false -> fill_test(Row_T,[Elems_H|Elems_T],[Row_H|Acc])
    end.

match_row_elems(Rows,Elems) ->
    lists:map(fun(N) -> 
        lists:map(fun(X) -> fill(X,lists:nth(N,Rows)) end,lists:nth(N,Elems))
        end,util:range(1,10)).

match(Rows,Perms) ->
    util:reduce(fun(Acc,N) ->
        util:replace(N,fill_test(lists:nth(N,Rows),lists:nth(N,Perms)),Acc)
        end,Rows,util:range(1,length(Perms)+1)).

solutions(L) ->
    Perms = util:perms(missings(L)),
    lists:map(fun(P) -> fill(P,L) end, Perms).

check([]) -> blank;
check([_|_]) -> filled.

to_indexes(N) ->
    I = integer_to_list(N,9),
    lists:map(fun(X) -> list_to_integer(X) end,util:lift(I)).

build_board(L) when length(L) == 81 ->
    L;
build_board(L) ->
    fun(L_next) -> build_board(L ++ L_next) end.

build_board() ->
    fun(L) -> build_board(L) end.

analyze() ->
    analyze("./data_baekjun.csv").
analyze(Name) ->
    B = read_board(file:read_file(Name)),
    Rows = lists:map(fun(N) -> get_row(N,B) end,util:range(1,10)),
    Elems = lists:map(fun(N) ->
        util:perms(missings(lists:nth(N,Rows)))
        end,util:range(1,10)),
    {B,Rows,Elems}.

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

divide(L) ->
    % Len = length(L),
    % [lists:sublist(L,Len div 2),lists:sublist(L,(Len div 2) + 1, Len div 2)],
    util:chunks(L,(length(L) div 2)).

filter_bad_perms(Rows,Perms) ->
    lists:map(fun(N) ->
        Row = lists:nth(N,Rows),
        RowPerms = lists:nth(N,Perms),
        lists:filter(fun(X) ->
            Replaced = util:replace(N,fill(X,Row),Rows),
            is_verified(lists:flatten(Replaced))
        end,RowPerms)
    end,lists:seq(1,9)).
filter_bad_perms(Replaced) ->
    ExtFunc = [fun get_row/2, fun get_column/2, fun get_square/2],
    Func = fun(F) -> 
        lists:any(fun(N) ->
            is_duplicated(F(N,Replaced))
            end,lists:seq(1,9))
        end,
    lists:any(fun(F) ->
        Func(F)
        end,ExtFunc).

solve(Name) ->
    {Board,Rows,Perms} = analyze(Name),
    NewPerms = sudoku:filter_bad_perms(Rows,Perms),
    Results = build(Rows,NewPerms),
    {Board,Results}.

