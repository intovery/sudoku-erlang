-module(sudoku).
-import(util,[range/2, reduce/3]).
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

%% convert file binary to list
binary_to_board(F) ->
    {ok,Bin} = F,
    L = string:tokens(binary_to_list(Bin),","),
    lists:reverse(util:reduce(fun(Acc,X) -> [list_to_integer(X)|Acc] end,[],stabilize(L))).

%% check duplication
is_duplicated(L) ->
    Origin = length(lists:filter(fun(X) -> X/=0 end,L)),
    Sorted = length(lists:usort(lists:filter(fun(X) -> X/=0 end,L))),
    Origin /= Sorted.

%% code from "stack overflow"
%% https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang/15008490
n_length_chunks_fast(List,Len) ->
  LeaderLength = case length(List) rem Len of
      0 -> 0;
      N -> Len - N
  end,
  Leader = lists:duplicate(LeaderLength,undefined),
  n_length_chunks_fast(Leader ++ lists:reverse(List),[],0,Len).

n_length_chunks_fast([],Acc,_,_) -> Acc;
n_length_chunks_fast([H|T],Acc,Pos,Max) when Pos==Max ->
    n_length_chunks_fast(T,[[H] | Acc],1,Max);
n_length_chunks_fast([H|T],[HAcc | TAcc],Pos,Max) ->
    n_length_chunks_fast(T,[[H | HAcc] | TAcc],Pos+1,Max);
n_length_chunks_fast([H|T],[],Pos,Max) ->
    n_length_chunks_fast(T,[[H]],Pos+1,Max).

%% convert X,Y indexing to list indexing
xy_to_list(X,Y) ->
    (Y*9)+X.

%% extraction functions
%% horizontal
get_row(L,Y) -> 
    lists:map(fun(X) -> lists:nth(xy_to_list(X,Y) + 1,L) end, range(0,9)).
%% vertical
get_column(L,X) ->
    lists:map(fun(Y) -> lists:nth(xy_to_list(X,Y) + 1,L) end, range(0,9)).
%% 3*3 square
get_square(L,N) ->
    BigX = N rem 3,
    BigY = N div 3,
    S = xy_to_list(BigX * 3, BigY * 3),
    lists:map(fun(X) -> lists:nth(S + (X div 3) * 9 + (X rem 3) + 1,L) end, range(0,9)).

%% verify list
is_complete(L) ->
    45 == util:reduce(fun(Acc,X) -> Acc + X end, 0, L).
%% get list from board, than verify
is_complete(F,B) ->
    lists:all(fun(X) -> is_complete(F(B,X)) end, util:range(0,9)).

%% verify board
verify(B) ->
    case lists:all(fun(F) -> is_complete(F,B) end,[fun get_row/2, fun get_column/2, fun get_square/2]) of
        true -> good;
        false -> bad
    end.