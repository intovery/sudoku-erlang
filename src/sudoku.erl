-module(sudoku).
% -export([
%     convert/1,
%     get_row/2,
%     extraction_indexes/1,
%     is_full/1]).
-compile(export_all).

%% convert "null to 0 board" into "{Index,Value} board"
%% util function for legacy code
convert(Board) when is_list(Board) ->
    lists:foldl(fun
        ({_,0},Acc) -> Acc;
        ({Index,V},Acc) -> [{Index,V}|Acc]
    end,[],lists:zip(lists:seq(1,81),Board)).

extract_row(N,Board) ->
    extract(index_preset(row,N),Board).

extract_column(N,Board) ->
    extract(index_preset(column,N),Board).

extract_grid(N,Board) ->
    extract(index_preset(grid,N),Board).

extract(Index,Board) ->
    {value,Element} = lists:search(fun({Idx,_}) -> Idx == Index end, Board),
    Element.

extract(IndexList,Board) ->
    lists:filter(fun({Idx,_}) -> lists:member(Idx,IndexList) end,Board).

% index presets of extraction strategies with specific index
index_preset(none) -> [];
index_preset(all) -> lists:seq(1,81);

index_preset(Shape) ->
    lists:map(fun(N) ->
        index_preset(Shape,N)
    end,lists:seq(1,9)).

index_preset(row,Index) ->
    lists:map(fun(N) -> N + ((Index-1)*9) end,lists:seq(1,9));

index_preset(column,Index) ->
    lists:map(fun(N) -> Index + ((N-1)*9) end,lists:seq(1,9));

index_preset(grid,Index) ->
    GridStart = (((Index-1) rem 3) * 3) + (((Index-1) div 3) * 27),
    lists:map(fun(N) -> 
        GridCell = (((N-1) rem 3) + 1) + (((N-1) div 3) * 9),
        GridStart + GridCell
    end,lists:seq(1,9)).

%% verification functions

is_verified_board(Board) ->
    lists:all(fun(Verification) ->
        Verification(board,Board)
    end,[fun is_full/2, fun is_not_duplicated/2, fun is_clean/2]).

is_verified_set(Set) ->
    lists:all(fun(Verification) -> 
        Verification(set,Set)
    end,[fun is_full/2, fun is_not_duplicated/2, fun is_clean/2]).

%% set checking
is_full(set,Set) -> length(Set) == 9.

is_not_duplicated(set,Set) ->
    L = lists:map(fun({_,Val}) -> Val end, Set),
    length(L) == length(lists:usort(L)).

is_clean(set,Set) ->
    lists:all(fun({_,Val}) -> (Val >= 1) andalso (Val =< 9) end, Set).

%% board checking
is_full(board,Board) -> length(Board) == 81.

is_not_duplicated(board,Board) ->
    L = lists:map(fun({Idx,_}) -> Idx end, Board),
    length(L) == length(lists:usort(L)).

is_clean(board,Board) ->
    lists:all(fun({_,Val}) -> (Val >= 1) andalso (Val =< 9) end, Board).

%% search function about specific shape position

position({Shape,N}) ->
    PresetList = index_preset(Shape),
    {value,{Index,_}} = lists:search(fun({_,Preset}) ->
        lists:member(N,Preset)
    end,lists:zip(lists:seq(1,9),PresetList)),
    Index.

%% function to make full list of set & board
%% blanks of set & board will be added as {Index,0}
make_full_set(L) ->
    lists:map(fun(Nth) ->
        case lists:search(fun({Idx,_}) -> Idx == Nth end,L) of
            {value,Result} -> Result;
            false -> {Nth,0}
        end
    end,lists:seq(1,9)).

make_full_board(L) ->
    lists:map(fun(Nth) ->
        case lists:search(fun({Idx,_}) -> Idx == Nth end,L) of
            {value,Result} -> Result;
            false -> {Nth,0}
        end
    end,lists:seq(1,81)).

%% formatted printing function
print(L) ->
    lists:foreach(fun
        ({N,Idx}) when (Idx rem 9) == 0 -> io:format("~2.. B~n",[N]);
        ({N,_}) -> io:format("~2.. B ",[N])
    end,lists:zip(L,lists:seq(1,length(L)))).
