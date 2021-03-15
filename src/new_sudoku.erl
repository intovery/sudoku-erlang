-module(new_sudoku).
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
        ({Index,0},Acc) -> Acc;
        ({Index,V},Acc) -> [{Index,V}|Acc]
    end,[],lists:zip(lists:seq(1,81),Board)).

get_row(N,Board) ->
    (extraction_indexes(si:get_row(N)))(Board).
get_column(N,Board) ->
    (extraction_indexes(si:get_column(N)))(Board).
get_square(N,Board) ->
    (extraction_indexes(si:get_square(N)))(Board).

index_preset(row) ->


extraction_indexes(Targets) ->
    fun(Board) ->
        [{Index,Val} || TargetIdx <- Targets, {Index,Val} <- Board, TargetIdx == Index]
    end.

%% verification functions

is_verified_board(Board) ->
    lists:all(fun(Extraction) ->
        lists:all(fun(N) ->
            is_verified_set(Extraction(N,Board))
        end,lists:seq(1,9))
    end,[fun get_row/2, fun get_column/2, fun get_square/2]).

is_verified_set(Set) ->
    lists:all(fun(F) -> F(Set) end, [fun is_full/1, fun is_not_duplicated/1, fun is_clean/1]).

%% set checking is ok to continue

is_full(Set) -> length(Set) == 9.

is_not_duplicated(Set) ->
    L = [Val || {_,Val} <- Set],
    length(L) == length(lists:usort(L)).

is_clean(Set) ->
    lists:all(fun({_,Val}) -> (Val >= 1) andalso (Val =< 9) end, Set).

% extraction_board(Board) ->
%     fun(Targets) ->
%         [{Index,Val} || TargetIdx <- Targets, {Index,Val} <- Board, TargetIdx == Index]
%     end.

%% convert Index into specific row, column, square position

position_row(N) -> ((N-1) div 9) + 1.
position_column(N) -> ((N-1) rem 9) + 1.
position_square(N) ->
    BigRow = ((position_row(N) - 1) div 3),
    BigColumn = ((position_column(N) - 1) div 3),
    (BigRow * 3) + BigColumn + 1.

print(L) ->
    lists:foreach(fun
        ({N,Idx}) when (Idx rem 9) == 0 -> io:format("~2.. B~n",[N]);
        ({N,_}) -> io:format("~2.. B ",[N])
    end,lists:zip(L,lists:seq(1,81))).
