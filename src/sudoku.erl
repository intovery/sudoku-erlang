-module(sudoku).
-export([
    convert/1,
    read_board/1,
    extract/2,
    extract_row/2,
    extract_column/2,
    extract_box/2,
    index_preset/2,
    print/1,
    is_45/1]).

-export_type([
    index/0,
    index_row/0,
    index_column/0,
    index_box/0,
    value/0,
    cell/0,
    board/0,
    suffix/0]).

-type index() :: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9.
-type index_row() :: index().
-type index_column() :: index().
-type index_box() :: index().

-type value() :: 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9.

-type cell() :: {index_row(),index_column(),value()}.
-type board() :: list(cell()).
-type suffix() :: list(cell()).

%% convert "null to 0 board" into "{Index,Value} board"
%% util function for legacy code
-spec convert(list()) -> board().
convert(CSV) ->
    lists:foldl(fun({RowIndex,ValueList},Board) ->
        Board ++ lists:foldl(fun({ColumnIndex,Value},Row) ->
            case list_to_integer(Value) of
                0 -> Row;
                V -> Row ++ [{{RowIndex,ColumnIndex},V}]
            end
        end,[],lists:zip(lists:seq(1,9),ValueList))
    end,[],lists:zip(lists:seq(1,9),CSV)).


-spec read_board(string()) -> any().
read_board(Name) ->
    convert(csv:read_csv(Name)).


%% extract functions
-spec extract({index_row(),index_column()} | list({index_row(),index_column()}),board()) -> suffix().
extract({TargetRow,TargetColumn},Board) ->
    lists:filter(fun({{Row,Column},_}) ->
        (Row == TargetRow) andalso (Column == TargetColumn)
    end,Board);

extract(TargetList,Board) when is_list(TargetList) ->
    lists:flatten(lists:map(fun({TargetRow,TargetColumn}) ->
        extract({TargetRow,TargetColumn},Board)
    end,TargetList)).

-spec extract_row(index_row(),board()) -> suffix().
extract_row(N,Board) ->
    TargetList = lists:map(fun(Col) -> {N,Col} end,lists:seq(1,9)),
    extract(TargetList,Board).

-spec extract_column(index_column(),board()) -> suffix().
extract_column(N,Board) ->
    TargetList = lists:map(fun(Row) -> {Row,N} end,lists:seq(1,9)),
    extract(TargetList,Board).

-spec extract_box(index_box(),board()) -> suffix().
extract_box(N,Board) ->
    L = [[1,2,3],[4,5,6],[7,8,9]],
    RowList = lists:nth(((N-1) div 3) + 1,L),
    ColList = lists:nth(((N-1) rem 3) + 1,L),
    extract([{Row,Col} || Row <- RowList, Col <- ColList],Board).


-spec index_preset(row | column | box, index()) -> list({index(),index()}).
%% index preset function
index_preset(row,N) ->
    [{N,Col} || Col <- lists:seq(1,9)]; 
index_preset(column,N) ->
    [{Row,N} || Row <- lists:seq(1,9)];
index_preset(box,N) ->
    L = [[1,2,3],[4,5,6],[7,8,9]],
    RowList = lists:nth(((N-1) div 3) + 1,L),
    ColList = lists:nth(((N-1) rem 3) + 1,L),
    [{Row,Col} || Row <- RowList, Col <- ColList].


-spec print(board()) -> ok.
%% formatted printing function
print(Board) ->
    F = fun(Row,Column) ->
        Fun = fun({{R,C},_}) -> (R == Row) andalso (C == Column) end,
        case lists:search(Fun,Board) of
            {value,{_,Value}} -> Value;
            false -> 0
        end
    end,
    lists:foreach(fun(Row) ->
        lists:foreach(fun(Column) ->
            Val = F(Row,Column),
            io:format("~2.. B ",[Val])
        end,lists:seq(1,9)),
        io:format("~n")
    end,lists:seq(1,9)).


-spec is_45(suffix()) -> boolean().
is_45(L) ->
    lists:foldl(fun({_,V},Acc) -> Acc + V end,0,L) == 45.
