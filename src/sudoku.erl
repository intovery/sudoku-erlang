-module(sudoku).
-compile(export_all).

%% convert "null to 0 board" into "{Index,Value} board"
%% util function for legacy code
convert(CSV) ->
    lists:foldl(fun({RowIndex,ValueList},Board) ->
        Board ++ lists:foldl(fun({ColumnIndex,Value},Row) ->
            case list_to_integer(Value) of
                0 -> Row;
                V -> Row ++ [{RowIndex,ColumnIndex,V}]
            end
        end,[],lists:zip(lists:seq(1,9),ValueList))
    end,[],lists:zip(lists:seq(1,9),CSV)).


%% extract functions
extract({TargetRow,TargetColumn},Board) ->
    lists:filter(fun({Row,Column,_}) ->
        (Row == TargetRow) andalso (Column == TargetColumn)
    end,Board);

extract(TargetList,Board) when is_list(TargetList) ->
    lists:flatten(lists:map(fun({TargetRow,TargetColumn}) ->
        extract({TargetRow,TargetColumn},Board)
    end,TargetList)).

extract_row(N,Board) ->
    TargetList = lists:map(fun(Col) -> {N,Col} end,lists:seq(1,9)),
    extract(TargetList,Board).
extract_column(N,Board) ->
    TargetList = lists:map(fun(Row) -> {Row,N} end,lists:seq(1,9)),
    extract(TargetList,Board).
extract_grid(N,Board) ->
    L = [[1,2,3],[4,5,6],[7,8,9]],
    RowList = lists:nth(((N-1) div 3) + 1,L),
    ColList = lists:nth(((N-1) rem 3) + 1,L),
    extract([{Row,Col} || Row <- RowList, Col <- ColList],Board).


%% index preset function
index_preset(row,N) ->
    [{N,Col} || Col <- lists:seq(1,9)];
index_preset(column,N) ->
    [{Row,N} || Row <- lists:seq(1,9)];
index_preset(grid,N) ->
    L = [[1,2,3],[4,5,6],[7,8,9]],
    RowList = lists:nth(((N-1) div 3) + 1,L),
    ColList = lists:nth(((N-1) rem 3) + 1,L),
    [{Row,Col} || Row <- RowList, Col <- ColList].


%% formatted printing function
print(Board) ->
    F = fun(Row,Column) ->
        Fun = fun({R,C,_}) -> (R == Row) andalso (C == Column) end,
        case lists:search(Fun,Board) of
            {value,{_,_,Value}} -> Value;
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
