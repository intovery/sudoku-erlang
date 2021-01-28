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

%% code from "stack overflow"
%% https://stackoverflow.com/questions/12534898/splitting-a-list-in-equal-sized-chunks-in-erlang/15008490
chunks(List,Len) ->
  LeaderLength = case length(List) rem Len of
      0 -> 0;
      N -> Len - N
  end,
  Leader = lists:duplicate(LeaderLength,undefined),
  chunks(Leader ++ lists:reverse(List),[],0,Len).

chunks([],Acc,_,_) -> Acc;
chunks([H|T],Acc,Pos,Max) when Pos==Max ->
    chunks(T,[[H] | Acc],1,Max);
chunks([H|T],[HAcc | TAcc],Pos,Max) ->
    chunks(T,[[H | HAcc] | TAcc],Pos+1,Max);
chunks([H|T],[],Pos,Max) ->
    chunks(T,[[H]],Pos+1,Max).

%% convert X,Y indexing to list indexing
xy_to_list(X,Y) ->
    (Y*9)+X.

%% extraction functions
%% horizontal
get_row(Y,L) -> 
    lists:map(fun(X) -> lists:nth(xy_to_list(X,Y)+1,L) end, range(0,9)).
%% vertical
get_column(X,L) ->
    lists:map(fun(Y) -> lists:nth(xy_to_list(X,Y)+1,L) end, range(0,9)).
%% 3*3 square
get_square(N,L) ->
    BigX = N rem 3,
    BigY = N div 3,
    S = xy_to_list(BigX * 3, BigY * 3),
    lists:map(fun(X) -> lists:nth(S + (X div 3) * 9 + (X rem 3) + 1,L) end, range(0,9)).

%% judge functions

%% 
is_sum_45(L) ->
    45 == util:reduce(fun(Acc,X) -> Acc + X end, 0, L).
%% get list from board, than verify
is_complete(F,B) ->
    lists:all(fun(X) -> is_verified(F(X,B)) end,util:range(0,9)).
%% verify list
is_verified(L) ->
    (is_sum_45(L)) and not (is_duplicated(L)).
%% verify board
verify(B) ->
    ExtractFunctions = [fun get_row/2, fun get_column/2, fun get_square/2],
    case lists:all(fun(F) -> is_complete(F,B) end, ExtractFunctions) of
        true -> good;
        false -> bad
    end.

missings(L) ->
    util:reduce(fun(Acc,X) ->
        case lists:member(X,L) of
            true -> Acc;
            false -> [X|Acc]
        end
    end,[],util:range(1,10)).

index_of_missing(L) ->
    util:reduce(fun(Acc,X) -> 
        case lists:nth(X+1,L) of
            0 -> [X|Acc];
            _ -> Acc
        end
    end,[],util:range(0,9)).

fill(Elems,Row) ->
    Indexs = lists:sort(index_of_missing(Row)),
    util:reduce(fun(Acc,N) ->
        I = lists:nth(N+1,Indexs),
        E = lists:nth(N+1,Elems),
        util:replace(I,E,Acc)
    end,Row,util:range(0,length(Indexs))).

match_row_elems(Rows,Elems) ->
    lists:map(fun(N) -> 
        lists:map(fun(X) -> fill(X,lists:nth(N,Rows)) end,lists:nth(N,Elems))
        end,util:range(1,10)).

match(Rows,MissingLists) ->
    lists:map(fun(N) -> 
        fill(lists:nth(N,MissingLists),lists:nth(N,Rows))
    end, util:range(1,10)).

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
    B = read_board(file:read_file("./data_easy.csv")),
    Rows = lists:map(fun(N) -> get_row(N,B) end,util:range(0,9)),
    Elems = lists:map(fun(N) ->
        util:perms(missings(lists:nth(N+1,Rows)))
        end,util:range(0,9)),
    {B,Rows,Elems}.
     

build_square(L) when length(L) == 9 ->
    L;
build_square(L) ->
    fun(L_next) -> build_square(L ++ L_next) end.

build_square() ->
    fun(L) -> build_square(L) end.

dive([],Result,F) ->
    F(lists:reverse(Result));
dive([H|T],Acc,F) ->
    lists:foreach(fun(X) ->
        dive(T,[X|Acc],F)
        end,H).

build(L) ->
    util:reduce(fun(Acc,X) -> Acc++X end, [], L).

test_judge({Rows,State,Checked}) ->
    receive
        {matched,Elems} ->
            Board = lists:flatten(match(Rows,Elems)),
            case verify(Board) of
                good -> test_judge({Rows,[Board|State],Checked+1});
                bad -> test_judge({Rows,State,Checked+1})
            end;
        {show_status} ->
            io:format("CORRECT BOARDS: ~p~n",[length(State)]),
            io:format("CHECKED: ~p~n",[Checked]),
            test_judge({Rows,State,Checked})
    end.

test_storage(State) ->
    receive
        {matched,Board} ->
            test_storage([Board|State]);
        {show_status} ->
            io:format("CORRECT BOARDS: ~p~n",[length(State)]),
            test_storage(State)
    end.

test_cnt(State) ->
    receive
        {increase} ->
            test_cnt(State+1);
        {get,Pid} ->
            Pid ! {get,State},
            test_cnt(State);
        _ ->
            io:format("???",[])
    end.

proc_builder(Pid_Storage,Pid_Cnt,Rows) ->
    fun(X) ->
        proc_builder(Pid_Storage,Pid_Cnt,Rows,X)
    end.
proc_builder(Pid_Storage,Pid_Cnt,Rows,Perms) ->
    io:format("proc start~n",[]),
    dive(Perms,[],fun(X) ->
        Pid_Cnt ! {increase},
        Board = lists:flatten(match(Rows,X)),
        case verify(Board) of
            good -> Pid_Storage ! {good,Board};
            bad -> ok
        end
    end),
    io:format("proc done...~n",[]).
    

divide(L) ->
    Len = length(L),
    [lists:sublist(L,Len div 2),lists:sublist(L,(Len div 2) + 1, Len div 2)].

check_and_throw(Pid,Rows) ->
    fun(X) ->
        Board = lists:flatten(match(Rows,X)),
        case verify(Board) of
            good -> Pid ! {matched,Board};
            bad -> ok
        end
    end.