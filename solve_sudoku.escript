%% -*- erlang -*-
%%! -pa _build/default/lib/sudoku/ebin
main([Path]) ->
    {Time,Solved} = timer:tc(fun() -> solver:solve(Path) end),
    io:format("~p~n",[Time]),
    print_results(Solved),
    halt(1);

main() ->
    usage().


print_results(Solved) ->
    lists:foreach(fun({Idx,Board}) ->
        io:format("SOLUTION ~p~n",[Idx]),
        sudoku:print(Board)
    end,lists:zip(lists:seq(1,length(Solved)),Solved)).

usage() ->
    io:format("USAGE: escript solve_sudoku.escript <PATH>~n").