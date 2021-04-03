%% -*- erlang -*-
%%! -pa _build/default/lib/sudoku/ebin
main([Path]) ->
    Solved = solver:solve(Path),
    print_results(Solved),
    halt(1);

main(_) ->
    main().

main() ->
    usage().

print_results(Solved) ->
    lists:foreach(fun({Idx,Board}) ->
        io:format("SOLUTION ~p~n",[Idx]),
        sudoku:print(Board)
    end,lists:zip(lists:seq(1,length(Solved)),Solved)).

usage() ->
    io:format("USAGE: escript solve_sudoku.escript <PATH>~n").