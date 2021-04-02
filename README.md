# Sudoku Interpreter w/ Erlang

Sudoku Interpreter implemented by Erlang.

# Project purpose
+ Build tools solving multiple problems about sudoku
+ Learn basic Erlang syntax & how to build Erlang program

# Modules
+ sudoku: reading board + inspect sudoku's properties
+ knuth: to solve "exact cover" problems, including sudoku
+ testcase: "exact cover" problem examples(exact cover, 2x2 sudoku, 9x9 sudoku)
+ solver: sudoku solver using module "knuth.erl"

# How I used it
1. Add sudoku problem into "./data" folder with format .csv
2. Call "solver:solve/1" or "solver:solve/2" with path of sudoku problem

>   ```erlang
>   1> solver:solve("./data/data.csv").
>   [[[{{9,7},1}],
>     [{{9,6},6}],
>     [{{9,...},2}],
>     [{{...},...}],
>     [{...}],
>     [...]|...]]
>     
>   2> solver:solve(fun(R) -> io:format("LENGTH => ~p~n",[length(R)]) end,"./data/data.csv").
>   LENGTH => 52
>   ok
>   
>   3> timer:tc(fun() -> solver:solve("./data/data.csv") end).
>   {110000,
>    [[[{{9,7},1}],
>      [{{9,6},6}],
>      [{{9,...},2}],
>      [{{...},...}],
>      [{...}],
>      [...]|...]]}
>   ```

# How can I test it?
Use "solve_sudoku.escript" to test sudoku solver.

>   ```erlang
>   ~$ escript solve_sudoku.escript
>   USAGE: escript solve_sudoku.escript <PATH>
>   ~$ escript solve_sudoku.escript "./data/data.csv"
>   SOLUTION 1
>    5  3  4  6  7  8  9  1  2
>    6  7  2  1  9  5  3  4  8
>    1  9  8  3  4  2  5  6  7
>    8  5  9  7  6  1  4  2  3
>    4  2  6  8  5  3  7  9  1
>    7  1  3  9  2  4  8  5  6
>    9  6  1  5  3  7  2  8  4
>    2  8  7  4  1  9  6  3  5
>    3  4  5  2  8  6  1  7  9
>   ~$ escript solve_sudoku.escript "./data/data_multiple.csv"
>   SOLUTION 1
>    5  3  4  6  7  8  9  1  2
>    6  7  2  1  9  5  3  4  8
>    1  9  8  3  4  2  5  6  7
>    8  2  6  7  5  1  4  9  3
>    4  5  9  8  6  3  7  2  1
>    7  1  3  9  2  4  8  5  6
>    9  6  1  5  3  7  2  8  4
>    2  8  7  4  1  9  6  3  5
>    3  4  5  2  8  6  1  7  9
>   SOLUTION 2
>    5  3  4  6  7  8  9  1  2
>    6  7  2  1  9  5  3  4  8
>    1  9  8  3  4  2  5  6  7
>    8  5  9  7  6  1  4  2  3
>    4  2  6  8  5  3  7  9  1
>    7  1  3  9  2  4  8  5  6
>    9  6  1  5  3  7  2  8  4
>    2  8  7  4  1  9  6  3  5
>    3  4  5  2  8  6  1  7  9
