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

# Example
```erlang
1> solver:solve("./data/data.csv").
[[[{{9,7},1}],
  [{{9,6},6}],
  [{{9,...},2}],
  [{{...},...}],
  [{...}],
  [...]|...]]
  
2> solver:solve(fun(R) -> io:format("LENGTH => ~p~n",[length(R)]) end,"./data/data.csv").
LENGTH => 52
ok

3> timer:tc(fun() -> solver:solve("./data/data.csv") end).
{110000,
 [[[{{9,7},1}],
   [{{9,6},6}],
   [{{9,...},2}],
   [{{...},...}],
   [{...}],
   [...]|...]]}
```
