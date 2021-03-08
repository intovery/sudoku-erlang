# Sudoku Interpreter w/ Erlang

Sudoku Interpreter implemented by Erlang.

# Project purpose
+ Build tools solving multiple problems about sudoku
+ Learn basic Erlang syntax & how to build Erlang program
+ Make experienced about "multi-process based" program

# Modules
+ util: Utilization functions to use erlang easier
+ si_read: Make list of integers(1~9) by reading .csv files in folder "./data"
+ si: Functions to analyze sudoku and comeplete it
+ parallel: Map function with multi-process

# How I used it
1. Add .csv sudoku file into "./data" folder
2. Read .csv binary data
3. 'si_read:read_board({ok,is_binary()})' will gives you the list of elements in sudoku
4. 'si:solve(<File Location + Name>)' will gives you solutions of sudoku

# Example
```
1> si:solve("./data/data.csv").
[[5,3,4,6,7,8,9,1,2,6,7,2,1,9,5,3,4,8,1,9,8,3,4,2,5,6|...]].
2> timer:tc(fun() -> si:solve("./data/data.csv") end).
{2609000,
 [[5,3,4,6,7,8,9,1,2,6,7,2,1,9,5,3,4,8,1,9,8,3,4,2,5,6|...]]}
```

# Milestones
+ Add multi-processing
+ Attach XMLHttpRequest module to receive data from clients
+ Increase processing speed with multi-process
+ Preperations for process down issues(Supervisors, solution-checkpoints etc...)
+ Change board list's elements to {Index,Value}
