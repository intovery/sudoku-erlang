# SUDOKU with erlang
## from 2021-01-11

Sudoku-Solver with Erlang.

# Modules
    - util: Utilization functions to use erlang easier
    - si_read: Make list of integers(1~9) by reading .csv files in folder "./data"
    - si: Functions to analyze sudoku and comeplete it
    - parallel: Map function with multi-process

# How I used it
    1. Add .csv sudoku file into "./data" folder
    2. Read .csv binary data
    3. 'si_read:read_board({ok,is_binary()})' will gives you the list of elements in sudoku
    4. 'si:solve(<File Location + Name>)' will gives you solutions of sudoku


# Example

```
1> sudoku:solve("./data/data.csv").
{[5,3,0,0,7,0,0,0,0,6,0,0,1,9,5,0,0,0,0,9,8,0,0,0,0,6,0,8|...],
 [[5,3,4,6,7,8,9,1,2,6,7,2,1,9,5,3,4,8,1,9,8,3,4,2,5,6|...]]}
2> util:measure_time(fun() -> sudoku:solve("./data/data_hard.csv") end).
{91000,
 {[0,0,5,3,0,0,0,0,0,8,0,0,0,0,0,0,2,0,0,7,0,0,1,0,5,0|...],
  [[1,4,5,3,2,7,6,9,8,8,3,9,6,5,4,1,2,7,6,7,2,9,1,8|...]]}}
```

# Milestones

- Attach XMLHttpRequest module to receive data from clients
- Increase processing speed with multi-process
- Preperations for process down issues(Supervisors, solution-checkpoints etc...)
