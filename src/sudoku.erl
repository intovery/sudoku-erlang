-module(sudoku).

-export([start/0]).

start() ->
    si:solve("./data/data_easy.csv").