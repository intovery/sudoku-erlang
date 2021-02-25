%%%-------------------------------------------------------------------
%% @doc sudoku public API
%% @end
%%%-------------------------------------------------------------------

-module(sudoku_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sudoku_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
