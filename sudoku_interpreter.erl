-module(sudoku_interpreter).
-behaviour(gen_server).
-author("into_very").

%% interface: sudoku_interpreter
-export([start_link/0, stop/0]).
-export([add/1, get/0]).
%% interface: gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2
    ,terminate/2, code_change/3]).
-record(state, {}).

%% interface: sudoku_interpreter
start_link(Client) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [],[]).
stop() ->
    gen_server:call(?MODULE, stop).

request(Pid,Board,F) ->
    gen_server:cast(Pid, {request,Board,F}).

%% =====================
%% interface: gen_server

init([]) -> {ok, []}.

handle_call({request,Board,F}, _From, Client) ->
    Rows = lists:map(sudoku:getter_row(Board),util:range(0,9)),
    %% fill rows with all possible combinations of missing numbers.
    FilledRows = sudoku:match_row_elems(Rows,
        lists:map(fun(R) -> util:perms(sudoku:missings(R)) end, Rows)),
    %% divide FilledRows to conquer with several Builder processes



    {reply,L,Client}.
    
handle_cast({solve,{Rows,Missings,Client}},Client) ->
    % Fill rows with missings
    FilledRows = [],
    Divider = fun(X) -> X end,
    Divided = lists:map(Divider, FilledRows),
    
    {noreply,Client};
handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn,State,_Extra) ->
    {ok, State}.