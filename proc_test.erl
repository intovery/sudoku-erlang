-module(proc_test).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    calculate/2,
    calculate/3,
    testing/1]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

%% API
start_link() ->
    gen_server:start_link(?MODULE,[],[]).

calculate(Pid,N) ->
    gen_server:call(Pid, {calculate,N}).
calculate(Pid,N,From) ->
    gen_server:call(Pid, {calculate,N,From}).
testing(Pid) ->
    gen_server:call(Pid, {testing}).

factorial(1) ->
    1;
factorial(N) when N > 1 ->
    N * factorial(N-1).

%% gen_Server
init([]) ->
    {ok,[]}.

handle_call({calculate,N},_From,State) ->
    Result = factorial(N),
    {reply,Result,State};
handle_call({calculate,N,Client},_From,State) ->
    gen_server:cast(self(),{calculate,{1,1,N,Client}}),
    {reply,ok,State};
handle_call({testing},_From,State) ->
    {reply,hello,State}.

handle_cast({calculate,{Acc,Cur,Last,From}},State) ->
    case Cur == (Last+1) of
        true    -> gen_server:cast(self(),{complete,{Acc,From}});
        false   -> gen_server:cast(self(),{calculate,{Acc*Cur,Cur+1,Last,From}})
    end,
    {noreply,State};
handle_cast({complete,{Result,From}},State) ->
    %gen_server:cast(From,{complete,Result}),
    From ! {complete,Result},
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok,State}.