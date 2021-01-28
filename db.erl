-module(db).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    insert/2,
    lookup/1,
    lookup/2]).
%% 
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

insert(Pid,D) ->
    gen_server:call(Pid, {insert,D}).
lookup(Pid) ->
    gen_server:call(Pid, {lookup}).
lookup(Pid,Key) ->
    gen_server:call(Pid, {lookup,Key}).

%% gen_server callbacks
init([]) ->
    {ok, ets:new(?MODULE,[set])}.

handle_call({insert,D},_From,State) ->
    ets:insert(State,D),
    {reply,D,State};
handle_call({lookup},_From,State) ->
    Reply = ets:tab2list(State),
    {reply,Reply,State};
handle_call({lookup,Key},_From,State) ->
    Reply = ets:lookup(State,Key),
    {reply,Reply,State}.

handle_cast(_Msg,State) ->
    {noreply,State}.

handle_info(_Info,State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
