-module(parallel).
-export([map/2, failer/1]).

map(F,L) ->
    Parent = self(),
    Processes = lists:map(fun(X) ->
        spawn_link(fun() -> Parent ! {self(),F(X)} end)
    end,L),
    lists:map(fun(Pid) ->
        receive
            {Pid,Result} -> Result
        end
    end,Processes).

process_end_action({'EXIT',Pid,normal}) ->
    receive
        {Pid,Result} -> Result
    end;
process_end_action({'EXIT',Pid,Reason}) ->
    io:format("== PROCESS SHUTDOWN] ==~n~p~n",[Reason]).

func(L) ->
    lists:map(fun(F) ->
        Pid = spawn_link(fun() -> self() ! F()),
        receive
            {'EXIT',Pid} -> process_end_action(Result)
        end
    end, L)

func_1(L) ->
