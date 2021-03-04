-module(parallel).
-export([map/2]).

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
