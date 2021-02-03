-module(lib_misc).
-compile(export_all).

on_exit(Pid,Fun) ->
    spawn(fun() ->
        process_flag(trap_exit, true),
        link(Pid),
        receive
            {'EXIT',P,normal} ->
                io:format("normally shut down.~n",[]);
            {'EXIT', P, Why} ->
                Fun(P,Why)
        end
    end).
