-module(proc).
-compile(export_all).

storage(State) ->
    receive
        {add,Data} ->
            storage([Data|State]);
        {show_status} ->
            io:format("COUNT = ~p~n",[length(State)]),
            storage(State);
        {get} ->
            io:format("~p~n",[State]),
            storage(State);
        {get,Pid} ->
            Pid ! {get,State},
            storage(State);
        terminate ->
            ok
    end.

counter(State) ->
    receive
        {increase} ->
            counter(State+1);
        {get,Pid} ->
            Pid ! {get,State},
            counter(State);
        _ ->
            io:format("???",[])
    end.

trigger(State,F_Check,F_Change,F_Activate) ->
    case F_Check(State) of
        true -> 
            F_Activate(State),
            ok;
        false ->
            receive
                {change,D} -> 
                    trigger(F_Change(State,D),F_Check,F_Change,F_Activate);
                _ -> err
            end
    end.

divided_link_manager(F_checker) ->
    process_flag(trap_exit, true),
    divided_link_manager(0,F_checker).

divided_link_manager(State,F_checker) ->
    case F_checker(State) of
        true -> 
            io:format("Manager done.~n",[]);
        false ->
            receive
                {'EXIT',Pid,normal} ->
                    io:format("EXIT normally => ~p~n",[Pid]),
                    divided_link_manager(State+1,F_checker);
                {show} ->
                    io:format("STATE => ~p~n",[State]),
                    divided_link_manager(State,F_checker);
                {link,Pid} ->
                    link(Pid),
                    io:format("LINK => ~p~n",[Pid]),
                    divided_link_manager(State,F_checker)
            end
    end.

observer(F,Pid) ->
    Pid ! {register,self()},
    observer_inner(F,Pid).
observer_inner(F,Pid) ->
    receive
        {done,Result} -> 
            F(Result),
            ok;
        {activate,D} ->
            Pid ! {change,D},
            observer_inner(F,Pid);
        _ -> 
            err
    end.
