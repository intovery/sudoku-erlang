-module(csv).
-export([read_csv/1,parser/2]).

%% convert file binary to list
read_csv(Name) ->
    {ok,Bin} = file:read_file(Name),
    L = string:tokens(binary_to_list(Bin),","),
    Storage = spawn(csv,parser,[[],[]]),
    lists:foreach(fun
        ([Last,$\r,$\n,Start]) ->
            Storage ! {add,[Last]},
            Storage ! {newline},
            Storage ! {add,[Start]};
        (Token) ->
            Storage ! {add,Token}
    end,L),
    Storage ! {return,self()},
    receive
        {result,Result} -> Result;
        _ -> error
    end.
        

parser(RowAcc,Stored) ->
    receive
        {add,Value} ->
            parser(RowAcc ++ [Value],Stored);
        {newline} ->
            parser([],[RowAcc|Stored]);
        {return,Pid} ->
            Pid ! {result,lists:reverse([RowAcc|Stored])},
            ok
    end.
            