-module(csv).
-export([read_csv/1,parser/2]).

-type row() :: list(string()).

%% convert file binary to list
-spec read_csv(string()) -> list(string()).
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
        

-spec parser(list(string()),list(row())) -> ok.
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
            