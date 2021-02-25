-module(si_read).
-export([read_board/1]).

delete_CRLF(L) when length(L) == 1 -> [L];
delete_CRLF(L) ->
    [[hd(L)],[lists:last(L)]].

%% eliminate "\r\n" that has been included from file:read_file/1
stabilize(L) -> 
    % util:reduce(fun(Acc,X) -> Acc ++ delete_CRLF(X) end, [], L).
    lists:foldl(fun(X,Acc) -> Acc ++ delete_CRLF(X) end, [], L).

tokenize(S) -> string:tokens(S,",").


%% convert file binary to list
read_board({ok,Bin}) ->
    L = tokenize(binary_to_list(Bin)),
    {board,[list_to_integer(X) || X <- stabilize(L)]}.
