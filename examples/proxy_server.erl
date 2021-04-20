-module(proxy_server).
-export([main/0, server/0, proxy/1]).

main() ->
    S = spawn(?MODULE, server, []),
    P = spawn(?MODULE, proxy, [S]),
    P ! {self(), req},
    receive
        result -> ok
    end.

proxy(SPid) ->
    receive
        {CPid, req} ->
            SPid ! {self(), req},
            receive
                result -> CPid ! result
            end
    end.

server() ->
    receive
        {PPid, req} ->
            PPid ! result
    end.
