-module(client_server).
-export([main/0, server/0, client/1]).

main() ->
    S = spawn(?MODULE, server, []),
    spawn(?MODULE, client, [S]),
    client(S).

server() ->
    receive
        {P, _} ->
            P ! ack,
            server()
    end.

client(S) ->
    S ! {self(), req},
    receive
        ack -> ok
    end.
