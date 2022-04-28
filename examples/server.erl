-module(server).
-export([main/1]).
%[{square, 10}, {adder, 20}, {log, 100}, {adder, 30}, {adder, 100}]

main(A) ->
    register(server, spawn(?MODULE, server, [])),
    register(log, spawn(?MODULE, logger, [0, []])),
    sendRequest(A).

server() ->
    receive
        {logged, Log} ->
            io:format("LOGGED TIME:~p\n", [Log]);
        {replay, Ris} ->
            io:format("RESULT:~p\n", [Ris]),
            log ! Ris;
        {Atom, Val} ->
            io:format("SEND REQUEST:~p\n", [{Atom, Val}]),
            case whereis(Atom) of
                undefined ->
                    register(Atom, spawn(?MODULE, Atom, [])),
                    Atom ! Val;
                _ ->
                    Atom ! Val
            end
    end,
    server().

logger(N, L) ->
    receive
        Val ->
            server ! {logged, N},
            logger(N + 1, L ++ [Val])
    end.

square() ->
    receive
        N ->
            server ! {replay, {square, N * N}},
            square()
    end.

log() ->
    receive
        N ->
            server ! {replay, {log, math:log10(N)}},
            log()
    end.

adder() -> adder(0).
adder(N) ->
    receive
        Val ->
            server ! {replay, {addder, Val + N}},
            adder(Val + N)
    end.

sendRequest([]) ->
    ok;
sendRequest([El | T]) ->
    server ! El,
    sendRequest(T).
