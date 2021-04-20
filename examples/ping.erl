-module(ping).
-export([main/0, ping/1, pong/0]).

main() ->
    Pong = spawn(ping, pong, []),
    self() ! 3,
    ping(Pong).

ping(Pong) ->
    receive
        kill ->
            done;
        N ->
            Pong ! {self(), N - 1},
            ping(Pong)
    end.

pong() ->
    receive
        {S, 0} ->
            S ! kill;
        {S, N} when N > 0 ->
            S ! N,
            pong()
    end.
