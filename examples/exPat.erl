-module(exPat).
-export([ack/2, main/0]).

main() ->
    self() ! {1, 2},
    ack(1, 2).

ack(N, M) ->
    receive
        {N, M} ->
            true;
        _ ->
            false
    end.
