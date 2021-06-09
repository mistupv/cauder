-module(paper1).

-export([main/0]).
-export([p1/1, p2/0, p3/1]).

main() ->
    spawn(?MODULE, p1, [self()]),
    spawn(?MODULE, p3, [self()]),
    p2(),
    % Necessary to ensure delivery of l2 and l3
    receive
        l2 -> ok
    end,
    receive
        l3 -> ok
    end.

p1(P2) ->
    P2 ! l1.

p2() ->
    receive
        l1 -> ok
    end.

p3(P2) ->
    P2 ! l2,
    P2 ! l3.
