-module(paper2).

-export([main/0]).
-export([p1/1, p2/1, p3/2, p4/1, p5/1]).

main() ->
    P1 = spawn(?MODULE, p1, [self()]),
    spawn(?MODULE, p2, [self()]),
    P4 = spawn(?MODULE, p4, [self()]),
    spawn(?MODULE, p5, [self()]),
    p3(P1, P4),
    % Necessary to ensure delivery of l7 and l8
    receive
        l7 -> ok
    end,
    receive
        l8 -> ok
    end.

p1(P3) ->
    receive
        l5 -> ok
    end,
    P3 ! l7.

p2(P3) ->
    P3 ! l2.

p3(P1, P4) ->
    P4 ! l3,
    receive
        l2 -> ok
    end,
    receive
        l4 -> ok
    end,
    receive
        l1 -> ok
    end,
    P1 ! l5,
    receive
        l6 -> ok
    end.

p4(P3) ->
    receive
        l3 -> ok
    end,
    P3 ! l6.

p5(P3) ->
    P3 ! l1,
    P3 ! l4,
    P3 ! l8.

%%%=============================================================================

race_set() ->
    TraceMap_2 = #{
        0 => [
            {spawn, {nonode@nohost, 1}, success},
            {spawn, {nonode@nohost, 2}, success},
            {spawn, {nonode@nohost, 3}, success},
            {spawn, {nonode@nohost, 4}, success},
            {spawn, {nonode@nohost, 5}, success}
        ],
        1 => [
            {deliver, 5},
            {'receive', 5},
            {send, 7}
        ],
        2 => [
            {send, 2}
        ],
        3 => [
            {deliver, 1},
            {deliver, 2},
            {send, 3},
            % Calculating ReceSet for this event
            {'receive', 2},
            {deliver, 4},
            {'receive', 4},
            {'receive', 1},
            {send, 5},
            {deliver, 6},
            {'receive', 6},
            {deliver, 7},
            {deliver, 8}
        ],
        4 => [
            {deliver, 3},
            {'receive', 3},
            {send, 6}
        ],
        5 => [
            {send, 1},
            {send, 4},
            {send, 8}
        ]
    },
    % Should return: [4,6]
    cauder_utils:race_set({3, {'receive', 2}}, TraceMap_2).
