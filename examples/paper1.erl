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

%%%=============================================================================

race_set_a() ->
    TraceMap_1a = #{
        0 => [
            {spawn, {nonode@nohost, 1}, success},
            {spawn, {nonode@nohost, 2}, success},
            {spawn, {nonode@nohost, 3}, success}
        ],
        1 => [
            {send, 1}
        ],
        2 => [
            {deliver, 1},
            % Calculating ReceSet for this event
            {'receive', 1},
            {deliver, 2},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    % Should return: [2]
    cauder_utils:race_set({2, {'receive', 1}}, TraceMap_1a).

race_set_b() ->
    TraceMap_1b = #{
        0 => [
            {spawn, {nonode@nohost, 1}, success},
            {spawn, {nonode@nohost, 2}, success},
            {spawn, {nonode@nohost, 3}, success}
        ],
        1 => [
            {send, 1}
        ],
        2 => [
            {deliver, 2},
            {deliver, 1},
            % Calculating ReceSet for this event
            {'receive', 1},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    % Should return: [3]
    cauder_utils:race_set({2, {'receive', 1}}, TraceMap_1b).
