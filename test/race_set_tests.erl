-module(race_set_tests).

-include_lib("eunit/include/eunit.hrl").

%% API
-export([]).

race_set_test_() ->
    Trace1a = #{
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
            % Calculating race set for this event
            E1a = {'receive', 1},
            {deliver, 2},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    Trace1b = #{
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
            % Calculating race set for this event
            E1b = {'receive', 1},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    Trace2 = #{
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
            % Calculating race set for this event
            E2 = {'receive', 2},
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
    [
        ?_assertEqual([2], cauder_utils:race_set({2, E1a}, Trace1a)),
        ?_assertEqual([3], cauder_utils:race_set({2, E1b}, Trace1b)),
        ?_assertEqual([4, 6], cauder_utils:race_set({3, E2}, Trace2))
    ].

race_sets_test_() ->
    Trace1a = #{
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
            {'receive', 1},
            {deliver, 2},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    Trace1b = #{
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
            {'receive', 1},
            {deliver, 3}
        ],
        3 => [
            {send, 2},
            {send, 3}
        ]
    },
    Trace2 = #{
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
    [
        ?_assertEqual(#{2 => #{1 => [2]}}, cauder_utils:race_sets(Trace1a)),
        ?_assertEqual(#{2 => #{1 => [3]}}, cauder_utils:race_sets(Trace1b)),
        ?_assertEqual(#{3 => #{1 => [2, 4, 6], 2 => [4, 6], 4 => [6, 8], 6 => [7, 8]}}, cauder_utils:race_sets(Trace2))
    ].
