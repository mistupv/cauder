-module(race_set_tests).

-include_lib("eunit/include/eunit.hrl").

race_set_test_() ->
    R1 = {'receive', 1},

    Trace1a = #{
        1 => [{send, 1, 2}],
        2 => [{deliver, 1}, R1, {deliver, 2}, {deliver, 3}],
        3 => [{send, 2, 2}, {send, 3, 2}]
    },
    Trace1b = #{
        1 => [{send, 1, 2}],
        2 => [{deliver, 2}, {deliver, 1}, R1, {deliver, 3}],
        3 => [{send, 2, 2}, {send, 3, 2}]
    },

    R2 = {'receive', 2},

    Trace2 = #{
        1 => [
            {deliver, 5},
            {'receive', 5},
            {send, 7, 3}
        ],
        2 => [
            {send, 2, 3}
        ],
        3 => [
            {deliver, 1},
            {deliver, 2},
            {send, 3, 4},
            R2,
            {deliver, 4},
            {'receive', 4},
            {'receive', 1},
            {send, 5, 1},
            {deliver, 6},
            {'receive', 6},
            {deliver, 7},
            {deliver, 8}
        ],
        4 => [
            {deliver, 3},
            {'receive', 3},
            {send, 6, 3}
        ],
        5 => [
            {send, 1, 3},
            {send, 4, 3},
            {send, 8, 3}
        ]
    },

    [
        ?_assertEqual([[2, 3]], cauder_race_sets:race_set({2, R1}, Trace1a)),
        ?_assertEqual([[3]], cauder_race_sets:race_set({2, R1}, Trace1b)),
        ?_assertEqual([[4, 8], [6]], cauder_race_sets:race_set({3, R2}, Trace2))
    ].

race_sets_test_() ->
    R1 = {'receive', 1},

    Trace1a = #{
        1 => [{send, 1, 2}],
        2 => [{deliver, 1}, R1, {deliver, 2}, {deliver, 3}],
        3 => [{send, 2, 2}, {send, 3, 2}]
    },
    Trace1b = #{
        1 => [{send, 1, 2}],
        2 => [{deliver, 2}, {deliver, 1}, R1, {deliver, 3}],
        3 => [{send, 2, 2}, {send, 3, 2}]
    },

    R2 = {'receive', 2},

    Trace2 = #{
        1 => [
            {deliver, 5},
            {'receive', 5},
            {send, 7, 3}
        ],
        2 => [
            {send, 2, 3}
        ],
        3 => [
            {deliver, 1},
            {deliver, 2},
            {send, 3, 4},
            R2,
            {deliver, 4},
            {'receive', 4},
            {'receive', 1},
            {send, 5, 1},
            {deliver, 6},
            {'receive', 6},
            {deliver, 7},
            {deliver, 8}
        ],
        4 => [
            {deliver, 3},
            {'receive', 3},
            {send, 6, 3}
        ],
        5 => [
            {send, 1, 3},
            {send, 4, 3},
            {send, 8, 3}
        ]
    },

    [
        ?_assertEqual(#{2 => #{1 => [[2, 3]]}}, cauder_race_sets:race_sets(Trace1a)),
        ?_assertEqual(#{2 => #{1 => [[3]]}}, cauder_race_sets:race_sets(Trace1b)),
        ?_assertEqual(
            #{
                3 => #{
                    1 => [[2], [4, 8], [6]],
                    2 => [[4, 8], [6]],
                    4 => [[6], [8]],
                    6 => [[7], [8]]
                }
            },
            cauder_race_sets:race_sets(Trace2)
        )
    ].
