-module(cauder_log_tests).

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-include_lib("eunit/include/eunit.hrl").

rdep_test_() ->
    [
        ?_assertEqual(
            #{
                2 => [{send, 2}, {'receive', 2}]
            },
            cauder_log:rdep(1, #{
                1 => [{send, 1}, {'receive', 1}],
                2 => [{send, 2}, {'receive', 2}]
            })
        ),
        ?_assertEqual(
            #{
                3 => [{send, 2}, {'receive', 2}]
            },
            cauder_log:rdep(1, #{
                1 => [{spawn, 2}],
                2 => [{send, 1}, {'receive', 1}],
                3 => [{send, 2}, {'receive', 2}]
            })
        ),
        ?_assertEqual(
            #{
                3 => [{send, 2}]
            },
            cauder_log:rdep(1, #{
                1 => [{spawn, 2}],
                2 => [{send, 1}, {'receive', 2}],
                3 => [{send, 2}, {'receive', 1}]
            })
        ),
        ?_assertEqual(
            #{
                2 => [{send, 2}],
                3 => [{send, 3}, {'receive', 2}]
            },
            cauder_log:rdep(1, #{
                1 => [{send, 1}, {'receive', 3}],
                2 => [{send, 2}, {'receive', 1}],
                3 => [{send, 3}, {'receive', 2}]
            })
        ),
        ?_assertEqual(
            #{
                2 => [{spawn, 3}, {send, 3}]
            },
            cauder_log:rdep(1, #{
                1 => [{send, 1}, {'receive', 3}],
                2 => [{spawn, 3}, {send, 3}, {'receive', 2}],
                3 => [{'receive', 1}, {send, 2}]
            })
        )
    ].
