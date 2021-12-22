-module(cauder_log_tests).

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-include("cauder_log.hrl").
-include_lib("eunit/include/eunit.hrl").

rdep_test_() ->
    [
        ?_assertEqual(
            #{
                2 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 2}
                ]
            },
            cauder_log:rdep(1, #{
                1 => [
                    #log_send{uid = 1},
                    #log_receive{uid = 1}
                ],
                2 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 2}
                ]
            })
        ),
        ?_assertEqual(
            #{
                3 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 2}
                ]
            },
            cauder_log:rdep(1, #{
                1 => [
                    #log_spawn{pid = 2}
                ],
                2 => [
                    #log_send{uid = 1},
                    #log_receive{uid = 1}
                ],
                3 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 2}
                ]
            })
        ),
        ?_assertEqual(
            #{
                3 => [
                    #log_send{uid = 2}
                ]
            },
            cauder_log:rdep(1, #{
                1 => [
                    #log_spawn{pid = 2}
                ],
                2 => [
                    #log_send{uid = 1},
                    #log_receive{uid = 2}
                ],
                3 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 1}
                ]
            })
        ),
        ?_assertEqual(
            #{
                2 => [
                    #log_send{uid = 2}
                ],
                3 => [
                    #log_send{uid = 3},
                    #log_receive{uid = 2}
                ]
            },
            cauder_log:rdep(1, #{
                1 => [
                    #log_send{uid = 1},
                    #log_receive{uid = 3}
                ],
                2 => [
                    #log_send{uid = 2},
                    #log_receive{uid = 1}
                ],
                3 => [
                    #log_send{uid = 3},
                    #log_receive{uid = 2}
                ]
            })
        ),
        ?_assertEqual(
            #{
                2 => [
                    #log_spawn{pid = 3},
                    #log_send{uid = 3}
                ]
            },
            cauder_log:rdep(1, #{
                1 => [
                    #log_send{uid = 1},
                    #log_receive{uid = 3}
                ],
                2 => [
                    #log_spawn{pid = 3},
                    #log_send{uid = 3},
                    #log_receive{uid = 2}
                ],
                3 => [
                    #log_receive{uid = 1},
                    #log_send{uid = 2}
                ]
            })
        )
    ].
