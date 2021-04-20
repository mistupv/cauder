-module(cauder_scheduler_tests).

-import(cauder_scheduler, [scheduler_round_robin/2, scheduler_fcfs/2]).

-include_lib("eunit/include/eunit.hrl").

scheduler_round_robin_test_() ->
    Q0 = queue:new(),
    Q1 = queue:from_list([a, b, c, d]),
    [
        assert_scheduling(a, [b, c, d, a], scheduler_round_robin(Q0, {init, [a, b, c, d]})),
        ?_assertError(empty, scheduler_round_robin(Q0, none)),
        ?_assertError(empty, scheduler_round_robin(Q0, {add, a})),
        ?_assertError(empty, scheduler_round_robin(Q0, {remove, a})),
        ?_assertError(empty, scheduler_round_robin(Q0, {update, b, a})),

        ?_assertError(not_empty, scheduler_round_robin(Q1, {init, [a, b, c, d]})),
        assert_scheduling(a, [b, c, d, a], scheduler_round_robin(Q1, none)),
        assert_scheduling(a, [b, c, d, e, a], scheduler_round_robin(Q1, {add, e})),
        assert_scheduling(a, [b, c, a], scheduler_round_robin(Q1, {remove, d})),
        ?_assertError(not_tail, scheduler_round_robin(Q1, {remove, a})),
        assert_scheduling(a, [b, c, x, a], scheduler_round_robin(Q1, {update, x, d})),

        assert_scheduling(b, [b], scheduler_round_robin(queue:from_list([a]), {update, b, a}))
    ].

scheduler_fcfs_test_() ->
    Q0 = queue:new(),
    Q1 = queue:from_list([a, b, c, d]),
    [
        assert_scheduling(a, [a, b, c, d], scheduler_fcfs(Q0, {init, [a, b, c, d]})),
        ?_assertError(empty, scheduler_fcfs(Q0, none)),
        ?_assertError(empty, scheduler_fcfs(Q0, {add, a})),
        ?_assertError(empty, scheduler_fcfs(Q0, {remove, a})),
        ?_assertError(empty, scheduler_fcfs(Q0, {update, b, a})),

        ?_assertError(not_empty, scheduler_fcfs(Q1, {init, [a, b, c, d]})),
        assert_scheduling(a, [a, b, c, d], scheduler_fcfs(Q1, none)),
        assert_scheduling(a, [a, b, c, d, e], scheduler_fcfs(Q1, {add, e})),
        ?_assertError(not_head, scheduler_fcfs(Q1, {remove, d})),
        assert_scheduling(b, [b, c, d], scheduler_fcfs(Q1, {remove, a})),
        assert_scheduling(b, [b, c, d, x], scheduler_fcfs(Q1, {update, x, a})),

        assert_scheduling(b, [b], scheduler_fcfs(queue:from_list([a]), {update, b, a}))
    ].

assert_scheduling(ExpectItem, ExpectList, Expr) ->
    {Item, Queue} = Expr,
    [?_assertEqual(ExpectItem, Item), ?_assertEqual(ExpectList, queue:to_list(Queue))].
