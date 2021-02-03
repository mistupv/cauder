-module(cauder_scheduler_tests).

-import(cauder_scheduler, [scheduler_round_robin/2, scheduler_fcfs/2, scheduler_manual/2]).

-include_lib("eunit/include/eunit.hrl").


-define(_assertScheduling(ExpectItem, ExpectList, Expr),
  fun() -> {Item, Queue} = Expr, [?_assertEqual(ExpectItem, Item), ?_assertEqual(ExpectList, queue:to_list(Queue))] end).


scheduler_round_robin_test_() ->
  Q0 = queue:new(),
  Q1 = queue:from_list([a, b, c, d]),
  [
    ?_assertScheduling(a, [b, c, d, a], scheduler_round_robin(Q0, {init, [a, b, c, d]})),
    ?_assertError(empty, scheduler_round_robin(Q0, none)),
    ?_assertError(empty, scheduler_round_robin(Q0, {add, a})),
    ?_assertError(empty, scheduler_round_robin(Q0, {remove, a})),

    ?_assertError(not_empty, scheduler_round_robin(Q1, {init, [a, b, c, d]})),
    ?_assertScheduling(a, [b, c, d, a], scheduler_round_robin(Q1, none)),
    ?_assertScheduling(a, [b, c, d, e, a], scheduler_round_robin(Q1, {add, e})),
    ?_assertScheduling(a, [b, c, a], scheduler_round_robin(Q1, {remove, d})),
    ?_assertError(not_tail, scheduler_round_robin(Q1, {remove, a}))
  ].


scheduler_fcfs_test_() ->
  Q0 = queue:new(),
  Q1 = queue:from_list([a, b, c, d]),
  [
    ?_assertScheduling(a, [a, b, c, d], scheduler_fcfs(Q0, {init, [a, b, c, d]})),
    ?_assertError(empty, scheduler_fcfs(Q0, none)),
    ?_assertError(empty, scheduler_fcfs(Q0, {add, a})),
    ?_assertError(empty, scheduler_fcfs(Q0, {remove, a})),

    ?_assertError(not_empty, scheduler_fcfs(Q1, {init, [a, b, c, d]})),
    ?_assertScheduling(a, [a, b, c, d], scheduler_fcfs(Q1, none)),
    ?_assertScheduling(a, [a, b, c, d, e], scheduler_fcfs(Q1, {add, e})),
    ?_assertError(not_head, scheduler_fcfs(Q1, {remove, d})),
    ?_assertScheduling(b, [b, c, d], scheduler_fcfs(Q1, {remove, a}))
  ].
