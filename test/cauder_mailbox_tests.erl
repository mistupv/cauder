-module(cauder_mailbox_tests).

-import(cauder_mailbox, [new/0, add/2, delete/2, pid_get/2, uid_take/2, uid_member/2, to_list/1]).

-include("cauder.hrl").
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    M1 = #message{uid = 1, value = "A", src = 1, dest = 1},
    M2 = #message{uid = 2, value = "B", src = 1, dest = 1},

    [
        ?_assertNot(uid_member(1, new())),
        ?_assertNot(uid_member(2, new())),

        ?_assert(uid_member(1, add(M1, new()))),
        ?_assertNot(uid_member(1, add(M2, new()))),

        ?_assert(uid_member(1, add(M2, add(M1, new())))),
        ?_assert(uid_member(2, add(M2, add(M1, new())))),
        ?_assertNot(uid_member(3, add(M2, add(M1, new())))),

        ?_assertError({existing_uid, 1}, add(M1, add(M1, new()))),
        ?_assertError({existing_uid, 1}, add(M1, add(M2, add(M1, new()))))
    ].

delete_test_() ->
    M1 = #message{uid = 1, value = "A", src = 1, dest = 1},
    M2 = #message{uid = 2, value = "B", src = 1, dest = 1},

    [
        ?_assertError({badkey, 1}, delete(M1, new())),
        ?_assertNot(uid_member(1, element(2, delete(M1, add(M1, new()))))),
        ?_assertError({badkey, 1}, delete(M1, delete(M1, new()))),

        ?_assertNot(uid_member(1, element(2, delete(M1, add(M2, add(M1, new())))))),
        ?_assert(uid_member(1, element(2, delete(M2, add(M2, add(M1, new())))))),

        ?_assert(uid_member(2, element(2, delete(M1, add(M2, add(M1, new())))))),
        ?_assertNot(uid_member(2, element(2, delete(M2, add(M2, add(M1, new()))))))
    ].

pid_get_test_() ->
    M1 = #message{uid = 1, value = "A", src = 1, dest = 3},
    M2 = #message{uid = 3, value = "B", src = 1, dest = 3},
    M3 = #message{uid = 2, value = "C", src = 2, dest = 3},
    M4 = #message{uid = 4, value = "D", src = 2, dest = 1},
    M5 = #message{uid = 6, value = "E", src = 3, dest = 1},
    M6 = #message{uid = 5, value = "F", src = 3, dest = 2},

    Mail = add(M6, add(M5, add(M4, add(M3, add(M2, add(M1, new())))))),

    List1 = lists:map(fun queue:to_list/1, pid_get(1, Mail)),
    List2 = lists:map(fun queue:to_list/1, pid_get(2, Mail)),
    List3 = lists:map(fun queue:to_list/1, pid_get(3, Mail)),
    List4 = lists:map(fun queue:to_list/1, pid_get(4, Mail)),

    [
        ?_assertEqual([[M4], [M5]], List1),
        ?_assertEqual([[M6]], List2),
        ?_assertEqual([[M1, M2], [M3]], List3),
        ?_assertEqual([], List4)
    ].

uid_take_test_() ->
    M1 = #message{uid = 1, value = "A", src = 1, dest = 2},
    M2 = #message{uid = 2, value = "B", src = 1, dest = 3},
    M3 = #message{uid = 3, value = "C", src = 1, dest = 2},

    Mail = add(M3, add(M2, add(M1, new()))),

    Uid1 = M1#message.uid,
    Uid2 = M2#message.uid,
    Uid3 = M3#message.uid,

    Tuple1 = uid_take(Uid1, Mail),
    Tuple2 = uid_take(Uid2, Mail),
    Tuple3 = uid_take(Uid3, Mail),

    [
        ?_assertMatch({value, {M1, 1}, _}, Tuple1),
        ?_assertMatch({value, {M2, 1}, _}, Tuple2),
        ?_assertMatch({value, {M3, 2}, _}, Tuple3),
        ?_assertEqual(sets:from_list([M2, M3]), sets:from_list(to_list(element(3, Tuple1)))),
        ?_assertEqual(sets:from_list([M1, M3]), sets:from_list(to_list(element(3, Tuple2)))),
        ?_assertEqual(sets:from_list([M1, M2]), sets:from_list(to_list(element(3, Tuple3)))),
        ?_assertNot(uid_take(4, Mail)),
        ?_assertNot(uid_take(5, Mail)),
        ?_assertNot(uid_take(6, Mail))
    ].
