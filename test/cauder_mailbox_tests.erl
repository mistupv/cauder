-module(cauder_mailbox_tests).

-import(cauder_mailbox, [new/0, add/2, remove/2, find_destination/2, take/2, is_element/2, to_list/1]).

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-include("cauder_message.hrl").
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
    M1 = #message{uid = 1, src = 1, dst = 1, val = "A"},
    M2 = #message{uid = 2, src = 1, dst = 1, val = "B"},

    [
        ?_assertNot(is_element(1, new())),
        ?_assertNot(is_element(2, new())),

        ?_assert(is_element(1, add(M1, new()))),
        ?_assertNot(is_element(1, add(M2, new()))),

        ?_assert(is_element(1, add(M2, add(M1, new())))),
        ?_assert(is_element(2, add(M2, add(M1, new())))),
        ?_assertNot(is_element(3, add(M2, add(M1, new())))),

        ?_assertError({existing_uid, 1}, add(M1, add(M1, new()))),
        ?_assertError({existing_uid, 1}, add(M1, add(M2, add(M1, new()))))
    ].

delete_test_() ->
    M1 = #message{uid = 1, src = 1, dst = 1, val = "A"},
    M2 = #message{uid = 2, src = 1, dst = 1, val = "B"},

    [
        ?_assertError({badkey, 1}, remove(M1, new())),
        ?_assertNot(is_element(1, element(2, remove(M1, add(M1, new()))))),
        ?_assertError({badkey, 1}, remove(M1, remove(M1, new()))),

        ?_assertNot(is_element(1, element(2, remove(M1, add(M2, add(M1, new())))))),
        ?_assert(is_element(1, element(2, remove(M2, add(M2, add(M1, new())))))),

        ?_assert(is_element(2, element(2, remove(M1, add(M2, add(M1, new())))))),
        ?_assertNot(is_element(2, element(2, remove(M2, add(M2, add(M1, new()))))))
    ].

pid_get_test_() ->
    M1 = #message{uid = 1, src = 1, dst = 3, val = "A"},
    M2 = #message{uid = 3, src = 1, dst = 3, val = "B"},
    M3 = #message{uid = 2, src = 2, dst = 3, val = "C"},
    M4 = #message{uid = 4, src = 2, dst = 1, val = "D"},
    M5 = #message{uid = 6, src = 3, dst = 1, val = "E"},
    M6 = #message{uid = 5, src = 3, dst = 2, val = "F"},

    Mail = add(M6, add(M5, add(M4, add(M3, add(M2, add(M1, new())))))),

    List1 = lists:map(fun queue:to_list/1, find_destination(1, Mail)),
    List2 = lists:map(fun queue:to_list/1, find_destination(2, Mail)),
    List3 = lists:map(fun queue:to_list/1, find_destination(3, Mail)),
    List4 = lists:map(fun queue:to_list/1, find_destination(4, Mail)),

    [
        ?_assertEqual([[M4], [M5]], List1),
        ?_assertEqual([[M6]], List2),
        ?_assertEqual([[M1, M2], [M3]], List3),
        ?_assertEqual([], List4)
    ].

uid_take_test_() ->
    M1 = #message{uid = 1, src = 1, dst = 2, val = "A"},
    M2 = #message{uid = 2, src = 1, dst = 3, val = "B"},
    M3 = #message{uid = 3, src = 1, dst = 2, val = "C"},

    Mail = add(M3, add(M2, add(M1, new()))),

    Uid1 = M1#message.uid,
    Uid2 = M2#message.uid,
    Uid3 = M3#message.uid,

    Tuple1 = take(Uid1, Mail),
    Tuple2 = take(Uid2, Mail),
    Tuple3 = take(Uid3, Mail),

    [
        ?_assertMatch({{M1, 1}, _}, Tuple1),
        ?_assertMatch({{M2, 1}, _}, Tuple2),
        ?_assertMatch({{M3, 2}, _}, Tuple3),
        ?_assertEqual(sets:from_list([M2, M3]), sets:from_list(to_list(element(2, Tuple1)))),
        ?_assertEqual(sets:from_list([M1, M3]), sets:from_list(to_list(element(2, Tuple2)))),
        ?_assertEqual(sets:from_list([M1, M2]), sets:from_list(to_list(element(2, Tuple3)))),
        ?_assertEqual(error, take(4, Mail)),
        ?_assertEqual(error, take(5, Mail)),
        ?_assertEqual(error, take(6, Mail))
    ].
