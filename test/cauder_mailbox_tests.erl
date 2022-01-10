-module(cauder_mailbox_tests).

-elvis([{elvis_style, dont_repeat_yourself, disable}]).

-include("cauder.hrl").
-include_lib("eunit/include/eunit.hrl").

create_mailbox() -> cauder_mailbox:new().
create_mailbox(Messages) -> lists:foldl(fun cauder_mailbox:add/2, cauder_mailbox:new(), Messages).

add_test_() ->
    M1 = #message{uid = 1, val = "A", src = 1, dst = 1},
    M2 = #message{uid = 2, val = "B", src = 1, dst = 1},

    [
        ?_assertNot(cauder_mailbox:is_element(1, create_mailbox())),
        ?_assertNot(cauder_mailbox:is_element(2, create_mailbox())),

        ?_assert(cauder_mailbox:is_element(1, create_mailbox([M1]))),
        ?_assertNot(cauder_mailbox:is_element(1, create_mailbox([M2]))),

        ?_assert(cauder_mailbox:is_element(1, create_mailbox([M1, M2]))),
        ?_assert(cauder_mailbox:is_element(2, create_mailbox([M1, M2]))),
        ?_assertNot(cauder_mailbox:is_element(3, create_mailbox([M1, M2]))),

        ?_assertError({existing_uid, 1}, create_mailbox([M1, M1])),
        ?_assertError({existing_uid, 1}, create_mailbox([M1, M2, M1]))
    ].

delete_test_() ->
    M1 = #message{uid = 1, val = "A", src = 1, dst = 1},
    M2 = #message{uid = 2, val = "B", src = 1, dst = 1},

    [
        ?_assertError({badkey, 1}, cauder_mailbox:remove(M1, create_mailbox())),
        ?_assertNot(cauder_mailbox:is_element(1, cauder_mailbox:remove(M1, create_mailbox([M1])))),
        ?_assertError({badkey, 1}, cauder_mailbox:remove(M1, cauder_mailbox:remove(M1, create_mailbox()))),

        ?_assertNot(cauder_mailbox:is_element(1, cauder_mailbox:remove(M1, create_mailbox([M1, M2])))),
        ?_assert(cauder_mailbox:is_element(1, cauder_mailbox:remove(M2, create_mailbox([M1, M2])))),

        ?_assert(cauder_mailbox:is_element(2, cauder_mailbox:remove(M1, create_mailbox([M1, M2])))),
        ?_assertNot(cauder_mailbox:is_element(2, cauder_mailbox:remove(M2, create_mailbox([M1, M2]))))
    ].

pid_get_test_() ->
    M1 = #message{uid = 1, val = "A", src = 1, dst = 3},
    M2 = #message{uid = 3, val = "B", src = 1, dst = 3},
    M3 = #message{uid = 2, val = "C", src = 2, dst = 3},
    M4 = #message{uid = 4, val = "D", src = 2, dst = 1},
    M5 = #message{uid = 6, val = "E", src = 3, dst = 1},
    M6 = #message{uid = 5, val = "F", src = 3, dst = 2},

    Mail = create_mailbox([M1, M2, M3, M4, M5, M6]),

    List1 = lists:map(fun queue:to_list/1, cauder_mailbox:pid_get(1, Mail)),
    List2 = lists:map(fun queue:to_list/1, cauder_mailbox:pid_get(2, Mail)),
    List3 = lists:map(fun queue:to_list/1, cauder_mailbox:pid_get(3, Mail)),
    List4 = lists:map(fun queue:to_list/1, cauder_mailbox:pid_get(4, Mail)),

    [
        ?_assertEqual([[M4], [M5]], List1),
        ?_assertEqual([[M6]], List2),
        ?_assertEqual([[M1, M2], [M3]], List3),
        ?_assertEqual([], List4)
    ].

uid_take_test_() ->
    M1 = #message{uid = 1, val = "A", src = 1, dst = 2},
    M2 = #message{uid = 2, val = "B", src = 1, dst = 3},
    M3 = #message{uid = 3, val = "C", src = 1, dst = 2},

    Mail = create_mailbox([M1, M2, M3]),

    Uid1 = M1#message.uid,
    Uid2 = M2#message.uid,
    Uid3 = M3#message.uid,

    {M1Out, Mail1} = cauder_mailbox:take(Uid1, Mail),
    {M2Out, Mail2} = cauder_mailbox:take(Uid2, Mail),
    {M3Out, Mail3} = cauder_mailbox:take(Uid3, Mail),

    [
        ?_assertEqual(M1, M1Out),
        ?_assertEqual(M2, M2Out),
        ?_assertEqual(M3, M3Out),
        ?_assertEqual(sets:from_list([M2, M3]), sets:from_list(cauder_mailbox:to_list(Mail1))),
        ?_assertEqual(sets:from_list([M1, M3]), sets:from_list(cauder_mailbox:to_list(Mail2))),
        ?_assertEqual(sets:from_list([M1, M2]), sets:from_list(cauder_mailbox:to_list(Mail3))),
        ?_assertNot(cauder_mailbox:is_element(1, Mail1)),
        ?_assertNot(cauder_mailbox:is_element(2, Mail2)),
        ?_assertNot(cauder_mailbox:is_element(3, Mail3))
    ].
