-module(cauder_mailbox_tests).

-import(cauder_mailbox, [new/0, add/2, delete/2, pid_get/2, uid_take/2, uid_member/2, to_list/1]).

-include("cauder.hrl").
-include_lib("eunit/include/eunit.hrl").

add_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},

  Mail0 = new(),
  Mail1 = add(M1, Mail0),
  Mail2 = add(M2, Mail1),
  Mail3 = add(M3, Mail2),

  Uid1 = M1#message.uid,
  Uid2 = M2#message.uid,
  Uid3 = M3#message.uid,

  [
    ?_assertEqual([], to_list(Mail0)),
    ?_assertEqual([M1], to_list(Mail1)),
    ?_assertError({existing_uid, Uid1}, add(M1, Mail1)),
    ?_assertEqual([M1, M2], to_list(Mail2)),
    ?_assertError({existing_uid, Uid2}, add(M2, Mail2)),
    ?_assertEqual([M1, M2, M3], to_list(Mail3)),
    ?_assertError({existing_uid, Uid3}, add(M3, Mail3)),
    ?_assertError({existing_uid, Uid2}, add(M2, Mail3)),
    ?_assertError({existing_uid, Uid1}, add(M1, Mail3))
  ].


delete_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},

  Mail0 = add(M3, add(M2, add(M1, new()))),
  Mail1 = delete(M2, Mail0),
  Mail2 = delete(M3, Mail1),
  Mail3 = delete(M1, Mail2),

  [
    ?_assertEqual([M1, M2, M3], to_list(Mail0)),
    ?_assertEqual([M1, M3], to_list(Mail1)),
    ?_assertEqual([M1, M3], to_list(delete(M2, Mail1))),
    ?_assertEqual([M1], to_list(Mail2)),
    ?_assertEqual([M1], to_list(delete(M2, Mail2))),
    ?_assertEqual([M1], to_list(delete(M3, Mail2))),
    ?_assertEqual([], to_list(Mail3)),
    ?_assertEqual([], to_list(delete(M1, Mail3))),
    ?_assertEqual([], to_list(delete(M2, Mail3))),
    ?_assertEqual([], to_list(delete(M3, Mail3)))
  ].


pid_get_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},
  M4 = #message{uid = 4, value = "fourth", dest = 1},
  M5 = #message{uid = 6, value = "fifth", dest = 2},
  M6 = #message{uid = 5, value = "sixth", dest = 0},

  Mail = add(M6, add(M5, add(M4, add(M3, add(M2, add(M1, new())))))),

  List0 = pid_get(0, Mail),
  List1 = pid_get(1, Mail),
  List2 = pid_get(2, Mail),
  List3 = pid_get(3, Mail),

  [
    ?_assertEqual([M1, M3, M6], List0),
    ?_assertEqual([M2, M4], List1),
    ?_assertEqual([M5], List2),
    ?_assertEqual([], List3)
  ].


uid_member_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},

  Mail = add(M3, add(M2, add(M1, new()))),

  Uid1 = M1#message.uid,
  Uid2 = M2#message.uid,
  Uid3 = M3#message.uid,

  [
    ?_assert(uid_member(Uid1, Mail)),
    ?_assert(uid_member(Uid2, Mail)),
    ?_assert(uid_member(Uid3, Mail)),
    ?_assertNot(uid_member(4, Mail)),
    ?_assertNot(uid_member(5, Mail)),
    ?_assertNot(uid_member(6, Mail))
  ].


uid_take_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},

  Mail = add(M3, add(M2, add(M1, new()))),

  Uid1 = M1#message.uid,
  Uid2 = M2#message.uid,
  Uid3 = M3#message.uid,

  Tuple1 = uid_take(Uid1, Mail),
  Tuple2 = uid_take(Uid2, Mail),
  Tuple3 = uid_take(Uid3, Mail),

  [
    ?_assertEqual({value, M1, [M2, M3]}, setelement(3, Tuple1, to_list(element(3, Tuple1)))),
    ?_assertEqual({value, M2, [M1, M3]}, setelement(3, Tuple2, to_list(element(3, Tuple2)))),
    ?_assertEqual({value, M3, [M1, M2]}, setelement(3, Tuple3, to_list(element(3, Tuple3)))),
    ?_assertNot(uid_take(4, Mail)),
    ?_assertNot(uid_take(5, Mail)),
    ?_assertNot(uid_take(6, Mail))
  ].


to_list_test_() ->
  M1 = #message{uid = 1, value = "first", dest = 0},
  M2 = #message{uid = 3, value = "second", dest = 1},
  M3 = #message{uid = 2, value = "third", dest = 0},
  M4 = #message{uid = 4, value = "fourth", dest = 1},
  M5 = #message{uid = 6, value = "fifth", dest = 2},
  M6 = #message{uid = 5, value = "sixth", dest = 0},
  M7 = #message{uid = 7, value = "seventh", dest = 3},
  M8 = #message{uid = 8, value = "eigth", dest = 2},
  M9 = #message{uid = 9, value = "ninth", dest = 0},
  M10 = #message{uid = 10, value = "tenth", dest = 1},

  Mail1 = add(M5, add(M4, add(M3, add(M2, add(M1, new()))))),
  Mail2 = add(M10, add(M9, add(M8, add(M7, add(M6, new()))))),
  Mail3 = add(M9, add(M7, add(M5, add(M3, add(M1, new()))))),
  Mail4 = add(M10, add(M8, add(M6, add(M4, add(M2, new()))))),
  Mail5 = add(M10, add(M9, add(M8, add(M7, add(M6, add(M5, add(M4, add(M3, add(M2, add(M1, new())))))))))),

  [
    ?_assertEqual([M1, M2, M3, M4, M5], to_list(Mail1)),
    ?_assertEqual([M6, M7, M8, M9, M10], to_list(Mail2)),
    ?_assertEqual([M1, M3, M5, M7, M9], to_list(Mail3)),
    ?_assertEqual([M2, M4, M6, M8, M10], to_list(Mail4)),
    ?_assertEqual([M1, M2, M3, M4, M5, M6, M7, M8, M9, M10], to_list(Mail5))
  ].
