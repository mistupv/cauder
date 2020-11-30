-module(sample).

-export([test_simple/0]).
-export([test_function/0]).
-export([test_case1/0, test_case2/0, test_case3/0, test_case4/0, test_case5/0, test_case6/0, test_nested_case/0]).
-export([test_if/0]).
-export([test_receive/0, test_receive_spawn/0]).

test_simple() ->
  A = 7,
  B = 42,
  A + B.

test_function() ->
  A = 7,
  B = foo(),
  A + B.

test_case1() ->
  A =
    case lucky of
      lucky -> 7;
      not_lucky -> 0
    end,
  B = 42,
  A + B.

test_case2() ->
  A =
    case lucky of
      lucky ->
        good,
        7;
      not_lucky ->
        bad,
        0
    end,
  B = 42,
  A + B.

test_case3() ->
  A = 7,
  B =
    case the_answer of
      not_the_answer -> 0;
      the_answer -> 42
    end,
  A + B.

test_case4() ->
  A = 7,
  B =
    case the_answer of
      not_the_answer ->
        bad,
        0;
      the_answer ->
        good,
        42
    end,
  A + B.

test_case5() ->
  A = 7,
  B = 42,
  case {A, B} of
    {7, 42} -> 49;
    _ -> 0
  end.

test_case6() ->
  A = 7,
  B = 42,
  case {A, B} of
    {7, 42} ->
      lucky_answer,
      49;
    _ ->
      nothing,
      0
  end.

test_nested_case() ->
  case lucky of
    lucky ->
      case the_answer of
        the_answer -> 7 + 42;
        _ -> 0
      end;
    _ -> 0
  end.

test_if() ->
  A = 7,
  B = 42,
  if
    A =:= 7, B =:= 42 ->
      lucky_answer,
      49;
    true ->
      nothing,
      0
  end.

test_receive() ->
  self() ! {A = 7, B = 42},
  receive
    {A, B} ->
      lucky_answer,
      49;
    _ ->
      nothing,
      0
  end.

test_receive_spawn() ->
  Tuple = {A = 7, B = 42},
  Pid = self(),
  erlang:spawn(fun() -> Pid ! Tuple end),
  receive
    {A, B} ->
      lucky_answer,
      49;
    _ ->
      nothing,
      0
  end.

% ====================

foo() -> 42.
