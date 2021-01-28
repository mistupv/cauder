-module(cauder_syntax_tests).

-import(cauder_syntax, [pattern/1, guard_test/1, gexpr/1]).

-include_lib("eunit/include/eunit.hrl").

-define(_assertPattern(Expect, String), ?_assertEqual(Expect, pattern(parse(String)))).

-define(_assertGuardTest(Expect, String), ?_assertEqual(Expect, guard_test(parse(String)))).

-define(_assertGexpr(Expect, String), ?_assertEqual(Expect, gexpr(parse(String)))).


pattern_test_() ->
  [
    ?_assertPattern({var, 1, '_'}, "_"),
    ?_assertPattern({var, 1, 'X'}, "X"),

    ?_assertPattern({value, 1, 42}, "42"),
    ?_assertPattern({value, 1, -1234567890}, "-1_234_567_890"),
    ?_assertPattern({value, 1, 65}, "$A"),
    ?_assertPattern({value, 1, 10}, "$\n"),
    ?_assertPattern({value, 1, 5}, "2#101"),
    ?_assertPattern({value, 1, 31}, "16#1f"),
    ?_assertPattern({value, 1, 5216630098191412324}, "16#4865_316F_774F_6C64"),
    ?_assertPattern({value, 1, 2.3}, "2.3"),
    ?_assertPattern({value, 1, 2300.0}, "2.3e3"),
    ?_assertPattern({value, 1, 0.0023}, "2.3e-3"),
    ?_assertPattern({value, 1, 1234.333333}, "1_234.333_333"),

    ?_assertPattern({value, 1, hello}, "hello"),
    ?_assertPattern({value, 1, phone_number}, "phone_number"),
    ?_assertPattern({value, 1, 'Monday'}, "'Monday'"),
    ?_assertPattern({value, 1, 'phone number'}, "'phone number'"),

    ?_assertPattern({value, 1, true}, "true"),
    ?_assertPattern({value, 1, false}, "false"),

    ?_assertPattern({value, 1, "hello"}, "\"hello\""),
    ?_assertPattern({value, 1, "string42"}, "\"string\" \"42\""),

    ?_assertPattern({value, 1, []}, "[]"),
    ?_assertPattern({value, 1, [a]}, "[a]"),
    ?_assertPattern({value, 1, [a, b, c]}, "[\na,\nb,\nc\n]"),
    ?_assertPattern({value, 1, [a, b, c]}, "[a | [b | [c | []]]]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, []}}, "[A]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, [b, c]}}, "[A, b, c]"),
    ?_assertPattern({cons, 1, {value, 2, a}, {cons, 3, {var, 3, 'B'}, {value, 4, [c]}}}, "[\na,\nB,\nc\n]"),

    ?_assertPattern({value, 1, [a | b]}, "[a | b]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, b}}, "[A | b]"),
    ?_assertPattern({cons, 1, {value, 1, a}, {var, 1, 'B'}}, "[a | B]"),

    ?_assertPattern({value, 1, {}}, "{}"),
    ?_assertPattern({value, 1, {adam, 24, {july, 29}}}, "{adam, 24, {july, 29}}"),
    ?_assertPattern({tuple, 1, [{value, 1, adam}, {value, 1, 24}, {var, 1, 'DOB'}]}, "{adam, 24, DOB}"),

    ?_assertPattern(
      {match, 1,
       {tuple, 1, [{var, 1, 'A'}, {var, 1, 'B'}]},
       {value, 1, {answer, 42}}},
      "{A, B} = {answer, 42}"),
    ?_assertPattern(
      {match, 1,
       {tuple, 1, [{var, 1, 'C'}, {var, 1, 'D'}]},
       {value, 1, [1, 2]}},
      "{C, D} = [1, 2]"), % TODO Should this fail?

    ?_assertPattern({value, 1, 1}, "+1"),
    ?_assertPattern({value, 1, -1}, "-1"),
    ?_assertPattern({value, 1, 1}, "-(-1)"),
    ?_assertPattern({value, 1, 2}, "1 + 1"),
    ?_assertPattern({value, 1, 2.0}, "4 / 2"),
    ?_assertPattern({value, 1, 2}, "5 div 2"),
    ?_assertPattern({value, 1, 1}, "5 rem 2"),
    ?_assertPattern({value, 1, 0}, "2#10 band 2#01"),
    ?_assertPattern({value, 1, 3}, "2#10 bor 2#01"),
    ?_assertPattern({cons, 1, {value, 1, $i}, {cons, 1, {value, 1, $o}, {var, 1, 'Str'}}}, "\"io\" ++ Str"),
    ?_assertPattern({value, 1, [1, 2, 3, 4, 5]}, "[1, 2, 3] ++ [4, 5]")
  ].


guard_test_test_() ->
  [
    ?_assertGuardTest({var, 1, 'X'}, "X"),

    ?_assertGuardTest({value, 1, false}, "42"),
    ?_assertGuardTest({op, 1, '-', [{value, 1, 1234567890}]}, "-1_234_567_890"),
    ?_assertGuardTest({value, 1, false}, "$A"),
    ?_assertGuardTest({value, 1, false}, "$\n"),
    ?_assertGuardTest({value, 1, false}, "2#101"),
    ?_assertGuardTest({value, 1, false}, "16#1f"),
    ?_assertGuardTest({value, 1, false}, "16#4865_316F_774F_6C64"),
    ?_assertGuardTest({value, 1, false}, "2.3"),
    ?_assertGuardTest({value, 1, false}, "2.3e3"),
    ?_assertGuardTest({value, 1, false}, "2.3e-3"),
    ?_assertGuardTest({value, 1, false}, "1_234.333_333"),

    ?_assertGuardTest({value, 1, false}, "hello"),
    ?_assertGuardTest({value, 1, false}, "phone_number"),
    ?_assertGuardTest({value, 1, false}, "'Monday'"),
    ?_assertGuardTest({value, 1, false}, "'phone number'"),

    ?_assertGuardTest({value, 1, true}, "true"),
    ?_assertGuardTest({value, 1, false}, "false"),

    ?_assertGuardTest({value, 1, false}, "\"hello\""),
    ?_assertGuardTest({value, 1, false}, "\"string\" \"42\""),

    ?_assertGuardTest({value, 1, false}, "[]"),
    ?_assertGuardTest({value, 1, false}, "[a]"),
    ?_assertGuardTest({value, 1, false}, "[\na,\nb,\nc\n]"),
    ?_assertGuardTest({value, 1, false}, "[a | [b | [c | []]]]"),
    ?_assertGuardTest({value, 1, false}, "[A]"),
    ?_assertGuardTest({value, 1, false}, "[A, b, c]"),
    ?_assertGuardTest({value, 1, false}, "[\na,\nB,\nc\n]"),

    ?_assertGuardTest({value, 1, false}, "[a | b]"),
    ?_assertGuardTest({value, 1, false}, "[A | b]"),
    ?_assertGuardTest({value, 1, false}, "[a | B]"),

    ?_assertGuardTest({value, 1, false}, "{}"),
    ?_assertGuardTest({value, 1, false}, "{adam, 24, {july, 29}}"),
    ?_assertGuardTest({value, 1, false}, "{adam, 24, DOB}"),

    ?_assertGuardTest({op, 1, '+', [{value, 1, 1}]}, "+1"),
    ?_assertGuardTest({op, 1, '-', [{value, 1, 1}]}, "-1"),
    ?_assertGuardTest({op, 1, '-', [{op, 1, '-', [{value, 1, 1}]}]}, "-(-1)"),
    ?_assertGuardTest({op, 1, '+', [{value, 1, 1}, {value, 1, 1}]}, "1 + 1"),
    ?_assertGuardTest({op, 1, '/', [{value, 1, 4}, {value, 1, 2}]}, "4 / 2"),
    ?_assertGuardTest({op, 1, 'div', [{value, 1, 5}, {value, 1, 2}]}, "5 div 2"),
    ?_assertGuardTest({op, 1, 'rem', [{value, 1, 5}, {value, 1, 2}]}, "5 rem 2"),
    ?_assertGuardTest({op, 1, 'band', [{value, 1, 2}, {value, 1, 1}]}, "2#10 band 2#01"),
    ?_assertGuardTest({op, 1, 'bor', [{value, 1, 2}, {value, 1, 1}]}, "2#10 bor 2#01"),

    % function applications with side-effects
    ?_assertGuardTest({self, 1}, "erlang:self()"),

    % function applications
    ?_assertGuardTest({bif, 1, erlang, length, [{var, 1, 'List'}]}, "erlang:length(List)"),
    ?_assertGuardTest({bif, 1, erlang, is_list, [{var, 1, 'List'}]}, "erlang:is_list(List)"),

    % operators
    ?_assertGuardTest({'orelse', 1,
                       {bif, 1, erlang, is_atom, [{var, 1, 'L'}]},
                       {'andalso', 1,
                        {bif, 1, erlang, is_list, [{var, 1, 'L'}]},
                        {op, 1, '==',
                         [{bif, 1, erlang, length, [{var, 1, 'L'}]},
                          {value, 1, 1}]}}},
                      "erlang:is_atom(L) orelse (erlang:is_list(L) andalso erlang:length(L) == 1)")
  ].


gexpr_test_() ->
  [
    ?_assertGexpr({var, 1, '_'}, "_"),
    ?_assertGexpr({var, 1, 'X'}, "X"),

    ?_assertGexpr({value, 1, 42}, "42"),
    ?_assertGexpr({op, 1, '-', [{value, 1, 1234567890}]}, "-1_234_567_890"),
    ?_assertGexpr({value, 1, 65}, "$A"),
    ?_assertGexpr({value, 1, 10}, "$\n"),
    ?_assertGexpr({value, 1, 5}, "2#101"),
    ?_assertGexpr({value, 1, 31}, "16#1f"),
    ?_assertGexpr({value, 1, 5216630098191412324}, "16#4865_316F_774F_6C64"),
    ?_assertGexpr({value, 1, 2.3}, "2.3"),
    ?_assertGexpr({value, 1, 2300.0}, "2.3e3"),
    ?_assertGexpr({value, 1, 0.0023}, "2.3e-3"),
    ?_assertGexpr({value, 1, 1234.333333}, "1_234.333_333"),

    ?_assertGexpr({value, 1, hello}, "hello"),
    ?_assertGexpr({value, 1, phone_number}, "phone_number"),
    ?_assertGexpr({value, 1, 'Monday'}, "'Monday'"),
    ?_assertGexpr({value, 1, 'phone number'}, "'phone number'"),

    ?_assertGexpr({value, 1, true}, "true"),
    ?_assertGexpr({value, 1, false}, "false"),

    ?_assertGexpr({value, 1, "hello"}, "\"hello\""),
    ?_assertGexpr({value, 1, "string42"}, "\"string\" \"42\""),

    ?_assertGexpr({value, 1, []}, "[]"),
    ?_assertGexpr({value, 1, [a]}, "[a]"),
    ?_assertGexpr({value, 1, [a, b, c]}, "[\na,\nb,\nc\n]"),
    ?_assertGexpr({value, 1, [a, b, c]}, "[a | [b | [c | []]]]"),
    ?_assertGexpr({cons, 1, {var, 1, 'A'}, {value, 1, []}}, "[A]"),
    ?_assertGexpr({cons, 1, {var, 1, 'A'}, {value, 1, [b, c]}}, "[A, b, c]"),
    ?_assertGexpr({cons, 1, {value, 2, a}, {cons, 3, {var, 3, 'B'}, {value, 4, [c]}}}, "[\na,\nB,\nc\n]"),

    ?_assertGexpr({value, 1, [a | b]}, "[a | b]"),
    ?_assertGexpr({cons, 1, {var, 1, 'A'}, {value, 1, b}}, "[A | b]"),
    ?_assertGexpr({cons, 1, {value, 1, a}, {var, 1, 'B'}}, "[a | B]"),

    ?_assertGexpr({value, 1, {}}, "{}"),
    ?_assertGexpr({value, 1, {adam, 24, {july, 29}}}, "{adam, 24, {july, 29}}"),
    ?_assertGexpr({tuple, 1, [{value, 1, adam}, {value, 1, 24}, {var, 1, 'DOB'}]}, "{adam, 24, DOB}"),

    ?_assertGexpr({op, 1, '+', [{value, 1, 1}]}, "+1"),
    ?_assertGexpr({op, 1, '-', [{value, 1, 1}]}, "-1"),
    ?_assertGexpr({op, 1, '-', [{op, 1, '-', [{value, 1, 1}]}]}, "-(-1)"),
    ?_assertGexpr({op, 1, '+', [{value, 1, 1}, {value, 1, 1}]}, "1 + 1"),
    ?_assertGexpr({op, 1, '/', [{value, 1, 4}, {value, 1, 2}]}, "4 / 2"),
    ?_assertGexpr({op, 1, 'div', [{value, 1, 5}, {value, 1, 2}]}, "5 div 2"),
    ?_assertGexpr({op, 1, 'rem', [{value, 1, 5}, {value, 1, 2}]}, "5 rem 2"),
    ?_assertGexpr({op, 1, 'band', [{value, 1, 2}, {value, 1, 1}]}, "2#10 band 2#01"),
    ?_assertGexpr({op, 1, 'bor', [{value, 1, 2}, {value, 1, 1}]}, "2#10 bor 2#01"),

    % function applications with side-effects
    ?_assertGexpr({self, 1}, "erlang:self()"),

    % function applications
    ?_assertGexpr({bif, 1, erlang, length, [{var, 1, 'List'}]}, "erlang:length(List)"),
    ?_assertGexpr({bif, 1, erlang, is_list, [{var, 1, 'List'}]}, "erlang:is_list(List)"),

    % operators
    ?_assertGexpr({'orelse', 1,
                   {bif, 1, erlang, is_atom, [{var, 1, 'L'}]},
                   {'andalso', 1,
                    {bif, 1, erlang, is_list, [{var, 1, 'L'}]},
                    {op, 1, '==',
                     [{bif, 1, erlang, length, [{var, 1, 'L'}]},
                      {value, 1, 1}]}}},
                  "erlang:is_atom(L) orelse (erlang:is_list(L) andalso erlang:length(L) == 1)")
  ].


-spec parse(String) -> AbstractExpression when
  String :: string(),
  AbstractExpression :: erl_parse:abstract_expr().

parse(String) ->
  {ok, Tokens, _} = erl_scan:string(String ++ "."),
  {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
  Expr.
