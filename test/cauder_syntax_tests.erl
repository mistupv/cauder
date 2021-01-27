-module(cauder_syntax_tests).

-import(cauder_syntax, [pattern/1]).

-include_lib("eunit/include/eunit.hrl").

-define(_assertPattern(Expect, String), ?_assertEqual(Expect, pattern(parse(String)))).


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


-spec parse(String) -> AbstractExpression when
  String :: string(),
  AbstractExpression :: erl_parse:abstract_expr().

parse(String) ->
  {ok, Tokens, _} = erl_scan:string(String ++ "."),
  {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
  Expr.
