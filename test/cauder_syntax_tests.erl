-module(cauder_syntax_tests).

-import(cauder_syntax, [pattern/1]).

-include_lib("eunit/include/eunit.hrl").

-define(_assertPattern(Expect, String), ?_assertEqual(Expect, pattern(parse(String)))).
-define(_assertNotPattern(String), ?_assertError(illegal_pattern, pattern(parse(String)))).


pattern_test_() ->
  [
    % Variable
    ?_assertPattern({var, 1, 'Name'}, "Name"),
    ?_assertPattern({var, 1, '_'}, "_"),

    % Integer
    ?_assertPattern({value, 1, 42}, "42"),
    ?_assertPattern({value, 1, -42}, "-42"),
    ?_assertPattern({value, 1, 42}, "+42"),

    % Char
    ?_assertPattern({value, 1, $\n}, "$\n"),
    ?_assertPattern({value, 1, -$\n}, "-$\n"),
    ?_assertPattern({value, 1, $\n}, "+$\n"),

    % Float
    ?_assertPattern({value, 1, 2.3e-3}, "2.3e-3"),
    ?_assertPattern({value, 1, -2.3e-3}, "-2.3e-3"),
    ?_assertPattern({value, 1, 2.3e-3}, "+2.3e-3"),

    % Atom
    ?_assertPattern({value, 1, hello_world}, "hello_world"),

    % String
    ?_assertPattern({value, 1, "Hello world!"}, "\"Hello world!\""),

    % Nil
    ?_assertPattern({value, 1, []}, "[]"),

    % Lists
    ?_assertPattern({value, 1, [a]}, "[a]"),
    ?_assertPattern({value, 2, [a, b, c]}, "\n[\na,\nb,\nc\n]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, []}}, "[A]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, [b, c]}}, "[A, b, c]"),
    ?_assertPattern({cons, 2, {value, 3, a}, {cons, 4, {var, 4, 'B'}, {value, 5, [c]}}}, "\n[\na,\nB,\nc\n]"),
    ?_assertNotPattern("[a, 42 ! b]"),

    % Improper Lists
    ?_assertPattern({value, 1, [a | b]}, "[a | b]"),
    ?_assertPattern({cons, 1, {var, 1, 'A'}, {value, 1, b}}, "[A | b]"),
    ?_assertPattern({cons, 1, {value, 1, a}, {var, 1, 'B'}}, "[a | B]"),
    ?_assertNotPattern("[a | 42 ! b]"),

    % Tuples
    ?_assertPattern({value, 1, {}}, "{}"),
    ?_assertPattern({value, 1, {adam, 24, {july, 29}}}, "{adam, 24, {july, 29}}"),
    ?_assertPattern({tuple, 1, [{value, 1, adam}, {value, 1, 24}, {var, 1, 'Date'}]}, "{adam, 24, Date}"),
    ?_assertNotPattern("{a, 42 ! b}"),

    % if expressions
    ?_assertNotPattern("if true -> ok end"),

    % case expressions
    ?_assertNotPattern("case 42 of _ -> ok end"),

    % receive expressions
    ?_assertNotPattern("receive _ -> ok end"),

    % fun expressions
    ?_assertNotPattern("fun () -> ok end"),
    ?_assertNotPattern("fun lists:memeber/2"),

    % function applications with side-effects
    ?_assertNotPattern("erlang:self()"),
    ?_assertNotPattern("erlang:spawn(Fun)"),
    ?_assertNotPattern("erlang:spawn(Module, Function, Arity)"),
    ?_assertNotPattern("erlang:send(Expr1, Expr2)"),

    % function applications
    ?_assertNotPattern("erlang:length(List)"),
    ?_assertNotPattern("lists:member(42, List)"),
    ?_assertNotPattern("lists:reverse(List)"),
    ?_assertNotPattern("handle(Msg, State)"),
    ?_assertNotPattern("Module:Function(Argument)"),
    ?_assertNotPattern("Fun(Argument)"),

    % match expression
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

    % operators
    ?_assertNotPattern("Expr1 ! Expr2"),
    ?_assertNotPattern("List ++ [42]"), % FIXME Should be valid
    ?_assertNotPattern("true orelse (false andalso true)")
  ].


-spec parse(String) -> AbstractExpression when
  String :: string(),
  AbstractExpression :: erl_parse:abstract_expr().

parse(String) ->
  {ok, Tokens, _} = erl_scan:string(String ++ "."),
  {ok, [Expr]} = erl_parse:parse_exprs(Tokens),
  Expr.
