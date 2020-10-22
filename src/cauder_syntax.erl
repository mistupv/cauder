%%%-----------------------------------------------------------------------------
%%% @doc CauDEr syntax trees.
%%% This module defines functions that transform "parse trees" to custom syntax
%%% trees based on the ones used by the Erlang debugger.
%%% @see erl_parse
%%% @see cauder_types
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_syntax).

%% API
-export([clauses/1, expr_list/1]).
-export([replace_variable/3]).
-export([to_abstract_expr/1]).
-export([remote_call/3]).


%%------------------------------------------------------------------------------
%% @doc Transforms a list of abstract clauses to the custom CauDEr
%% representation.

-spec clauses(Clauses1) -> Clauses2 when
  Clauses1 :: [erl_parse:abstract_clause()],
  Clauses2 :: [cauder_types:af_clause()].

clauses([C0 | Cs]) ->
  C1 = clause(C0),
  [C1 | clauses(Cs)];
clauses([]) -> [].


-spec clause(Clause1) -> Clause2 when
  Clause1 :: erl_parse:abstract_clause(),
  Clause2 :: cauder_types:af_clause().

clause({clause, Anno, H0, G0, B0}) ->
  H1 = head(H0),
  G1 = guard(G0),
  B1 = exprs(B0),
  {clause, ln(Anno), H1, G1, B1}.


head(Ps) -> patterns(Ps).


patterns([P0 | Ps]) ->
  P1 = pattern(P0),
  [P1 | patterns(Ps)];
patterns([]) -> [].


pattern({integer, Anno, I})               -> {value, ln(Anno), I};
pattern({char, Anno, I})                  -> {value, ln(Anno), I};
pattern({float, Anno, F})                 -> {value, ln(Anno), F};
pattern({atom, Anno, A})                  -> {value, ln(Anno), A};
pattern({string, Anno, S})                -> {value, ln(Anno), S};
pattern({nil, Anno})                      -> {value, ln(Anno), []};
pattern({var, Anno, V})                   -> {var, ln(Anno), V};
pattern({cons, Anno, H, T})               -> {cons, ln(Anno), pattern(H), pattern(T)};
pattern({tuple, Anno, Es})                -> {tuple, ln(Anno), pattern_list(Es)};
pattern({match, Anno, Pat1, Pat2})        -> {match, ln(Anno), pattern(Pat1), pattern(Pat2)};
pattern({op, _, '-', {integer, Anno, I}}) -> {value, ln(Anno), -I};
pattern({op, _, '+', {integer, Anno, I}}) -> {value, ln(Anno), I};
pattern({op, _, '-', {char, Anno, I}})    -> {value, ln(Anno), -I};
pattern({op, _, '+', {char, Anno, I}})    -> {value, ln(Anno), I};
pattern({op, _, '-', {float, Anno, I}})   -> {value, ln(Anno), -I};
pattern({op, _, '+', {float, Anno, I}})   -> {value, ln(Anno), I}.

%% TODO Patterns - Map & Bit String
%% TODO Patterns - Evaluate compile-time expressions.


pattern_list([P0 | Ps]) ->
  P1 = pattern(P0),
  [P1 | pattern_list(Ps)];
pattern_list([]) -> [].


guard([G0 | Gs]) ->
  G1 = and_guard(G0),
  [G1 | guard(Gs)];
guard([]) -> [].


and_guard([G0 | Gs]) ->
  G1 = guard_test(G0),
  [G1 | and_guard(Gs)];
and_guard([]) -> [].


guard_test({var, _, _} = V)    -> V; % Boolean var
guard_test({atom, Anno, true}) -> {value, ln(Anno), true};
%% All other constants at this level means false.
guard_test({atom, Anno, _})    -> {value, ln(Anno), false};
guard_test({integer, Anno, _}) -> {value, ln(Anno), false};
guard_test({char, Anno, _})    -> {value, ln(Anno), false};
guard_test({float, Anno, _})   -> {value, ln(Anno), false};
guard_test({string, Anno, _})  -> {value, ln(Anno), false};
guard_test({nil, Anno})        -> {value, ln(Anno), false};
guard_test({cons, Anno, _, _}) -> {value, ln(Anno), false};
guard_test({tuple, Anno, _})   -> {value, ln(Anno), false};

guard_test({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, self}}, []}) ->
  {self, ln(Anno)};
guard_test({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, F}}, As0}) ->
  check_guard_bif(F, length(As0)),
  As = gexpr_list(As0),
  {bif, ln(Anno), erlang, F, As};
guard_test({op, Anno, Op, L0}) ->
  check_guard_op(Op, 1, [arith, bool]),
  L1 = gexpr(L0),
  {op, ln(Anno), Op, [L1]};
guard_test({op, Anno, Op, L0, R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
  L1 = gexpr(L0),
  R1 = gexpr(R0),
  {Op, ln(Anno), L1, R1};
guard_test({op, Anno, Op, L0, R0}) ->
  check_guard_op(Op, 2, [comp, bool, arith]),
  L1 = gexpr(L0),
  R1 = gexpr(R0),
  {op, ln(Anno), Op, [L1, R1]};
guard_test(_)                  -> error(guard_expr).


gexpr({integer, Anno, I})               -> {value, ln(Anno), I};
gexpr({char, Anno, I})                  -> {value, ln(Anno), I};
gexpr({float, Anno, F})                 -> {value, ln(Anno), F};
gexpr({atom, Anno, A})                  -> {value, ln(Anno), A};
gexpr({string, Anno, S})                -> {value, ln(Anno), S};
gexpr({nil, Anno})                      -> {value, ln(Anno), []};
gexpr({var, Anno, V})                   -> {var, ln(Anno), V};
gexpr({cons, Anno, H0, T0}) ->
  case {gexpr(H0), gexpr(T0)} of
    {{value, Line, H1}, {value, Line, T1}} -> {value, Line, [H1 | T1]};
    {H1, T1} -> {cons, ln(Anno), H1, T1}
  end;
gexpr({tuple, Anno, Es0}) ->
  Es1 = gexpr_list(Es0),
  {tuple, ln(Anno), Es1};
gexpr({op, _, '-', {integer, Anno, I}}) -> {value, ln(Anno), -I};
gexpr({op, _, '+', {integer, Anno, I}}) -> {value, ln(Anno), I};
gexpr({op, _, '-', {char, Anno, I}})    -> {value, ln(Anno), -I};
gexpr({op, _, '+', {char, Anno, I}})    -> {value, ln(Anno), I};
gexpr({op, _, '-', {float, Anno, I}})   -> {value, ln(Anno), -I};
gexpr({op, _, '+', {float, Anno, I}})   -> {value, ln(Anno), I};

%% TODO Guards - Map & Bit String

%%% The erl_expand_records pass has added the module name 'erlang' to
%%% all BIF calls, even in guards.
gexpr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, self}}, []}) ->
  {self, ln(Anno)};
gexpr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, F}}, As0}) ->
  check_guard_bif(F, length(As0)),
  As = gexpr_list(As0),
  {bif, ln(Anno), erlang, F, As};
gexpr({op, Anno, Op, A0}) ->
  check_guard_op(Op, 1, [arith]),
  A1 = gexpr(A0),
  {op, ln(Anno), Op, [A1]};
gexpr({op, Anno, Op, L0, R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
  L1 = gexpr(L0),
  R1 = gexpr(R0),
  {Op, ln(Anno), L1, R1};
gexpr({op, Anno, Op, L0, R0}) ->
  check_guard_op(Op, 2, [comp, bool, arith]),
  L1 = gexpr(L0),
  R1 = gexpr(R0),
  {op, ln(Anno), Op, [L1, R1]};
gexpr(_)                                -> error(guard_expr).


gexpr_list([E0 | Es]) ->
  E1 = gexpr(E0),
  [E1 | gexpr_list(Es)];
gexpr_list([]) -> [].


exprs([E0 | Es]) ->
  E1 = expr(E0),
  [E1 | exprs(Es)];
exprs([]) -> [].


expr({var, Anno, V})                   -> {var, ln(Anno), V};
expr({integer, Anno, I})               -> {value, ln(Anno), I};
expr({char, Anno, I})                  -> {value, ln(Anno), I};
expr({float, Anno, F})                 -> {value, ln(Anno), F};
expr({atom, Anno, A})                  -> {value, ln(Anno), A};
expr({string, Anno, S})                -> {value, ln(Anno), S};
expr({nil, Anno})                      -> {value, ln(Anno), []};
expr({cons, Anno, H0, T0}) ->
  case {expr(H0), expr(T0)} of
    {{value, Line, H1}, {value, Line, T1}} -> {value, Line, [H1 | T1]};
    {H1, T1} -> {cons, ln(Anno), H1, T1}
  end;
expr({tuple, Anno, Es0}) ->
  Es1 = expr_list(Es0),
  try lists:map(fun({value, _, V}) -> V end, Es1) of
    Es2 -> {value, ln(Anno), list_to_tuple(Es2)}
  catch
    error:function_clause -> {tuple, ln(Anno), Es1}
  end;
expr({'if', Anno, Cs0}) ->
  Cs1 = icr_clauses(Cs0),
  {'if', ln(Anno), Cs1};
expr({'case', Anno, E0, Cs0}) ->
  E1 = expr(E0),
  Cs1 = icr_clauses(Cs0),
  {'case', ln(Anno), E1, Cs1};
expr({'receive', Anno, Cs0}) ->
  Cs1 = icr_clauses(Cs0),
  {'receive', ln(Anno), Cs1};
expr({'fun', Anno, {clauses, Cs0}}) ->
  Cs = fun_clauses(Cs0),
  Name = new_fun_name(),
  {make_fun, ln(Anno), Name, Cs};
expr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, self}}, []}) ->
  {self, ln(Anno)};
expr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, spawn}}, [Fun]}) ->
  {spawn, ln(Anno), expr(Fun)};
expr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, spawn}}, [Mod, Func, As]}) ->
  {spawn, ln(Anno), expr(Mod), expr(Func), expr(As)};
expr({call, Anno, {remote, _, {atom, _, erlang}, {atom, _, send}}, [Dest, Msg]}) ->
  {send, ln(Anno), expr(Dest), expr(Msg)};
expr({call, Anno, {remote, _, {atom, _, Mod}, {atom, _, Func}}, As0}) ->
  As = expr_list(As0),
  case erlang:is_builtin(Mod, Func, length(As)) of
    false -> {remote_call, ln(Anno), Mod, Func, As};
    true -> {bif, ln(Anno), Mod, Func, As}
  end;
expr({call, Anno, {atom, _, Func}, As0}) ->
  As = expr_list(As0),
  {local_call, ln(Anno), Func, As};
expr({call, Anno, {remote, _, Mod0, Func0}, As0}) ->
  Mod = expr(Mod0),
  Func = expr(Func0),
  As = expr_list(As0),
  {apply, ln(Anno), Mod, Func, As};
expr({call, Anno, Fun0, As0}) ->
  Fun = expr(Fun0),
  As = expr_list(As0),
  {apply_fun, ln(Anno), Fun, As};
expr({match, Anno, P0, E0}) ->
  E1 = expr(E0),
  P1 = pattern(P0),
  {match, ln(Anno), P1, E1};
expr({op, _, '-', {integer, Anno, I}}) -> {value, ln(Anno), -I};
expr({op, _, '+', {integer, Anno, I}}) -> {value, ln(Anno), I};
expr({op, _, '-', {char, Anno, I}})    -> {value, ln(Anno), -I};
expr({op, _, '+', {char, Anno, I}})    -> {value, ln(Anno), I};
expr({op, _, '-', {float, Anno, I}})   -> {value, ln(Anno), -I};
expr({op, _, '+', {float, Anno, I}})   -> {value, ln(Anno), I};
expr({op, Anno, Op, A0}) ->
  A1 = expr(A0),
  {op, ln(Anno), Op, [A1]};
expr({op, Anno, '!', L0, R0}) ->
  L1 = expr(L0),
  R1 = expr(R0),
  {send_op, ln(Anno), L1, R1};
expr({op, Anno, Op, L0, R0}) when Op =:= 'andalso'; Op =:= 'orelse' ->
  L1 = expr(L0),
  R1 = expr(R0),
  {Op, ln(Anno), L1, R1};
expr({op, Anno, Op, L0, R0}) ->
  L1 = expr(L0),
  R1 = expr(R0),
  {op, ln(Anno), Op, [L1, R1]}.

%% TODO Expressions - Map, Block, Try-Catch, Comprehensions & Bit String


%%------------------------------------------------------------------------------
%% @doc Transforms a list of abstract expressions to the custom CauDEr
%% representation.

-spec expr_list(Expressions1) -> Expressions2 when
  Expressions1 :: [erl_parse:abstract_expr()],
  Expressions2 :: [cauder_types:abstract_expr()].

expr_list([E0 | Es]) ->
  E1 = expr(E0),
  [E1 | expr_list(Es)];
expr_list([]) -> [].


icr_clauses([C0 | Cs]) ->
  C1 = clause(C0),
  [C1 | icr_clauses(Cs)];
icr_clauses([]) -> [].


fun_clauses([{clause, A, H, G, B} | Cs]) ->
  [{clause, ln(A), head(H), guard(G), exprs(B)} | fun_clauses(Cs)];
fun_clauses([]) -> [].


new_fun_name() ->
  {F, A} = get(current_function),
  I = get(fun_count),
  put(fun_count, I + 1),
  Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A) ++ "-fun-" ++ integer_to_list(I) ++ "-",
  list_to_atom(Name).


ln(Anno) -> erl_anno:line(Anno).


check_guard_bif(Name, Arity) ->
  case erl_internal:guard_bif(Name, Arity) of
    true -> ok;
    false -> error(guard_expr)
  end.


check_guard_op(Op, Arity, AllowedTypes) ->
  Type = erl_internal:op_type(Op, Arity),
  case lists:member(Type, AllowedTypes) of
    true -> ok;
    false -> error(guard_expr)
  end.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Replaces all occurrences of the given `Variable' in each one of the
%% `Expressions' with the given literal `Value'.

-spec replace_variable(Expression | [Expression], Variable, Value) -> NewExpression | [NewExpression] when
  Expression :: cauder_types:abstract_expr(),
  Variable :: cauder_types:af_variable(),
  Value :: term(),
  NewExpression :: cauder_types:abstract_expr().

replace_variable([E0 | Es], Var = {var, _, _}, Val) ->
  E = replace_variable(E0, Var, Val),
  [E | replace_variable(Es, Var, Val)];
replace_variable([], _, _) -> [];

replace_variable({clause, Line, H0, G0, B0}, Var, Val) ->
  H = replace_variable(H0, Var, Val),
  G = replace_variable(G0, Var, Val),
  B = replace_variable(B0, Var, Val),
  {clause, Line, H, G, B};

replace_variable(Expr = {value, _, _}, _, _) ->
  Expr;
replace_variable({var, _, Name}, {var, Line, Name}, Val) ->
  {value, Line, Val};
replace_variable(Expr = {var, _, _}, _, _) ->
  Expr;
replace_variable({cons, Line, H0, T0}, Var, Val) ->
  H = replace_variable(H0, Var, Val),
  T = replace_variable(T0, Var, Val),
  case cauder_eval:is_value(H) andalso cauder_eval:is_value(T) of
    true -> {value, Line, [cauder_eval:concrete(H) | cauder_eval:concrete(T)]};
    false -> {cons, Line, H, T}
  end;
replace_variable({tuple, Line, Es0}, Var, Val) ->
  Es = replace_variable(Es0, Var, Val),
  case cauder_eval:is_value(Es) of
    true ->
      Tuple = list_to_tuple(lists:map(fun cauder_eval:concrete/1, Es)),
      {value, Line, Tuple};
    false -> {tuple, Line, Es}
  end;
replace_variable({'if', Line, Cs0}, Var, Val) ->
  Cs = replace_variable(Cs0, Var, Val),
  {'if', Line, Cs};
replace_variable({'case', Line, A0, Cs0}, Var, Val) ->
  A = replace_variable(A0, Var, Val),
  Cs = replace_variable(Cs0, Var, Val),
  {'case', Line, A, Cs};
replace_variable({'receive', Line, Cs0}, Var, Val) ->
  Cs = replace_variable(Cs0, Var, Val),
  {'receive', Line, Cs};
replace_variable({make_fun, Line, Name, Cs0}, Var, Val) ->
  Cs = replace_variable(Cs0, Var, Val),
  {make_fun, Line, Name, Cs};
replace_variable({bif, Line, M, F, As0}, Var, Val) ->
  As = replace_variable(As0, Var, Val),
  {bif, Line, M, F, As};
replace_variable(E = {self, _}, _, _) ->
  E;
replace_variable({spawn, Line, Fun0}, Var, Val) ->
  Fun = replace_variable(Fun0, Var, Val),
  {spawn, Line, Fun};
replace_variable({spawn, Line, M0, F0, As0}, Var, Val) ->
  M = replace_variable(M0, Var, Val),
  F = replace_variable(F0, Var, Val),
  As = replace_variable(As0, Var, Val),
  {spawn, Line, M, F, As};
replace_variable({send, Line, L0, R0}, Var, Val) ->
  L = replace_variable(L0, Var, Val),
  R = replace_variable(R0, Var, Val),
  {send, Line, L, R};
replace_variable({send_op, Line, L0, R0}, Var, Val) ->
  L = replace_variable(L0, Var, Val),
  R = replace_variable(R0, Var, Val),
  {send_op, Line, L, R};
replace_variable({local_call, Line, F, As0}, Var, Val) ->
  As = replace_variable(As0, Var, Val),
  {local_call, Line, F, As};
replace_variable({remote_call, Line, M, F, As0}, Var, Val) ->
  As = replace_variable(As0, Var, Val),
  {remote_call, Line, M, F, As};
replace_variable({apply, Line, M0, F0, As0}, Var, Val) ->
  M = replace_variable(M0, Var, Val),
  F = replace_variable(F0, Var, Val),
  As = replace_variable(As0, Var, Val),
  {apply, Line, M, F, As};
replace_variable({apply_fun, Line, Fun0, As0}, Var, Val) ->
  Fun = replace_variable(Fun0, Var, Val),
  As = replace_variable(As0, Var, Val),
  {apply_fun, Line, Fun, As};
replace_variable({match, Line, P0, E0}, Var, Val) ->
  E = replace_variable(E0, Var, Val),
  P = replace_variable(P0, Var, Val),
  {match, Line, P, E};
replace_variable({op, Line, Op, [A0]}, Var, Val) ->
  A = replace_variable(A0, Var, Val),
  {op, Line, Op, [A]};
replace_variable({op, Line, Op, [L0, R0]}, Var, Val) ->
  L = replace_variable(L0, Var, Val),
  R = replace_variable(R0, Var, Val),
  {op, Line, Op, [L, R]};
replace_variable({Op, Line, L0, R0}, Var, Val) when Op =:= 'andalso'; Op =:= 'orelse' ->
  L = replace_variable(L0, Var, Val),
  R = replace_variable(R0, Var, Val),
  {Op, Line, L, R}.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Converts the given expression to the original `erl_parse'
%% representation.

-spec to_abstract_expr(Expression) -> NewExpression when
  Expression :: cauder_types:abstract_expr(),
  NewExpression :: erl_syntax:syntaxTree()
;                     (Expressions) -> NewExpressions when
  Expressions :: [cauder_types:abstract_expr()],
  NewExpressions :: [erl_syntax:syntaxTree()].

to_abstract_expr(Es) when is_list(Es) -> lists:map(fun to_abstract_expr/1, Es);

to_abstract_expr({clause, Line, H, G, B}) ->
  Node = erl_syntax:clause(to_abstract_expr(H), to_abstract_expr(G), to_abstract_expr(B)),
  set_line(Node, Line);

to_abstract_expr({value, Line, Fun}) when is_function(Fun) ->
  % TODO Module
  Node = erl_syntax:text(io_lib:format("~p", [Fun])),
  set_line(Node, Line);
to_abstract_expr({value, Line, Value}) ->
  Node = erl_syntax:abstract(Value),
  set_line(Node, Line);
to_abstract_expr({var, Line, Name}) ->
  Node = erl_syntax:variable(Name),
  set_line(Node, Line);
to_abstract_expr({cons, Line, H, T}) ->
  Node = erl_syntax:cons(to_abstract_expr(H), to_abstract_expr(T)),
  set_line(Node, Line);
to_abstract_expr({tuple, Line, Es}) ->
  Node = erl_syntax:tuple(to_abstract_expr(Es)),
  set_line(Node, Line);
to_abstract_expr({'if', Line, Cs}) ->
  Node = erl_syntax:if_expr(to_abstract_expr(Cs)),
  set_line(Node, Line);
to_abstract_expr({'case', Line, A, Cs}) ->
  Node = erl_syntax:case_expr(to_abstract_expr(A), to_abstract_expr(Cs)),
  set_line(Node, Line);
to_abstract_expr({'receive', Line, Cs}) ->
  Node = erl_syntax:receive_expr(to_abstract_expr(Cs)),
  set_line(Node, Line);
to_abstract_expr({make_fun, Line, _Name, Cs}) ->
  Node = erl_syntax:fun_expr(to_abstract_expr(Cs)),
  set_line(Node, Line);
to_abstract_expr({bif, Line, M, F, As}) ->
  Node = erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), to_abstract_expr(As)),
  set_line(Node, Line);
to_abstract_expr({self, Line}) ->
  Node = erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(self), []),
  set_line(Node, Line);
to_abstract_expr({spawn, Line, Fun}) ->
  Node = erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(spawn), [to_abstract_expr(Fun)]),
  set_line(Node, Line);
to_abstract_expr({spawn, Line, M, F, As}) ->
  Node = erl_syntax:application(
    erl_syntax:atom(erlang),
    erl_syntax:atom(spawn),
    [to_abstract_expr(M), to_abstract_expr(F), to_abstract_expr(As)]),
  set_line(Node, Line);
to_abstract_expr({send, Line, L, R}) ->
  Node = erl_syntax:application(erl_syntax:atom(erlang), erl_syntax:atom(send), [to_abstract_expr(L), to_abstract_expr(R)]),
  set_line(Node, Line);
to_abstract_expr({send_op, Line, L, R}) ->
  Node = erl_syntax:infix_expr(to_abstract_expr(L), erl_syntax:operator('!'), to_abstract_expr(R)),
  set_line(Node, Line);
to_abstract_expr({local_call, Line, F, As}) ->
  Node = erl_syntax:application(erl_syntax:atom(F), to_abstract_expr(As)),
  set_line(Node, Line);
to_abstract_expr({remote_call, Line, M, F, As}) ->
  Node = erl_syntax:application(erl_syntax:atom(M), erl_syntax:atom(F), to_abstract_expr(As)),
  set_line(Node, Line);
to_abstract_expr({apply, Line, M, F, As}) ->
  Node = erl_syntax:application(to_abstract_expr(M), to_abstract_expr(F), to_abstract_expr(As)),
  set_line(Node, Line);
to_abstract_expr({apply_fun, Line, Fun, As}) ->
  Node = erl_syntax:application(to_abstract_expr(Fun), to_abstract_expr(As)),
  set_line(Node, Line);
to_abstract_expr({match, Line, P, E}) ->
  Node = erl_syntax:match_expr(to_abstract_expr(P), to_abstract_expr(E)),
  set_line(Node, Line);
to_abstract_expr({op, Line, Op, [A]}) ->
  Node = erl_syntax:prefix_expr(erl_syntax:operator(Op), to_abstract_expr(A)),
  set_line(Node, Line);
to_abstract_expr({op, Line, Op, [L, R]}) ->
  Node = erl_syntax:infix_expr(to_abstract_expr(L), erl_syntax:operator(Op), to_abstract_expr(R)),
  set_line(Node, Line);
to_abstract_expr({Op, Line, L, R}) when Op =:= 'andalso'; Op =:= 'orelse' ->
  Node = erl_syntax:infix_expr(to_abstract_expr(L), erl_syntax:operator(Op), to_abstract_expr(R)),
  set_line(Node, Line).


-spec set_line(SyntaxTree1, Line) -> SyntaxTree2 when
  SyntaxTree1 :: erl_syntax:syntaxTree(),
  Line :: non_neg_integer(),
  SyntaxTree2 :: erl_syntax:syntaxTree().

set_line(Node, Line) -> erl_syntax:set_pos(Node, erl_anno:new(Line)).


%%%=============================================================================


-spec remote_call(Module, Function, Arguments) -> RemoteCall when
  Module :: module(),
  Function :: atom(),
  Arguments :: [cauder_types:af_literal()],
  RemoteCall :: cauder_types:af_remote_call().

remote_call(M, F, As) ->
  A = length(As),
  {_, Cs} = cauder_utils:fundef_lookup({M, F, A}),
  Line = cauder_eval:clause_line([], Cs, As),
  {remote_call, Line, M, F, lists:map(fun(V) -> setelement(2, V, Line) end, As)}.



