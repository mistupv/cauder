-module(cauder_eval).

-export([seq/3, abstract/1, concrete/1, is_value/1, is_reducible/2, binding/2]).
-export([match_rec/4]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_list(cauder_types:environment(), [cauder_types:abstract_expr()], cauder_types:stack()) -> cauder_types:result().

eval_list(Bs, [E | Es], Stk) ->
  case is_reducible(E, Bs) of
    true ->
      R = #result{exprs = Es1} = expr(Bs, E, Stk),
      R#result{exprs = Es1 ++ Es};
    false ->
      R = #result{exprs = Es1} = eval_list(Bs, Es, Stk),
      R#result{exprs = [E | Es1]}
  end.


-spec seq(cauder_types:environment(), [cauder_types:abstract_expr()], cauder_types:stack()) -> cauder_types:result().

seq(Bs, [E | Es], Stk) ->
  case is_reducible(E, Bs) of
    false ->
      case Es of
        [] ->
          case Stk of
            % Call entry
            [{{_M, _F, _A}, Bs1, Es1, Var} | Stk1] ->
              Es2 = cauder_syntax:replace_variable(Es1, Var, concrete(E)),
              #result{env = Bs1, exprs = Es2, stack = Stk1};
            % Block entry
            [{_Type, Es1, Var} | Stk1] ->
              Es2 = cauder_syntax:replace_variable(Es1, Var, concrete(E)),
              #result{env = Bs, exprs = Es2, stack = Stk1}
          end;
        _ ->
          #result{env = Bs, exprs = Es, stack = Stk}
      end;
    true ->
      #result{env = Bs1, exprs = Es1, stack = Stk1, label = L} = expr(Bs, E, Stk),
      %io:format("expr: ~p\n", [Es1]),
      case Stk1 of
        [{{M, F, A}, Bs2, Es2, Var} | Stk] ->
          #result{env = Bs2, exprs = Es2, stack = [{{M, F, A}, Bs1, Es1 ++ Es, Var} | Stk], label = L};
        [{Type, Es2, Var} | Stk] ->
          #result{env = Bs1, exprs = Es2, stack = [{Type, Es1 ++ Es, Var} | Stk], label = L};
        _ ->
          #result{env = Bs1, exprs = Es1 ++ Es, stack = Stk1, label = L}
      end
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec expr(cauder_types:environment(), cauder_types:abstract_expr(), cauder_types:stack()) -> cauder_types:result().

expr(Bs, {var, Line, Name}, Stk) ->
  {value, Value} = binding(Name, Bs),
  #result{env = Bs, exprs = [{value, Line, Value}], stack = Stk};

expr(Bs, E = {cons, Line, H0, T0}, Stk) ->
  case is_reducible(H0, Bs) of
    true ->
      R = #result{exprs = [H]} = expr(Bs, H0, Stk),
      case is_value(H) andalso is_value(T0) of
        true -> R#result{exprs = [{value, Line, [concrete(H) | concrete(T0)]}]};
        false -> R#result{exprs = [setelement(3, E, H)]}
      end;
    false ->
      case is_reducible(T0, Bs) of
        true ->
          R = #result{exprs = [T]} = expr(Bs, T0, Stk),
          case is_value(H0) andalso is_value(T) of
            true -> R#result{exprs = [{value, Line, [concrete(H0) | concrete(T)]}]};
            false -> R#result{exprs = [setelement(4, E, T)]}
          end;
        false -> #result{env = Bs, exprs = [{value, Line, [concrete(H0) | concrete(T0)]}], stack = Stk}
      end
  end;

expr(Bs, E = {tuple, Line, Es0}, Stk) ->
  R = #result{exprs = Es} = eval_list(Bs, Es0, Stk),
  case is_value(Es) of
    true ->
      Tuple = list_to_tuple(lists:map(fun concrete/1, Es)),
      #result{env = Bs, exprs = [{value, Line, Tuple}], stack = Stk};
    false -> R#result{exprs = [setelement(3, E, Es)]}
  end;

expr(Bs, {'if', Line, Cs}, Stk0) ->
  case match_if(Bs, Cs) of
    {match, Body} ->
      Var = utils:temp_variable(Line),
      Stk = [{'if', Body, Var} | Stk0],
      #result{env = Bs, exprs = [Var], stack = Stk};
    nomatch -> error(if_clause)
  end;

expr(Bs0, E = {'case', Line, A, Cs}, Stk0) ->
  case is_reducible(A, Bs0) of
    true -> eval_and_update({Bs0, A, Stk0}, {3, E});
    false ->
      case match_case(Bs0, Cs, A) of
        {match, Bs, Body} ->
          Var = utils:temp_variable(Line),
          Stk = [{'case', Body, Var} | Stk0],
          #result{env = Bs, exprs = [Var], stack = Stk};
        nomatch -> error(case_clause, concrete(A))
      end
  end;

%% TODO Support receive with timeout
expr(Bs, {'receive', Line, Cs}, Stk0) ->
  % TODO One of these variables is not necessary
  Var = utils:temp_variable(Line),
  VarBody = utils:temp_variable(Line),
  Stk = [{'receive', [VarBody], Var} | Stk0],
  #result{env = Bs, exprs = [Var], stack = Stk, label = {rec, VarBody, Cs}};

% TODO Support fun() as entry point argument?
% TODO Handle calls to interpreted fun() from uninterpreted module
expr(Bs, {'make_fun', Line, Name, Cs}, Stk0) ->
  {ok, M} = current_module(Stk0),
  Arity = length(element(3, hd(Cs))),
  Info = {{M, Name}, Bs, Cs},
  Fun =
    case Arity of
      0 -> fun() -> {[], Info} end;
      1 -> fun(A) -> {[A], Info} end;
      2 -> fun(A, B) -> {[A, B], Info} end;
      3 -> fun(A, B, C) -> {[A, B, C], Info} end;
      4 -> fun(A, B, C, D) -> {[A, B, C, D], Info} end;
      5 -> fun(A, B, C, D, E) -> {[A, B, C, D, E], Info} end;
      _ -> error({argument_limit, Arity}) % TODO Support more arities
    end,
  #result{env = Bs, exprs = [{value, Line, Fun}], stack = Stk0};

expr(Bs, E = {bif, Line, M, F, As}, Stk) ->
  case is_reducible(As, Bs) of
    true -> eval_and_update({Bs, As, Stk}, {5, E});
    false ->
      Value = apply(M, F, lists:map(fun concrete/1, As)),
      #result{env = Bs, exprs = [{value, Line, Value}], stack = Stk}
  end;

expr(Bs, {self, Line}, Stk) ->
  Var = utils:temp_variable(Line),
  #result{env = Bs, exprs = [Var], stack = Stk, label = {self, Var}};

expr(Bs, E = {spawn, Line, Fun}, Stk) ->
  case is_reducible(Fun, Bs) of
    true -> eval_and_update({Bs, Fun, Stk}, {3, E});
    false ->
      Var = utils:temp_variable(Line),
      Label = {spawn, Var, concrete(Fun)},
      #result{env = Bs, exprs = [Var], stack = Stk, label = Label}
  end;

expr(Bs, E = {spawn, Line, M, F, As}, Stk) ->
  case is_reducible(M, Bs) of
    true -> eval_and_update({Bs, M, Stk}, {3, E});
    false ->
      case is_reducible(F, Bs) of
        true -> eval_and_update({Bs, F, Stk}, {4, E});
        false ->
          case is_reducible(As, Bs) of
            true -> eval_and_update({Bs, As, Stk}, {5, E});
            false ->
              Var = utils:temp_variable(Line),
              Label = {spawn, Var, concrete(M), concrete(F), concrete(As)},
              #result{env = Bs, exprs = [Var], stack = Stk, label = Label}
          end
      end
  end;

expr(Bs, E = {Send, _, L, R}, Stk) when Send =:= 'send' orelse Send =:= 'send_op' ->
  case is_reducible(L, Bs) of
    true -> eval_and_update({Bs, L, Stk}, {3, E});
    false ->
      case is_reducible(R, Bs) of
        true -> eval_and_update({Bs, R, Stk}, {4, E});
        false ->
          Label = {send, concrete(L), concrete(R)},
          #result{env = Bs, exprs = [R], stack = Stk, label = Label}
      end
  end;

expr(Bs0, E = {local_call, Line, F, As}, Stk0) ->
  case is_reducible(As, Bs0) of
    true -> eval_and_update({Bs0, As, Stk0}, {4, E});
    false ->
      {ok, M} = current_module(Stk0),
      A = length(As),
      {_, Cs} = utils:fundef_lookup(M, F, A),
      {match, Bs, Body} = match_fun(Cs, As),
      Var = utils:temp_variable(Line),
      Stk = [{{M, F, A}, Bs, Body, Var} | Stk0],
      #result{env = Bs0, exprs = [Var], stack = Stk}
  end;

expr(Bs0, E = {remote_call, Line, M, F, As}, Stk0) ->
  case is_reducible(As, Bs0) of
    true -> eval_and_update({Bs0, As, Stk0}, {5, E});
    false ->
      A = length(As),
      case utils:fundef_lookup(M, F, A) of
        {Exported, Cs} ->
          % Check if function is accessible
          case current_module(Stk0) of
            {ok, M} -> ok;
            {ok, _} -> true = Exported;
            error when Stk0 =:= [] -> ok
          end,
          {match, Bs, Body} = match_fun(Cs, As),
          Var = utils:temp_variable(Line),
          Stk = [{{M, F, A}, Bs, Body, Var} | Stk0],
          #result{env = Bs0, exprs = [Var], stack = Stk};
        error ->
          Value = apply(M, F, lists:map(fun concrete/1, As)),
          #result{env = Bs0, exprs = [{value, Line, Value}], stack = Stk0}
      end
  end;

% TODO Handle calls to self/0, spawn/1, spawn/3
expr(Bs0, E = {apply, Line, M0, F0, As}, Stk0) ->
  case is_reducible(M0, Bs0) of
    true -> eval_and_update({Bs0, M0, Stk0}, {3, E});
    false ->
      case is_reducible(F0, Bs0) of
        true -> eval_and_update({Bs0, F0, Stk0}, {4, E});
        false ->
          case is_reducible(As, Bs0) of
            true -> eval_and_update({Bs0, As, Stk0}, {5, E});
            false ->
              M = concrete(M0),
              F = concrete(F0),
              A = length(As),
              case utils:fundef_lookup(M, F, A) of
                {Exported, Cs} ->
                  % Check if function is accessible
                  case current_module(Stk0) of
                    {ok, M} -> ok;
                    {ok, _} -> true = Exported;
                    error when Stk0 =:= [] -> ok
                  end,
                  {match, Bs, Body} = match_fun(Cs, As),
                  Var = utils:temp_variable(Line),
                  Stk = [{{M, F, A}, Bs, Body, Var} | Stk0],
                  #result{env = Bs0, exprs = [Var], stack = Stk};
                error ->
                  Value = apply(M, F, lists:map(fun concrete/1, As)),
                  #result{env = Bs0, exprs = [{value, Line, Value}], stack = Stk0}
              end
          end
      end
  end;

expr(Bs0, E = {apply_fun, Line, Fun, As}, Stk0) ->
  case is_reducible(Fun, Bs0) of
    true -> eval_and_update({Bs0, Fun, Stk0}, {3, E});
    false ->
      case is_reducible(As, Bs0) of
        true -> eval_and_update({Bs0, As, Stk0}, {4, E});
        false ->
          A = length(As),
          {env, [{{M, F}, Bs1, Cs}]} = erlang:fun_info(concrete(Fun), env),
          {match, Bs2, Body} = match_fun(Cs, As),
          Var = utils:temp_variable(Line),
          Stk = [{{M, F, A}, utils:merge_bindings(Bs1, Bs2), Body, Var} | Stk0],
          #result{env = Bs0, exprs = [Var], stack = Stk}
      end
  end;

expr(Bs0, E = {match, _, Lhs, Rhs}, Stk) ->
  case is_reducible(Lhs, Bs0) of
    true -> eval_and_update({Bs0, Lhs, Stk}, {3, E});
    false ->
      case is_reducible(Rhs, Bs0) of
        true -> eval_and_update({Bs0, Rhs, Stk}, {4, E});
        false ->
          case match(Bs0, [Lhs], [Rhs]) of
            {match, Bs} -> #result{env = Bs, exprs = [Rhs], stack = Stk};
            nomatch -> error({badmatch, Bs0, Lhs, Rhs})
          end
      end
  end;

expr(Bs, E = {op, Line, Op, As}, Stk) ->
  case is_reducible(As, Bs) of
    true -> eval_and_update({Bs, As, Stk}, {4, E});
    false ->
      Value = apply(erlang, Op, lists:map(fun concrete/1, As)),
      #result{env = Bs, exprs = [{value, Line, Value}], stack = Stk}
  end;

expr(Bs, E = {'andalso', Line, Lhs, Rhs}, Stk) ->
  case is_reducible(Lhs, Bs) of
    true -> eval_and_update({Bs, Lhs, Stk}, {3, E});
    false ->
      case Lhs of
        {value, _, false} -> #result{env = Bs, exprs = [Lhs], stack = Stk};
        {value, _, true} ->
          case is_reducible(Rhs, Bs) of
            true -> eval_and_update({Bs, Rhs, Stk}, {4, E});
            false ->
              Value = apply(erlang, 'and', [concrete(Lhs), concrete(Rhs)]),
              #result{env = Bs, exprs = [{value, Line, Value}], stack = Stk}
          end
      end
  end;

expr(Bs, E = {'orelse', Line, Lhs, Rhs}, Stk) ->
  case is_reducible(Lhs, Bs) of
    true -> eval_and_update({Bs, Lhs, Stk}, {3, E});
    false ->
      case Lhs of
        {value, _, true} -> #result{env = Bs, exprs = [Lhs], stack = Stk};
        {value, _, false} ->
          case is_reducible(Rhs, Bs) of
            true -> eval_and_update({Bs, Rhs, Stk}, {4, E});
            false ->
              Value = apply(erlang, 'or', [concrete(Lhs), concrete(Rhs)]),
              #result{env = Bs, exprs = [{value, Line, Value}], stack = Stk}
          end
      end
  end.


%% =====================================================================


-spec match_if(cauder_types:environment(), cauder_types:af_clause_seq()) ->
  {match, [cauder_types:abstract_expr()]} | nomatch.

match_if(_, []) -> nomatch;
match_if(Bs, [{'clause', _, [], G, B} | Cs]) ->
  case concrete(eval_guard_seq(Bs, G)) of
    true -> {match, B};
    false -> match_if(Bs, Cs)
  end.


-spec match_case(cauder_types:environment(), cauder_types:af_clause_seq(), cauder_types:af_literal()) ->
  {match, cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_case(Bs, Cs, V) -> match_clause(Bs, Cs, [V]).


-spec match_fun(cauder_types:af_clause_seq(), [cauder_types:af_literal()]) ->
  {match, cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_fun(Cs, Vs) -> match_clause([], Cs, Vs).


-spec match_rec(Clauses, Environment, Log, Mail) -> {NewEnvironment, MatchedBranch, MatchedMessage, RestMessages} | nomatch when
  Clauses :: cauder_types:af_clause_seq(),
  Environment :: cauder_types:environment(),
  Log :: cauder_types:log(),
  Mail :: [cauder_types:message()],
  NewEnvironment :: cauder_types:environment(),
  MatchedBranch :: [cauder_types:abstract_expr()],
  MatchedMessage :: cauder_types:message(),
  RestMessages :: [cauder_types:message()].

match_rec(Cs, Bs0, [{'receive', Stamp} | _], Mail) ->
  case utils:check_msg(Mail, Stamp) of
    none -> nomatch;
    Msg ->
      case match_clause(Bs0, Cs, [abstract(Msg#msg.val)]) of
        {match, Bs, Body} -> {Bs, Body, Msg, lists:delete(Msg, Mail)};
        nomatch -> nomatch
      end
  end;
match_rec(_, _, _, _) -> nomatch.


-spec match_clause(cauder_types:environment(), cauder_types:af_clause_seq(), [cauder_types:af_literal()]) ->
  {match, cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_clause(_, [], _) -> nomatch;
match_clause(Bs0, [{'clause', _, Ps, G, B} | Cs], Vs) ->
  case match(Bs0, Ps, Vs) of
    {match, Bs} ->
      case concrete(eval_guard_seq(Bs, G)) of
        true -> {match, Bs, B};
        false -> match_clause(Bs0, Cs, Vs)
      end;
    nomatch -> match_clause(Bs0, Cs, Vs)
  end.


%% Tries to match a list of values against a list of patterns using the given environment.
%% The list of values should have no variables.

-spec match(cauder_types:environment(), [cauder_types:af_pattern()], [cauder_types:af_literal()]) ->
  {match, cauder_types:environment()} | nomatch.

match(Bs, [], [])    -> {match, Bs};
match(Bs0, [Pat | Ps0], [{value, _, Val} | Vs0]) when length(Ps0) == length(Vs0) ->
  case catch match1(Pat, Val, Bs0) of
    {match, Bs} -> match(Bs, Ps0, Vs0);
    Result -> Result
  end;
match(_Bs, _Ps, _Vs) -> nomatch.


-spec match1(cauder_types:abstract_expr(), term(), cauder_types:environment()) -> {match, cauder_types:environment()} | nomatch.

match1({value, _, V}, V, Bs) ->
  {match, Bs};
match1({var, _, '_'}, _, Bs) ->
  {match, Bs};
match1({var, _, Name}, Term, Bs) ->
  case binding(Name, Bs) of
    {value, Term} -> {match, Bs};
    {value, _} -> throw(nomatch);
    unbound -> {match, orddict:store(Name, Term, Bs)} % Add the new binding
  end;
match1({match, _, Pat1, Pat2}, Term, Bs0) ->
  {match, Bs1} = match1(Pat1, Term, Bs0),
  match1(Pat2, Term, Bs1);
match1({cons, _, H, T}, [H1 | T1], Bs0) ->
  {match, Bs} = match1(H, H1, Bs0),
  match1(T, T1, Bs);
match1({tuple, _, Es}, Tuple, Bs) when length(Es) =:= tuple_size(Tuple) ->
  match_tuple(Es, Tuple, 1, Bs);
match1(_, _, _) ->
  throw(nomatch).


match_tuple([], _, _, Bs) -> {match, Bs};
match_tuple([E | Es], Tuple, I, Bs0) ->
  {match, Bs} = match1(E, element(I, Tuple), Bs0),
  match_tuple(Es, Tuple, I + 1, Bs).


-spec eval_guard_seq(cauder_types:environment(), cauder_types:af_guard_seq()) -> cauder_types:af_boolean().

eval_guard_seq(_, []) -> abstract(true);
eval_guard_seq(Bs, Gs) when is_list(Gs) ->
  % In a guard sequence, guards are evaluated until one is true. The remaining guards, if any, are not evaluated.
  % See: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences
  abstract(lists:any(fun(G) -> concrete(eval_guard(Bs, G)) end, Gs)).


-spec eval_guard(cauder_types:environment(), cauder_types:af_guard()) -> cauder_types:af_boolean().

eval_guard(Bs, G) when is_list(G) ->
  abstract(lists:all(fun(Gt) -> concrete(eval_guard_test(Bs, Gt)) end, G)).


-spec eval_guard_test(cauder_types:environment(), cauder_types:af_guard_test()) -> cauder_types:af_guard_test() | cauder_types:af_boolean().

eval_guard_test(Bs, Gt) ->
  case is_reducible(Gt, Bs) of
    true ->
      #result{exprs = [Gt1]} = expr(Bs, Gt, []),
      eval_guard_test(Bs, Gt1);
    false -> Gt
  end.


%% =====================================================================
%% Converts the given Erlang data structure into an abstract expression.

-spec abstract(term()) -> cauder_types:af_literal().

abstract(Value) -> {value, 0, Value}.


%% =====================================================================
%% Converts the given abstract expression of a term into a conventional Erlang data structure (that is, the term itself).

-spec concrete(cauder_types:af_literal()) -> term().

concrete({value, _, Value})                       -> Value;
concrete({cons, _, {value, _, H}, {value, _, T}}) -> [H | T].


%% =====================================================================
%% @doc Checks if the given abstract expression can be reduced any further or not.

-spec is_reducible(cauder_types:abstract_expr() | [cauder_types:abstract_expr()], cauder_types:environment()) -> boolean().

is_reducible([], _)                 -> false;
is_reducible([E | Es], Bs)          -> is_reducible(E, Bs) orelse is_reducible(Es, Bs);

is_reducible({value, _, _}, _)      -> false;
is_reducible({var, _, '_'}, _)      -> false;
is_reducible({var, _, Name}, Bs)    -> not utils:is_temp_variable_name(Name) andalso binding(Name, Bs) =/= unbound;
is_reducible({cons, _, H, T}, Bs)   -> is_reducible(H, Bs) orelse is_reducible(T, Bs);
is_reducible({tuple, _, Es}, Bs)    -> is_reducible(Es, Bs);
is_reducible(E, _) when is_tuple(E) -> true.


%% =====================================================================
%% @doc Checks if the given abstract expression is a literal value.

-spec is_value(cauder_types:abstract_expr() | [cauder_types:abstract_expr()]) -> boolean().

is_value([])                 -> true;
is_value([E | Es])           -> is_value(E) andalso is_value(Es);

is_value({value, _, _})      -> true;
is_value({cons, _, H, T})    -> is_value(H) andalso is_value(T);
is_value({tuple, _, Es})     -> is_value(Es);
is_value(E) when is_tuple(E) -> false.


%% =====================================================================
%% @doc Returns the binding for the given name in the given environment.

-spec binding(atom(), cauder_types:environment()) -> {value, term()} | unbound.

binding(Name, Bs) -> erl_eval:binding(Name, Bs).


%% =====================================================================
%% @doc Returns the current module according to the stack.

-spec current_module(cauder_types:stack()) -> {ok, atom()} | error.

current_module([{{M, _, _}, _, _, _} | _]) -> {ok, M};
current_module([_ | Stk])                  -> current_module(Stk);
current_module([])                         -> error.


eval_and_update({Bs, Es, Stk}, {Index, Tuple}) when is_list(Es) ->
  R = #result{exprs = Es1} = eval_list(Bs, Es, Stk),
  R#result{exprs = [setelement(Index, Tuple, Es1)]};
eval_and_update({Bs, E, Stk}, {Index, Tuple}) ->
  R = #result{exprs = [E1]} = expr(Bs, E, Stk),
  R#result{exprs = [setelement(Index, Tuple, E1)]}.
