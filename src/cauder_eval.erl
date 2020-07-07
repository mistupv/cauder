-module(cauder_eval).

-export([seq/3, abstract/1, concrete/1, is_expr/2]).
-export([match_rec/3]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_list(cauder_types:environment(), [cauder_types:abstract_expr()], cauder_types:stack()) -> cauder_types:result().

eval_list(Bs, [E | Es], Stk) ->
  case is_expr(E, Bs) of
    true ->
      R = #result{exprs = Es1} = expr(Bs, E, Stk),
      R#result{exprs = Es1 ++ Es};
    false ->
      R = #result{exprs = Es1} = eval_list(Bs, Es, Stk),
      R#result{exprs = [E | Es1]}
  end.


-spec seq(cauder_types:environment(), [cauder_types:abstract_expr()], cauder_types:stack()) -> cauder_types:result().

seq(Bs, [E | Es], Stk) ->
  case is_expr(E, Bs) of
    false ->
      case Es of
        [] ->
          case Stk of
            % Call entry
            [{{_M, _F, _A}, Bs1, Es1, Var} | Stk1] ->
              Es2 = utils:replace_variable(Es1, Var, E),
              #result{env = Bs1, exprs = Es2, stack = Stk1};
            % Block entry
            [{_Type, Es1, Var} | Stk1] ->
              Es2 = utils:replace_variable(Es1, Var, E),
              #result{env = Bs, exprs = Es2, stack = Stk1}
          end;
        _ ->
          #result{env = Bs, exprs = Es, stack = Stk}
      end;
    true ->
      #result{env = Bs1, exprs = Es1, stack = Stk1, label = L} = expr(Bs, E, Stk),
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

expr(Bs, {var, _, Name}, Stk) when is_atom(Name) ->
  {value, Val} = binding(Name, Bs),
  #result{env = Bs, exprs = [abstract(Val)], stack = Stk};

expr(Bs0, E = {match, _, Lhs, Rhs}, Stk) ->
  case is_expr(Lhs, Bs0) of
    true -> eval_and_update({Bs0, Lhs, Stk}, {3, E});
    false ->
      case is_expr(Rhs, Bs0) of
        true -> eval_and_update({Bs0, Rhs, Stk}, {4, E});
        false ->
          {match, Bs} = match(Bs0, [Lhs], [Rhs]),
          #result{env = Bs, exprs = [Rhs], stack = Stk}
      end
  end;

expr(Bs, E = {op, _, Op, Lhs, Rhs}, Stk) when is_atom(Op) ->
  case is_expr(Lhs, Bs) of
    true -> eval_and_update({Bs, Lhs, Stk}, {4, E});
    false ->
      case {concrete(Lhs), Op} of
        {'false', 'andalso'} -> #result{env = Bs, exprs = [Lhs], stack = Stk};
        {'true', 'orelse'} -> #result{env = Bs, exprs = [Lhs], stack = Stk};
        _ ->
          case is_expr(Rhs, Bs) of
            true -> eval_and_update({Bs, Rhs, Stk}, {5, E});
            false ->
              case Op of
                '!' -> #result{env = Bs, exprs = [Rhs], stack = Stk, label = {send, Lhs, Rhs}};
                _ ->
                  % Infix operators are always built-in, so we just evaluate the expression
                  Val = apply(erlang, Op, [concrete(Lhs), concrete(Rhs)]),
                  #result{env = Bs, exprs = [(abstract(Val))], stack = Stk}
              end
          end
      end
  end;

expr(Bs, E = {op, _, Op, Arg}, Stk) when is_atom(Op) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  case is_expr(Arg, Bs) of
    true -> eval_and_update({Bs, Arg, Stk}, {4, E});
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      Val = apply(erlang, Op, [concrete(Arg)]),
      #result{env = Bs, exprs = [abstract(Val)], stack = Stk}
  end;

expr(Bs0, E1 = {call, _, E2 = {remote, _, M0, F0}, As0}, Stk0) ->
  case is_expr(M0, Bs0) of
    true ->
      R = #result{exprs = [M]} = expr(Bs0, M0, Stk0),
      R#result{exprs = [setelement(3, E1, setelement(3, E2, M))]};
    false ->
      case is_expr(F0, Bs0) of
        true ->
          R = #result{exprs = [F]} = expr(Bs0, F0, Stk0),
          R#result{exprs = [setelement(3, E1, setelement(4, E2, F))]};
        false ->
          case lists:any(fun(A) -> is_expr(A, Bs0) end, As0) of
            true ->
              R = #result{exprs = As} = eval_list(Bs0, As0, Stk0),
              R#result{exprs = [setelement(4, E1, As)]};
            false ->
              M = concrete(M0),
              F = concrete(F0),
              remote_call(Bs0, {M, F, As0}, Stk0)
          end
      end
  end;

expr(Bs0, E = {call, _, F0, As0}, Stk0) ->
  case is_expr(F0, Bs0) of
    true -> eval_and_update({Bs0, F0, Stk0}, {3, E});
    false ->
      case lists:any(fun(A) -> is_expr(A, Bs0) end, As0) of
        true -> eval_and_update({Bs0, As0, Stk0}, {4, E});
        false ->
          F = concrete(F0),
          local_call(Bs0, {F, As0}, Stk0)
      end
  end;

expr(Bs, E = {cons, _, H, T}, Stk) ->
  case is_expr(H, Bs) of
    true -> eval_and_update({Bs, H, Stk}, {3, E});
    false -> eval_and_update({Bs, T, Stk}, {4, E})
  end;

expr(Bs, E = {tuple, _, Es0}, Stk) ->
  eval_and_update({Bs, Es0, Stk}, {3, E});

expr(Bs0, E = {'case', _, A, Cs}, Stk0) ->
  case is_expr(A, Bs0) of
    true -> eval_and_update({Bs0, A, Stk0}, {3, E});
    false ->
      {match, Bs, Body} = match_case(Bs0, Cs, A),
      Var = utils:temp_variable(),
      Stk = [{'case', Body, Var} | Stk0],
      #result{env = Bs, exprs = [Var], stack = Stk}
  end;

expr(Bs, {'if', _, Cs}, Stk0) ->
  {match, Body} = match_if(Bs, Cs),
  Var = utils:temp_variable(),
  Stk = [{'if', Body, Var} | Stk0],
  #result{env = Bs, exprs = [Var], stack = Stk};

%% TODO Support receive with timeout
expr(Bs, {'receive', _, Cs}, Stk0) ->
  Var = utils:temp_variable(),
  VarBody = utils:temp_variable(),
  Stk = [{'receive', [VarBody], Var} | Stk0],
  #result{env = Bs, exprs = [Var], stack = Stk, label = {rec, VarBody, Cs}};

expr(Bs, {'fun', Anno, {'clauses', Cs}}, Stk0) ->
  {ok, M} = current_module(Stk0),
  Info = {M, Bs, Cs},
  % TODO Handle calls to interpreted fun() from uninterpreted module
  Fun = fun() -> Info end,
  io:format("fun_info: ~p\n", [erlang:fun_info(Fun)]),
  #result{env = Bs, exprs = [{value, Anno, Fun}], stack = Stk0}.



-spec bif(atom(), [cauder_types:abstract_expr()]) -> {cauder_types:abstract_expr(), cauder_types:label()}.

bif(F, As) -> bif(erlang, F, As).


-spec bif(atom(), atom(), [cauder_types:abstract_expr()]) -> {cauder_types:abstract_expr(), cauder_types:label()}.

% TODO erlang:spawn/1
bif(erlang, spawn, [SpawnM, SpawnF, SpawnAs]) ->
  Var = utils:temp_variable(),
  {Var, {spawn, Var, SpawnM, SpawnF, erl_syntax:list_elements(SpawnAs)}};
bif(erlang, self, []) ->
  Var = utils:temp_variable(),
  {Var, {self, Var}};
bif(M, F, As) ->
  % BIF so we just evaluate it
  As1 = lists:map(fun concrete/1, As),
  Val = apply(M, F, As1),
  {abstract(Val), tau}.


%% =====================================================================


-spec match_if(cauder_types:environment(), cauder_types:af_clause_seq()) ->
  {match, [cauder_types:abstract_expr()]} | nomatch.

match_if(_, []) -> nomatch;
match_if(Bs, [{'clause', _, [], G, B} | Cs]) ->
  {atom, _, Bool} = eval_guard_seq(Bs, G),
  case Bool of
    true -> {match, B};
    false -> match_if(Bs, Cs)
  end.


-spec match_case(cauder_types:environment(), cauder_types:af_clause_seq(), cauder_types:abstract_expr()) ->
  {match, cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_case(Bs, Cs, V) -> match_clause(Bs, Cs, [V]).


-spec match_fun(cauder_types:af_clause_seq(), [cauder_types:abstract_expr()]) ->
  {match, cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_fun(Cs, Vs) -> match_clause([], Cs, Vs).


-spec match_rec(Clauses, Mail, Environment) -> {NewEnvironment, MatchedBranch, MatchedMessage, RestMessages} | nomatch when
  Clauses :: cauder_types:af_clause_seq(),
  Mail :: [cauder_types:process_message()],
  Environment :: cauder_types:environment(),
  NewEnvironment :: cauder_types:environment(),
  MatchedBranch :: [cauder_types:abstract_expr()],
  MatchedMessage :: cauder_types:process_message(),
  RestMessages :: [cauder_types:process_message()].

match_rec(Cs, Mail, Bs) -> match_rec(Cs, Mail, [], Bs).


-spec match_rec(Clauses, RemainingMail, CheckedMail, Environment) -> {NewEnvironment, MatchedBranch, MatchedMessage, RestMessages} | nomatch when
  Clauses :: cauder_types:af_clause_seq(),
  RemainingMail :: [cauder_types:process_message()],
  CheckedMail :: [cauder_types:process_message()],
  Environment :: cauder_types:environment(),
  NewEnvironment :: cauder_types:environment(),
  MatchedBranch :: [cauder_types:abstract_expr()],
  MatchedMessage :: cauder_types:process_message(),
  RestMessages :: [cauder_types:process_message()].

match_rec(_, [], _, _) -> nomatch;
match_rec(Cs, [M | Ms0], Ms1, Bs0) ->
  {Val, _Time} = M,
  case match_clause(Bs0, Cs, [Val]) of
    {match, Bs, Body} -> {Bs, Body, M, lists:reverse(Ms1, Ms0)};
    nomatch -> match_rec(Cs, Ms0, [M | Ms1], Bs0)
  end.


-spec match_clause(cauder_types:environment(), cauder_types:af_clause_seq(), [cauder_types:abstract_expr()]) ->
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
%% TODO Allow for variables in list of values

-spec match(cauder_types:environment(), [cauder_types:af_pattern()], [cauder_types:abstract_expr()]) ->
  {match, cauder_types:environment()} | nomatch.

match(Bs, [], [])    -> {match, Bs};
match(Bs0, [Pat | Ps0], [Val | Vs0]) when length(Ps0) == length(Vs0) ->
  case catch match1(Pat, Val, Bs0) of
    {match, Bs} -> match(Bs, Ps0, Vs0);
    Result -> Result
  end;
match(_Bs, _Ps, _Vs) -> nomatch.


-spec match1(cauder_types:abstract_expr(), cauder_types:abstract_expr(), cauder_types:environment()) -> {match, cauder_types:environment()} | nomatch.

match1({var, _, '_'}, _, Bs) -> {match, Bs};
match1({var, _, Name}, E, Bs) ->
  case binding(Name, Bs) of
    {value, Val} ->
      case concrete(E) of
        Val -> {match, Bs};
        _ -> throw(nomatch)
      end;
    unbound ->
      {match, orddict:store(Name, concrete(E), Bs)} % Add the new binding
  end;
match1({match, _, Pat1, Pat2}, Term, Bs0) ->
  {match, Bs1} = match1(Pat1, Term, Bs0),
  match1(Pat2, Term, Bs1);
match1({cons, _, H0, T0}, {cons, _, H1, T1}, Bs0) ->
  {match, Bs} = match1(H0, H1, Bs0),
  match1(T0, T1, Bs);
match1({tuple, _, Es0}, {tuple, _, Es1}, Bs)
  when length(Es0) =:= length(Es1) ->
  match_elements(Es0, Es1, Bs);
match1(_, _, _) ->
  throw(nomatch).


match_elements([E0 | Es0], [E1 | Es1], Bs0) ->
  {match, Bs} = match1(E0, E1, Bs0),
  match_elements(Es0, Es1, Bs);
match_elements([], [], Bs) -> {match, Bs};
match_elements(_, _, _)    -> throw(nomatch).


-spec eval_pattern(cauder_types:environment(), cauder_types:af_pattern()) -> cauder_types:af_pattern().

eval_pattern(Bs0, Pt0) ->
  case is_expr(Pt0, Bs0) of
    true ->
      #result{env = Bs, exprs = [Pt]} = expr(Bs0, Pt0, []),
      eval_pattern(Bs, Pt);
    false -> Pt0
  end.


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
  case erl_lint:is_guard_test(Gt) of
    true ->
      case is_expr(Gt, Bs) of
        true ->
          #result{exprs = [Gt1]} = expr(Bs, Gt, []),
          eval_guard_test(Bs, Gt1);
        false -> Gt
      end;
    false -> erlang:error(guard_expr) % TODO How to handle error in the interpreted code?
  end.


-spec remote_call(cauder_types:environment(), {atom(), atom(), [cauder_types:abstract_expr()]}, cauder_types:stack()) -> cauder_types:result().

remote_call(Bs0, {M, F, As}, Stk0) ->
  A = length(As),
  case utils:fundef_lookup(M, F, A) of
    {ok, FunDef} ->
      Cs = erl_syntax:function_clauses(utils:fundef_rename(FunDef)), % TODO Is necessary to rename?
      % There should be no variables to evaluate so we pass no bindings
      {match, Bs, Body} = match_fun(Cs, As),
      Var = utils:temp_variable(),
      Stk = [{{M, F, A}, Bs, Body, Var} | Stk0],
      #result{env = Bs0, exprs = [Var], stack = Stk};
    error ->
      % TODO If the function name was a variable then BIF should not be evaluated
      % TODO Look for function in other files in the same directory
      {Val, L} = bif(M, F, As),
      #result{env = Bs0, exprs = [Val], stack = Stk0, label = L}
  end.


-spec local_call(cauder_types:environment(), {atom(), [cauder_types:abstract_expr()]}, cauder_types:stack()) -> cauder_types:result().

% Special case for guard calls
local_call(Bs0, {F, As}, []) ->
  {Val, tau} = bif(F, As),
  #result{env = Bs0, exprs = [Val], stack = []};
local_call(Bs0, {Fun, As}, Stk0) when is_function(Fun) ->
  {env, [{M, Bs, Cs}]} = erlang:fun_info(Fun, env),
  {name, F} = erlang:fun_info(Fun, name),
  A = length(As),
  {match, Bs1, Body} = match_fun(Cs, As),
  Var = utils:temp_variable(),
  Stk = [{{M, F, A}, utils:merge_env(Bs, Bs1), Body, Var} | Stk0],
  #result{env = Bs0, exprs = [Var], stack = Stk};
local_call(Bs0, {F, As}, Stk0) ->
  {ok, M} = current_module(Stk0),
  A = length(As),
  case utils:fundef_lookup(M, F, A) of
    {ok, FunDef} ->
      Cs = erl_syntax:function_clauses(utils:fundef_rename(FunDef)), % TODO Is necessary to rename?
      {match, Bs, Body} = match_fun(Cs, As),
      Var = utils:temp_variable(),
      Stk = [{{M, F, A}, Bs, Body, Var} | Stk0],
      #result{env = Bs0, exprs = [Var], stack = Stk};
    error ->
      % TODO If the function name was a variable then BIF should not be evaluated
      % TODO Look for function in other files in the same directory
      {Val, L} = bif(F, As),
      #result{env = Bs0, exprs = [Val], stack = Stk0, label = L}
  end.


%% =====================================================================
%% Converts the given Erlang data structure into an abstract expression.

-spec abstract(term()) -> cauder_types:abstract_expr().

abstract(Fun) when is_function(Fun) -> {value, erl_anno:new(0), Fun};
abstract(Value)                     -> erl_syntax:revert(erl_syntax:abstract(Value)).


%% =====================================================================
%% Converts the given abstract expression of a term into a conventional Erlang data structure (that is, the term itself).

-spec concrete(cauder_types:abstract_expr()) -> term().

concrete({value, _, Val}) -> Val;
concrete(Value)           -> erl_syntax:concrete(Value).


%% =====================================================================
%% @doc Checks if the given abstract expression can be reduced any further or not.

-spec is_expr(cauder_types:abstract_expr(), cauder_types:environment()) -> boolean().

is_expr({atom, _, _}, _)     -> false;
is_expr({char, _, _}, _)     -> false;
is_expr({float, _, _}, _)    -> false;
is_expr({integer, _, _}, _)  -> false;
is_expr({nil, _}, _)         -> false;
is_expr({string, _, _}, _)   -> false;
is_expr({var, _, '_'}, _)    -> false;
is_expr({var, _, Name}, Bs)  -> binding(Name, Bs) =/= unbound;
is_expr({cons, _, H, T}, Bs) -> is_expr(H, Bs) orelse is_expr(T, Bs);
is_expr({tuple, _, Es}, Bs)  -> lists:any(fun(E) -> is_expr(E, Bs) end, Es);
is_expr({value, _, _}, _)    -> false;
is_expr(_, _)                -> true.


%% =====================================================================
%% @doc Returns the binding for the given name in the given environment.

-spec binding(atom(), cauder_types:environment()) -> {value, term()} | unbound.

binding(Name, Bs) -> erl_eval:binding(Name, Bs).


%% =====================================================================
%% @doc Returns the current module according to the stack.

-spec current_module(cauder_types:stack()) -> atom().

current_module([{{M, _, _}, _, _, _} | _]) -> {ok, M};
current_module([_ | Stk])                  -> current_module(Stk);
current_module([])                         -> error.


eval_and_update({Bs, Es, Stk}, {Index, Tuple}) when is_list(Es) ->
  R = #result{exprs = Es1} = eval_list(Bs, Es, Stk),
  R#result{exprs = [setelement(Index, Tuple, Es1)]};
eval_and_update({Bs, E, Stk}, {Index, Tuple}) ->
  R = #result{exprs = [E1]} = expr(Bs, E, Stk),
  R#result{exprs = [setelement(Index, Tuple, E1)]}.
