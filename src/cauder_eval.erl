-module(cauder_eval).

-export([expr/2, is_expr/2]).
-export([matchrec/3]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_expr_list(cauder_types:environment(), [cauder_types:abstract_expr()]) -> cauder_types:eval_result().

eval_expr_list(Env, [Expr | Exprs]) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    true ->
      {NewEnv, NewExprs, Label} = expr(Env, Expr),
      {NewEnv, NewExprs ++ Exprs, Label};
    false ->
      {NewEnv, NewExprs, Label} = eval_expr_list(Env, Exprs),
      {NewEnv, [Expr | NewExprs], Label}
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec expr(cauder_types:environment(), cauder_types:abstract_expr()) -> cauder_types:eval_result().

expr(Env, Expr) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    false ->
      % If we find a literal we just consume it.
      {Env, [], tau};
    true ->
      case erl_syntax:type(Expr) of
        variable -> variable(Env, Expr);
        match_expr -> match_expr(Env, Expr);
        infix_expr -> infix_expr(Env, Expr);
        prefix_expr -> prefix_expr(Env, Expr);
        application -> application(Env, Expr);
        list -> list(Env, Expr);
        tuple -> tuple(Env, Expr);
        case_expr -> case_expr(Env, Expr);
        if_expr -> if_expr(Env, Expr);
        receive_expr -> receive_expr(Env, Expr)
      end
  end.


-spec variable(cauder_types:environment(), cauder_types:af_variable()) -> cauder_types:eval_result().

variable(Env, {var, _, Name}) when is_atom(Name) ->
  Value = orddict:fetch(Name, Env),
  AbstractValue = abstract(Value),
  {Env, [AbstractValue], tau}.


-spec match_expr(cauder_types:environment(), cauder_types:af_match(cauder_types:abstract_expr())) -> cauder_types:eval_result().

match_expr(Env, MatchExpr = {match, Line, Pattern, Body}) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], Label} = expr(Env, Pattern),
      {NewEnv, [{match, Line, NewPattern, Body}], Label};
    false ->
      case is_expr(Body, Env) of
        true ->
          {NewEnv, [NewBody], Label} = expr(Env, Body),
          {NewEnv, [{match, Line, Pattern, NewBody}], Label};
        false ->
          % There should be no variables to evaluate so we pass no bindings
          {value, Value, Bindings} = erl_eval:expr(MatchExpr, []),
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, [(abstract(Value))], tau}
      end
  end.


-spec infix_expr(cauder_types:environment(), cauder_types:af_binary_op(cauder_types:abstract_expr())) -> cauder_types:eval_result().

infix_expr(Env, {op, Line, Operator, Left, Right}) when is_atom(Operator) ->
  case is_expr(Left, Env) of
    true ->
      {NewEnv, [NewLeft], Label} = expr(Env, Left),
      {NewEnv, [{op, Line, Operator, NewLeft, Right}], Label};
    false ->
      case {erl_parse:normalise(Left), Operator} of
        {'false', 'andalso'} -> {Env, Left, tau};
        {'true', 'orelse'} -> {Env, Left, tau};
        _ ->
          case is_expr(Right, Env) of
            true ->
              {NewEnv, [NewRight], Label} = expr(Env, Right),
              {NewEnv, [{op, Line, Operator, Left, NewRight}], Label};
            false ->
              case Operator of
                '!' -> {Env, [Right], {send, Left, Right}};
                _ ->
                  % Infix operators are always built-in, so we just evaluate the expression
                  Value = apply(erlang, Operator, [erl_parse:normalise(Left), erl_parse:normalise(Right)]),
                  {Env, [(abstract(Value))], tau}
              end
          end
      end
  end.


-spec prefix_expr(cauder_types:environment(), cauder_types:af_unary_op(cauder_types:abstract_expr())) -> cauder_types:eval_result().

prefix_expr(Env, {op, Pos, Operator, Argument}) when is_atom(Operator) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = expr(Env, Argument),
      {NewEnv, [{op, Pos, Operator, NewArgument}], Label};
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      Value = apply(erlang, Operator, [erl_parse:normalise(Argument)]),
      {Env, [(abstract(Value))], tau}
  end.


-spec application(cauder_types:environment(), cauder_types:af_local_call() | cauder_types:af_remote_call()) -> cauder_types:eval_result().

application(Env, RemoteCall = {call, CallPos, RemoteFun = {remote, RemotePos, Module, Name}, Arguments}) ->
  case is_expr(Module, Env) of
    true ->
      {NewEnv, [NewModule], Label} = expr(Env, Module),
      {NewEnv, [{call, CallPos, {remote, RemotePos, NewModule, Name}, Arguments}], Label};
    false ->
      case is_expr(Name, Env) of
        true ->
          {NewEnv, [NewName], Label} = expr(Env, Name),
          {NewEnv, [{call, CallPos, {remote, RemotePos, Module, NewName}, Arguments}], Label};
        false ->
          case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
            true ->
              {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
              {NewEnv, [{call, CallPos, RemoteFun, NewArguments}], Label};
            false ->
              case erl_syntax:atom_value(Module) of
                'erlang' -> bif(Env, RemoteCall);
                _ ->
                  % TODO Check if module matches current one
                  % TODO Handle calls to functions in other modules
                  error(not_implemented)
              end
          end
      end
  end;
application(Env, LocalCall = {call, Pos, LocalFun, Arguments}) ->
  case is_expr(LocalFun, Env) of
    true ->
      {NewEnv, [NewLocalFun], Label} = expr(Env, LocalFun),
      {NewEnv, [{call, Pos, NewLocalFun, Arguments}], Label};
    false ->
      case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
        true ->
          {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
          {NewEnv, [{call, Pos, LocalFun, NewArguments}], Label};
        false ->
          Name = erl_syntax:atom_value(LocalFun),
          % Check if the function is in this file
          case utils:fundef_lookup({Name, length(Arguments)}, utils:ref_lookup(?FUN_DEFS)) of
            {value, FunDef} ->
              FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
              % There should be no variables to evaluate so we pass no bindings
              {Bindings, Body} = match_clause([], FunClauses, Arguments),
              NewEnv = utils:merge_env(Env, Bindings),
              {NewEnv, Body, tau};
            false ->
              % TODO If the function name was a variable then BIF should not be evaluated
              % TODO Look for function in other files in the same directory
              bif(Env, LocalCall)
          end
      end
  end.


-spec bif(cauder_types:environment(), cauder_types:af_local_call() | cauder_types:af_remote_call()) -> cauder_types:eval_result().

bif(Env, {call, CallPos, {atom, NamePos, Name}, Arguments}) ->
  bif(Env, {call, CallPos, {remote, NamePos, erlang, Name}, Arguments});
bif(Env, {call, _, {remote, _, erlang, 'spawn'}, Arguments}) ->
  TmpVar = utils:temp_variable(),
  case Arguments of
    % TODO erlang:spawn/1,2,4
    % erlang:spawn/3
    [_SpawnModule, SpawnFunction, SpawnArgs] ->
      % TODO Handle calls to functions in other modules
      {Env, [TmpVar], {spawn, {TmpVar, SpawnFunction, erl_syntax:list_elements(SpawnArgs)}}}
  end;
bif(Env, {call, _, {remote, _, erlang, 'self'}, {nil, _}}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {self, TmpVar}};
bif(Env, {call, _, {remote, _, erlang, FunName}, Arguments}) ->
  ConcreteArgs = lists:map(fun erl_syntax:concrete/1, Arguments),
  % BIF so we just evaluate it
  Value = apply(erlang, FunName, ConcreteArgs),
  {Env, [(abstract(Value))], tau}.


-spec list(cauder_types:environment(), cauder_types:af_cons(cauder_types:abstract_expr())) -> cauder_types:eval_result().

list(Env, {cons, Pos, Head, Tail}) ->
  case is_expr(Head, Env) of
    true ->
      {NewEnv, [NewHead], Label} = expr(Env, Head),
      {NewEnv, [{cons, Pos, NewHead, Tail}], Label};
    false ->
      {NewEnv, [NewTail], Label} = expr(Env, Tail),
      {NewEnv, [{cons, Pos, Head, NewTail}], Label}
  end.


-spec tuple(cauder_types:environment(), cauder_types:af_tuple(cauder_types:abstract_expr())) -> cauder_types:eval_result().

tuple(Env, {tuple, Pos, Elements}) when is_list(Elements) ->
  {NewEnv, NewElements, Label} = eval_expr_list(Env, Elements),
  {NewEnv, [{tuple, Pos, NewElements}], Label}.


-spec case_expr(cauder_types:environment(), cauder_types:af_case()) -> cauder_types:eval_result().

case_expr(Env, {'case', Pos, Argument, Clauses}) ->
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = expr(Env, Argument),
      {NewEnv, [{'case', Pos, NewArgument, Clauses}], Label};
    false ->
      {NewEnv, Body} = match_clause(Env, Clauses, [Argument]),
      {NewEnv, Body, tau}
  end.


-spec match_clause(cauder_types:environment(), cauder_types:af_clause_seq(), [cauder_types:abstract_expr()]) ->
  {cauder_types:environment(), [cauder_types:abstract_expr()]} | nomatch.

match_clause(Env, [{'clause', _, Ps, G, B} | Cs], Vs) ->
  case match(Env, Ps, Vs) of
    {match, Env1} ->
      Bool = erl_syntax:atom_value(eval_guard_seq(Env1, G)),
      case Bool of
        true -> {Env1, B};
        false -> match_clause(Env, Cs, Vs)
      end;
    nomatch -> match_clause(Env, Cs, Vs)
  end;
match_clause(_Env, [], _Vs) -> nomatch.


%% Tries to match a list of values against a list of patterns using the given environment.
%% The list of values should have no variables.
%% TODO Allow for variables in list of values

-spec match(cauder_types:environment(), [cauder_types:af_pattern()], [cauder_types:abstract_expr()]) ->
  {match, cauder_types:environment()} | nomatch.

match(Env, [], [])    -> {match, Env};
match(Env, Ps, Vs) when length(Ps) == length(Vs) ->
  Ps1 = eval_pattern(Env, erl_syntax:revert(erl_syntax:tuple(Ps))),
  Vs1 = erl_syntax:revert(erl_syntax:tuple(Vs)),
  try erl_eval:expr({match, erl_anno:new(0), Ps1, Vs1}, []) of
    {value, _Val, Bs} ->
      Env1 = utils:merge_env(Env, Bs),
      {match, Env1}
  catch
    error:{badmatch, _Rhs} -> nomatch
  end;
match(_Env, _Ps, _Vs) -> nomatch.


-spec eval_pattern(cauder_types:environment(), cauder_types:af_pattern()) -> cauder_types:af_pattern().

eval_pattern(Env, Pattern) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], tau} = expr(Env, Pattern),
      eval_pattern(NewEnv, NewPattern);
    false -> Pattern
  end.


-spec eval_guard_seq(cauder_types:environment(), cauder_types:af_guard_seq()) -> cauder_types:af_boolean().

eval_guard_seq(_Env, []) -> abstract(true);
eval_guard_seq(Env, GuardSeq) when is_list(GuardSeq) ->
  % In a guard sequence, guards are evaluated until one is true. The remaining guards, if any, are not evaluated.
  % See: https://erlang.org/doc/reference_manual/expressions.html#guard-sequences
  AnyTrue = lists:any(fun(Guard) -> erl_syntax:atom_value(eval_guard(Env, Guard)) end, GuardSeq),
  abstract(AnyTrue).


-spec eval_guard(cauder_types:environment(), cauder_types:af_guard()) -> cauder_types:af_boolean().

eval_guard(Env, Guard) when is_list(Guard) ->
  AllTrue = lists:all(fun(GuardTest) -> erl_syntax:atom_value(eval_guard_test(Env, GuardTest)) end, Guard),
  {atom, erl_anno:new(0), AllTrue}.


-spec eval_guard_test(cauder_types:environment(), cauder_types:af_guard_test()) -> cauder_types:af_guard_test() | cauder_types:af_boolean().

eval_guard_test(Env, GuardTest) ->
  case erl_lint:is_guard_test(GuardTest) of
    true ->
      case is_expr(GuardTest, Env) of
        true ->
          % Environment should not change, and the label should be `tau`
          {Env, [NewGuardTest], tau} = expr(Env, GuardTest),
          eval_guard_test(Env, NewGuardTest);
        false -> GuardTest
      end;
    false -> erlang:error(guard_expr) % TODO How to handle error in the interpreted code?
  end.


-spec if_expr(cauder_types:environment(), cauder_types:af_if()) -> cauder_types:eval_result().

if_expr(Env, {'if', _, Clauses}) ->
  {NewEnv, Body} = match_clause(Env, Clauses, []),
  {NewEnv, Body, tau}.


-spec receive_expr(cauder_types:environment(), cauder_types:af_receive()) -> cauder_types:eval_result().

%% TODO Support receive with timeout
receive_expr(Env, {'receive', _, Clauses}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {rec, TmpVar, Clauses}}.


-spec abstract(term()) -> cauder_types:abstract_expr().

abstract(Value) -> erl_syntax:revert(erl_syntax:abstract(Value)).


%% =====================================================================
%% @doc Tries to match each message, in time order, in the mailbox against every
%% pattern from the `Clauses`, sequentially. If a match succeeds and the optional
%% guard sequence is `true`, a tuple with the following form is returned:
%% `{NewEnvironment, NewExpression, MatchedMessage, RestMessages}`
%% Otherwise, the atom `nomatch` is returned.

-spec matchrec(Clauses, Mail, Environment) -> {NewEnvironment, MatchedBranch, MatchedMessage, RestMessages} | nomatch when
  Clauses :: cauder_types:af_clause_seq(),
  Mail :: [cauder_types:process_message()],
  Environment :: cauder_types:environment(),
  NewEnvironment :: cauder_types:environment(),
  MatchedBranch :: [cauder_types:abstract_expr()],
  MatchedMessage :: cauder_types:process_message(),
  RestMessages :: [cauder_types:process_message()].

matchrec(Clauses, Mail, Env) -> matchrec(Clauses, Mail, [], Env).


-spec matchrec(Clauses, RemainingMail, CheckedMail, Environment) -> {NewEnvironment, MatchedBranch, MatchedMessage, RestMessages} | nomatch when
  Clauses :: cauder_types:af_clause_seq(),
  RemainingMail :: [cauder_types:process_message()],
  CheckedMail :: [cauder_types:process_message()],
  Environment :: cauder_types:environment(),
  NewEnvironment :: cauder_types:environment(),
  MatchedBranch :: [cauder_types:abstract_expr()],
  MatchedMessage :: cauder_types:process_message(),
  RestMessages :: [cauder_types:process_message()].

matchrec(_Clauses, [], _CheckedMsgs, _Env) -> nomatch;
matchrec(Clauses, [CurMsg | RestMsgs], CheckedMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  case match_clause(Env, Clauses, [MsgValue]) of
    {NewEnv, Body} -> {NewEnv, Body, CurMsg, lists:reverse(CheckedMsgs, RestMsgs)};
    nomatch -> matchrec(Clauses, RestMsgs, [CurMsg | CheckedMsgs], Env)
  end.


%% =====================================================================
%% @doc Checks if the given abstract expression can be reduced any further or not.

-spec is_expr(cauder_types:abstract_expr(), cauder_types:environment()) -> boolean().

is_expr({atom, _, _}, _)      -> false;
is_expr({char, _, _}, _)      -> false;
is_expr({float, _, _}, _)     -> false;
is_expr({integer, _, _}, _)   -> false;
is_expr({nil, _}, _)          -> false;
is_expr({string, _, _}, _)    -> false;
is_expr({var, _, '_'}, _)     -> false;
is_expr({var, _, Name}, Env)  -> erl_eval:binding(Name, Env) =/= unbound;
is_expr({cons, _, H, T}, Env) -> is_expr(H, Env) orelse is_expr(T, Env);
is_expr({tuple, _, Es}, Env)  -> lists:any(fun(E) -> is_expr(E, Env) end, Es);
is_expr(_, _)                 -> true.
