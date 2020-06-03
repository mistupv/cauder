%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).
-export([eval_step/2, eval_sched/2]).
-export([eval_opts/1, eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").


% TODO Add other labels
% TODO Maybe use concrete values?
-type label() :: tau
               | {spawn, {{'var', erl_anno:anno(), atom()}, {'atom', erl_anno:anno(), atom()}, [erl_parse:abstract_expr()]}}
               | {self, {'var', erl_anno:anno(), atom()}}
               | {send, {'integer', erl_anno:anno(), non_neg_integer()}, erl_parse:abstract_expr()}
               | {rec, {'var', erl_anno:anno(), atom()}, [erl_parse:abstract_clause()]}.


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_expr_list(Environment, Expressions) -> {NewEnvironment, NewExpressions, Label} when
  Environment :: erl_eval:binding_struct(),
  Expressions :: [erl_parse:abstract_expr()],
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpressions :: [erl_parse:abstract_expr()],
  Label :: label().

eval_expr_list(Env, [Expr | Exprs]) when is_tuple(Expr) ->
  case is_expr(Env, Expr) of
    true ->
      {NewEnv, NewExpr, Label} = eval_expr(Env, Expr),
      {NewEnv, [NewExpr | Exprs], Label};
    false ->
      {NewEnv, NewExpr, Label} = eval_expr_list(Env, Exprs),
      {NewEnv, [Expr | NewExpr], Label}
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec eval_expr(Environment, Expression) -> {NewEnvironment, NewExpressions, Label} when
  Environment :: erl_eval:binding_struct(),
  Expression :: erl_syntax:syntaxTree(),
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpressions :: erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()],
  Label :: label().

eval_expr(Env, Expr) ->
  case is_expr(Env, Expr) of
    false ->
      % If we find a literal we just consume it.
      {Env, [], tau};
    true ->
      case erl_syntax:type(Expr) of
        variable ->
          eval_variable(Env, Expr);
        match_expr ->
          eval_match_expr(Env, Expr);
        infix_expr ->
          eval_infix_expr(Env, Expr);
        prefix_expr ->
          eval_prefix_expr(Env, Expr);
        application ->
          eval_application(Env, Expr);
        list ->
          eval_list(Env, Expr);
        tuple ->
          eval_tuple(Env, Expr);
        case_expr ->
          eval_case_expr(Env, Expr);
        disjunction ->
          eval_disjunction(Env, Expr);
        conjunction ->
          eval_conjunction(Env, Expr);
        receive_expr ->
          eval_receive(Env, Expr)
      end
  end.


-spec eval_variable(Environment, Variable) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  Variable :: erl_parse:abstract_expr(), % erl_parse:af_variable()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: tau.

eval_variable(Env, {var, _Pos, Name}) when is_atom(Name) ->
  Binding = orddict:fetch(Name, Env),
  NewValue = erl_syntax:revert(erl_syntax:abstract(Binding)),
  {Env, NewValue, tau}.


-spec eval_match_expr(Environment, MatchExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  MatchExpression :: erl_parse:abstract_expr(), % erl_parse:af_match(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_match_expr(Env, MatchExpr = {match, _Pos, Pattern, Body}) ->
  case is_expr(Env, Pattern) of
    true ->
      {NewEnv, NewPattern, Label} = eval_expr(Env, Pattern),
      % Structure: {match, Pos, Pattern, Body}
      NewMatchExp = setelement(3, MatchExpr, NewPattern),
      {NewEnv, NewMatchExp, Label};
    false ->
      case is_expr(Env, Body) of
        true ->
          {NewEnv, NewBody, Label} = eval_expr(Env, Body),
          % Structure: {match, Pos, Pattern, Body}
          NewMatchExp = setelement(4, MatchExpr, NewBody),
          {NewEnv, NewMatchExp, Label};
        false ->
          % There should be no variables to evaluate so we pass no bindings
          {value, Value, Bindings} = erl_eval:expr(MatchExpr, []),
          NewEnv = utils:merge_env(Env, Bindings),
          % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
          % of `erl_parser:abstract/1` to avoid problems with lists being
          % represented as strings
          NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
          {NewEnv, NewValue, tau}
      end
  end.


-spec eval_infix_expr(Environment, InfixExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  InfixExpression :: erl_parse:abstract_expr(), % erl_parse:af_binary_op(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_infix_expr(Env, InfixExpr = {op, _Pos, Operator, Left, Right}) when is_atom(Operator) ->
  case is_expr(Env, Left) of
    true ->
      {NewEnv, NewLeft, Label} = eval_expr(Env, Left),
      % Structure: {op, Pos, Operator, Left, Right}
      NewInfixExpr = setelement(4, InfixExpr, NewLeft),
      {NewEnv, NewInfixExpr, Label};
    false ->
      case {erl_parse:normalise(Left), Operator} of
        {'false', 'andalso'} ->
          {Env, Left, tau};
        {'true', 'orelse'} ->
          {Env, Left, tau};
        _ ->
          case is_expr(Env, Right) of
            true ->
              {NewEnv, NewRight, Label} = eval_expr(Env, Right),
              % Structure: {op, Pos, Operator, Left, Right}
              NewInfixExpr = setelement(5, InfixExpr, NewRight),
              {NewEnv, NewInfixExpr, Label};
            false ->
              case Operator of
                '!' ->
                  {Env, Right, {send, Left, Right}};
                _ ->
                  % Infix operators are always built-in, so we just evaluate the expression
                  Value = apply(erlang, Operator, [erl_parse:normalise(Left), erl_parse:normalise(Right)]),
                  % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
                  % of `erl_parser:abstract/1` to avoid problems with lists being
                  % represented as strings
                  NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
                  {Env, NewValue, tau}
              end
          end
      end
  end.


-spec eval_prefix_expr(Environment, PrefixExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  PrefixExpression :: erl_parse:abstract_expr(), % erl_parse:af_unary_op(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_prefix_expr(Env, PrefixExpr = {op, _Pos, Operator, Argument}) when is_atom(Operator) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  case is_expr(Env, Argument) of
    true ->
      {NewEnv, NewArg, Label} = eval_expr(Env, Argument),
      % Structure: {op, Pos, Operator, Arg}
      NewPrefixExpr = setelement(4, PrefixExpr, NewArg),
      {NewEnv, NewPrefixExpr, Label};
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      Value = apply(erlang, Operator, [erl_parse:normalise(Argument)]),
      % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
      % of `erl_parser:abstract/1` to avoid problems with lists being
      % represented as strings
      NewValue = erl_syntax:revert(erl_syntax:abstract(Value)),
      {Env, NewValue, tau}
  end.


-spec eval_application(Environment, CallExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  CallExpression :: erl_parse:abstract_expr(), % erl_parse:af_local_call() | erl_parse:af_remote_call()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()],
  Label :: label().

eval_application(Env, CallExpr = {call, _CallPos, RemoteExpr = {remote, _RemotePos, Module, Name}, Arguments}) ->
  case is_expr(Env, Module) of
    true ->
      {NewEnv, NewModule, Label} = eval_expr(Env, Module),
      % Structure: {remote, Pos, Module, Arg}
      NewRemoteExpr = setelement(3, RemoteExpr, NewModule),
      % Structure: {call, Pos, Operator, Args}
      NewCallExpr = setelement(3, CallExpr, NewRemoteExpr),
      {NewEnv, NewCallExpr, Label};
    false ->
      case is_expr(Env, Name) of
        true ->
          {NewEnv, NewName, Label} = eval_expr(Env, Name),
          % Structure: {remote, Pos, Module, Arg}
          NewRemoteExpr = setelement(4, RemoteExpr, NewName),
          % Structure: {call, Pos, Operator, Args}
          NewCallExpr = setelement(3, CallExpr, NewRemoteExpr),
          {NewEnv, NewCallExpr, Label};
        false ->
          case lists:any(fun(Arg) -> is_expr(Env, Arg) end, Arguments) of
            true ->
              {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
              % Structure: {call, Pos, Operator, Args}
              NewCallExpr = setelement(4, CallExpr, NewArguments),
              {NewEnv, NewCallExpr, Label};
            false ->
              case erl_syntax:atom_value(Module) of
                'erlang' ->
                  eval_bif(Env, CallExpr);
                _ ->
                  % TODO Check if module matches current one
                  % TODO Handle calls to functions in other modules
                  % There should be no variables to evaluate so we pass no bindings
                  {value, Value, _} = erl_eval:expr(CallExpr, []),
                  % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
                  % of `erl_parser:abstract/1` to avoid problems with lists being
                  % represented as strings
                  NewExpr = erl_syntax:revert(erl_syntax:abstract(Value)),
                  {Env, NewExpr, tau}
              end
          end
      end
  end;
eval_application(Env, CallExpr = {call, _CallPos, Operator, Arguments}) ->
  case is_expr(Env, Operator) of
    true ->
      {NewEnv, NewOperator, Label} = eval_expr(Env, Operator),
      % Structure: {call, Pos, Operator, Args}
      NewCallExpr = setelement(3, CallExpr, NewOperator),
      {NewEnv, NewCallExpr, Label};
    false ->
      case lists:any(fun(Arg) -> is_expr(Env, Arg) end, Arguments) of
        true ->
          {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
          % Structure: {call, Pos, Operator, Args}
          NewExpr = setelement(4, CallExpr, NewArguments),
          {NewEnv, NewExpr, Label};
        false ->
          Name = erl_syntax:atom_value(Operator),
          % Check if the function is in this file
          case utils:fundef_lookup(Name, length(Arguments), ref_lookup(?FUN_DEFS)) of
            {value, FunDef} ->
              FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
              % There should be no variables to evaluate so we pass no bindings
              % TODO `match_clause` looks like an internal function, because it is not documented
              {Body, Bindings} = erl_eval:match_clause(FunClauses, Arguments, [], none),
              % The environment stores the literal value but `match_clause`
              % returns the bindings as `erl_parse` nodes so we convert them
              ValueBindings = [{Var, erl_syntax:concrete(Val)} || {Var, Val} <- Bindings],
              NewEnv = utils:merge_env(Env, ValueBindings),
              {NewEnv, Body, tau};
            false ->
              % TODO If the function name was a variable then BIF should not be evaluated
              % TODO Look for function in other files in the same directory
              eval_bif(Env, CallExpr)
          end
      end
  end.


-spec eval_bif(Environment, CallExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  CallExpression :: erl_parse:abstract_expr(), % erl_parse:af_local_call() | erl_parse:af_remote_call()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_bif(Env, {call, CallPos, {atom, AtomPos, FunName}, Arguments}) when is_atom(FunName) ->
  eval_bif(Env, {call, CallPos, {remote, AtomPos, erlang, FunName}, Arguments});
eval_bif(Env, {call, _CallPos, {remote, _RemotePos, erlang, 'spawn'}, Arguments}) ->
  TmpVar = utils:temp_variable(),
  case Arguments of
    % TODO erlang:spawn/1,2,4
    % erlang:spawn/3
    [_SpawnModule, SpawnFunction, SpawnArgs] ->
      % TODO Handle calls to functions in other modules
      {Env, TmpVar, {spawn, {TmpVar, SpawnFunction, erl_syntax:list_elements(SpawnArgs)}}}
  end;
eval_bif(Env, {call, _CallPos, {remote, _RemotePos, erlang, 'self'}, {nil, _NilPos}}) ->
  TmpVar = utils:temp_variable(),
  {Env, TmpVar, {self, TmpVar}};
eval_bif(Env, {call, _CallPos, {remote, _RemotePos, erlang, FunName}, Arguments}) when is_atom(FunName) ->
  ConcreteArgs = lists:map(fun erl_syntax:concrete/1, Arguments),
  % BIF so we just evaluate it
  Value = apply(erlang, FunName, ConcreteArgs),
  % Use `erl_syntax:abstract/1` and `erl_syntax:revert/1` instead
  % of `erl_parser:abstract/1` to avoid problems with lists being
  % represented as strings
  NewExpr = erl_syntax:revert(erl_syntax:abstract(Value)),
  {Env, NewExpr, tau}.


-spec eval_list(Environment, List) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  List :: erl_parse:abstract_expr(), % erl_parse:af_cons(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_list(Env, List = {cons, _Pos, Head, Tail}) ->
  case is_expr(Env, Head) of
    true ->
      {NewEnv, NewHead, Label} = eval_expr(Env, Head),
      % Structure: {cons, Pos, Head, Tail}
      NewList = setelement(3, List, NewHead),
      {NewEnv, NewList, Label};
    false ->
      % `list_tail` returns a syntax tree but we want an `erl_parse` node
      %Tail = erl_syntax:revert(erl_syntax:list_tail(List)),
      {NewEnv, NewTail, Label} = eval_expr(Env, Tail),
      % Structure: {cons, Pos, Head, Tail}
      NewList = setelement(4, List, NewTail),
      {NewEnv, NewList, Label}
  end.


-spec eval_tuple(Environment, Tuple) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  Tuple :: erl_parse:abstract_expr(), % erl_parse:af_tuple(erl_parse:abstract_expr())
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: label().

eval_tuple(Env, Tuple = {tuple, _Pos, Elements}) ->
  {NewEnv, NewElements, Label} = eval_expr_list(Env, Elements),
  % Structure: {tuple, Pos, Elements}
  NewTuple = setelement(3, Tuple, NewElements),
  {NewEnv, NewTuple, Label}.


-spec eval_case_expr(Environment, CaseExpression) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  CaseExpression :: erl_parse:abstract_expr(), % erl_parse:af_case()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_syntax:syntaxTree() | [erl_syntax:syntaxTree()],
  Label :: label().

eval_case_expr(Env, CaseExpr = {'case', _Pos, Argument, Clauses}) ->
  case is_expr(Env, Argument) of
    true ->
      {NewEnv, NewArgument, Label} = eval_expr(Env, Argument),
      % Structure: {'case', Pos, Argument, Clauses}
      NewCaseExpr = setelement(3, CaseExpr, NewArgument),
      {NewEnv, NewCaseExpr, Label};
    false ->
      {NewEnv, Body} = match_clause(Env, Clauses, Argument),
      {NewEnv, Body, tau}
  end.


-spec match_clause(Environment, Clauses, Value) -> Match | nomatch when
  Environment :: erl_eval:binding_struct(),
  Clauses :: [erl_parse:abstract_clause()],
  Value :: erl_parse:abstract_expr(),
  NewEnvironment :: erl_eval:binding_struct(),
  Body :: [erl_parse:abstract_expr()],
  Match :: {NewEnvironment, Body}.

match_clause(Env, Clauses, Value) ->
  try
    lists:foreach(
      fun(Clause) ->
        % `case` and `receive` branches should have one pattern only
        [Pattern] = erl_syntax:clause_patterns(Clause),
        NewPattern = eval_pattern(Env, Pattern),
        MatchExpr = erl_syntax:revert(erl_syntax:match_expr(NewPattern, Value)),
        try erl_eval:expr(MatchExpr, []) of
          {value, _Value, Bindings} ->
            NewEnv = utils:merge_env(Env, Bindings),
            case erl_syntax:clause_guard(Clause) of
              none ->
                Body = erl_syntax:clause_body(Clause),
                throw({NewEnv, Body});
              Guard ->
                NewGuard = eval_guard(NewEnv, Guard),
                case erl_syntax:atom_value(NewGuard) of
                  true ->
                    Body = erl_syntax:clause_body(Clause),
                    throw({NewEnv, Body});
                  false -> continue
                end
            end
        catch
          % Pattern doesn't match
          error:{badmatch, _Rhs} -> continue
        end
      end,
      Clauses
    ),
    nomatch
  catch
    throw:Match -> Match
  end.


-spec eval_pattern(Environment, PatternExpression) -> NewExpression when
  Environment :: erl_eval:binding_struct(),
  PatternExpression :: erl_syntax:syntaxTree(), % erl_parse:af_pattern()
  NewExpression :: erl_syntax:syntaxTree().

eval_pattern(Env, Pattern) ->
  case is_expr(Env, Pattern) of
    true ->
      {NewEnv, NewPattern, tau} = eval_expr(Env, Pattern),
      eval_pattern(NewEnv, NewPattern);
    false ->
      Pattern
  end.


-spec eval_guard(Environment, GuardExpression) -> NewExpression when
  Environment :: erl_eval:binding_struct(),
  GuardExpression :: erl_syntax:syntaxTree(), % erl_parse:af_guard_seq()
  NewExpression :: erl_syntax:syntaxTree().

eval_guard(Env, Guard) ->
  case is_expr(Env, Guard) of
    true ->
      {NewEnv, NewGuard, tau} = eval_expr(Env, Guard),
      eval_guard(NewEnv, NewGuard);
    false ->
      [Elem] = erl_syntax:disjunction_body(Guard),
      case erl_syntax:type(Elem) of
        conjunction ->
          hd(erl_syntax:conjunction_body(Elem));
        atom ->
          Elem
      end
  end.


-spec eval_disjunction(Environment, Disjunction) -> {NewEnvironment, NewDisjunction, Label} when
  Environment :: erl_eval:binding_struct(),
  Disjunction :: erl_syntax:syntaxTree(),
  NewEnvironment :: erl_eval:binding_struct(),
  NewDisjunction :: erl_syntax:syntaxTree(),
  Label :: label().

eval_disjunction(Env, Disjunction) ->
  Body = erl_syntax:disjunction_body(Disjunction),
  case lists:any(fun(Elem) -> is_expr(Env, Elem) end, Body) of
    true ->
      {NewEnv, NewBody, Label} = eval_expr_list(Env, Body),
      NewDisjunction = erl_syntax:copy_pos(Disjunction, erl_syntax:disjunction(NewBody)),
      {NewEnv, NewDisjunction, Label};
    false ->
      [LeftConjunction, RightConjunction | Other] = Body,

      [Left] = erl_syntax:conjunction_body(LeftConjunction),
      [Right] = erl_syntax:conjunction_body(RightConjunction),

      LeftBool = erl_syntax:atom_value(Left), % true | false
      RightBool = erl_syntax:atom_value(Right), % true | false

      NewValue = erl_parse:abstract(LeftBool or RightBool),
      NewDisjunction = erl_syntax:copy_pos(Disjunction, erl_syntax:disjunction([NewValue | Other])),
      {Env, NewDisjunction, tau}
  end.


-spec eval_conjunction(Environment, Conjunction) -> {NewEnvironment, NewConjunction, Label} when
  Environment :: erl_eval:binding_struct(),
  Conjunction :: erl_syntax:syntaxTree(),
  NewEnvironment :: erl_eval:binding_struct(),
  NewConjunction :: erl_syntax:syntaxTree(),
  Label :: label().

eval_conjunction(Env, Conjunction) ->
  Body = erl_syntax:conjunction_body(Conjunction),
  case lists:any(fun(Elem) -> is_expr(Env, Elem) end, Body) of
    true ->
      {NewEnv, NewBody, Label} = eval_expr_list(Env, Body),
      NewConjunction = erl_syntax:copy_pos(Conjunction, erl_syntax:conjunction(NewBody)),
      {NewEnv, NewConjunction, Label};
    false ->
      [Left, Right | Other] = Body,

      LeftBool = erl_syntax:atom_value(Left), % true | false
      RightBool = erl_syntax:atom_value(Right), % true | false

      NewValue = erl_parse:abstract(LeftBool and RightBool),
      NewConjunction = erl_syntax:copy_pos(Conjunction, erl_syntax:conjunction([NewValue | Other])),
      {Env, NewConjunction, tau}
  end.


-spec eval_receive(Environment, ReceiveExpr) -> {NewEnvironment, NewExpression, Label} when
  Environment :: erl_eval:binding_struct(),
  ReceiveExpr :: erl_parse:abstract_expr(), % erl_parse:af_receive()
  NewEnvironment :: erl_eval:binding_struct(),
  NewExpression :: erl_parse:abstract_expr(),
  Label :: {rec, {'var', erl_anno:anno(), atom()}, [erl_parse:abstract_clause()]}.

%% TODO Support receive with timeout
eval_receive(Env, _ReceiveExpr = {'receive', _Pos, Clauses}) ->
  TmpVar = utils:temp_variable(),
  {Env, TmpVar, {rec, TmpVar, Clauses}}.


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(System, Pid) -> NewSystem when
  System :: #sys{},
  Pid :: {'integer', erl_anno:anno(), non_neg_integer()},
  NewSystem :: #sys{}.

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exp = Exprs, mail = Mail} = Proc,
  [CurExpr | RestExpr] = Exprs,
  {NewEnv, NewExpr, Label} = eval_expr(Env, CurExpr),
  io:format("eval_step:\n\tNewEnv: ~p\n\tNewExpr: ~p\n\tLabel: ~p\n", [NewEnv, NewExpr, Label]),
  case Label of
    tau ->
      NewProc = Proc#proc{
        hist = [{tau, Env, Exprs} | Hist],
        env  = NewEnv,
        exp  = lists:flatten([NewExpr], RestExpr) % FIXME NewExp can be a list or not
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {self, TmpVar} ->
      RepExpr = utils:replace_variable(TmpVar, Pid, lists:flatten([NewExpr])),

      NewProc = Proc#proc{
        hist = [{self, Env, Exprs} | Hist],
        env  = NewEnv,
        exp  = lists:flatten([RepExpr], RestExpr)
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {send, DestPid, MsgValue} ->
      Time = utils:fresh_time(),
      NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},

      NewProc = Proc#proc{
        hist = [{send, Env, Exprs, DestPid, {MsgValue, Time}} | Hist],
        env  = NewEnv,
        exp  = lists:flatten([NewExpr], RestExpr)
      },
      TraceItem = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = DestPid,
        val  = MsgValue,
        time = Time
      },
      System#sys{
        msgs  = [NewMsg | Msgs],
        procs = [NewProc | RestProcs],
        trace = [TraceItem | Trace]
      };
    {spawn, {TmpVar, FunName, FunArgs}} ->
      SpawnPid = erl_parse:abstract(utils:fresh_pid()),
      SpawnProc = #proc{
        pid = SpawnPid,
        exp = [erl_syntax:revert(erl_syntax:application(FunName, FunArgs))],
        spf = {erl_syntax:atom_value(FunName), length(FunArgs)}
      },

      RepExpr = utils:replace_variable(TmpVar, SpawnPid, lists:flatten([NewExpr])),

      NewProc = Proc#proc{
        hist = [{spawn, Env, Exprs, SpawnPid} | Hist],
        env  = NewEnv,
        exp  = lists:flatten([RepExpr], RestExpr)
      },
      TraceItem = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      System#sys{
        procs = [NewProc | [SpawnProc | RestProcs]],
        trace = [TraceItem | Trace]
      };
    {rec, TmpVar, ReceiveClauses} ->
      {Bindings, RecExp, ConsMsg, NewMail} = matchrec(ReceiveClauses, Mail, NewEnv),
      {MsgValue, Time} = ConsMsg,

      RepExpr = utils:replace_variable(TmpVar, RecExp, lists:flatten([NewExpr])),

      NewProc = Proc#proc{
        hist = [{rec, Env, Exprs, ConsMsg, Mail} | Hist],
        env  = utils:merge_env(NewEnv, Bindings),
        exp  = lists:flatten([RepExpr], RestExpr),
        mail = NewMail
      },
      TraceItem = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = MsgValue,
        time = Time
      },
      System#sys{
        procs = [NewProc | RestProcs],
        trace = [TraceItem | Trace]
      }
  end.


%% =====================================================================
%% @doc Performs an evaluation step in message Id, given System

eval_sched(System, Id) ->
  #sys{procs = Procs, msgs = Msgs} = System,
  {Msg, RestMsgs} = utils:select_msg(Msgs, Id),
  #msg{dest = DestPid, val = Value, time = Id} = Msg,
  {Proc, RestProcs} = utils:select_proc(Procs, DestPid),
  Mail = Proc#proc.mail,
  NewMail = Mail ++ [{Value, Id}],
  NewProc = Proc#proc{mail = NewMail},
  System#sys{msgs = RestMsgs, procs = [NewProc | RestProcs]}.


%% =====================================================================
%% @doc Checks if the given Expression can is an expression or not

-spec is_expr(Environment, Expression) -> boolean() when
  Environment :: erl_eval:binding_struct(),
  Expression :: erl_syntax:syntaxTree().

is_expr(Env, Expr) when is_tuple(Expr) ->
  case erl_syntax:type(Expr) of
    atom ->
      false;
    integer ->
      false;
    float ->
      false;
    char ->
      false;
    string ->
      false;
    nil ->
      false;
    list ->
      is_expr(Env, erl_syntax:list_head(Expr)) orelse is_expr(Env, erl_syntax:list_tail(Expr));
    tuple ->
      lists:any(fun(Elem) -> is_expr(Env, Elem) end, erl_syntax:tuple_elements(Expr));
    variable ->
      Name = erl_syntax:variable_name(Expr),
      erl_eval:binding(Name, Env) =/= unbound;
    underscore ->
      false;
    disjunction ->
      Body = erl_syntax:disjunction_body(Expr),
      length(Body) > 1 orelse lists:any(fun(Elem) -> is_expr(Env, Elem) end, Body);
    conjunction ->
      Body = erl_syntax:conjunction_body(Expr),
      length(Body) > 1 orelse lists:any(fun(Elem) -> is_expr(Env, Elem) end, Body);
    _ ->
      true
  end.


%% =====================================================================
%% @doc Tries to match each message, in time order, in the mailbox against every
%% pattern from the `Clauses`, sequentially. If a match succeeds and the optional
%% guard sequence is `true`, a tuple with the following form is returned:
%% `{NewEnvironment, NewExpression, MatchedMessage, RestMessages}`
%% Otherwise, the atom `nomatch` is returned.

-spec matchrec(Clauses, Mail, Environment) -> nomatch | Match when
  Clauses :: [erl_parse:abstract_clause()], % erl_parse:af_clause_seq()
  Mail :: [{erl_parse:abstract_expr(), non_neg_integer()}],
  Environment :: erl_eval:binding_struct(),
  NewEnvironment :: erl_eval:binding_struct(),
  MatchedBody :: [erl_parse:abstract_expr()],
  MatchedMessage :: {erl_parse:abstract_expr(), non_neg_integer()},
  RestMessages :: [{erl_parse:abstract_expr(), non_neg_integer()}],
  Match :: {NewEnvironment, MatchedBody, MatchedMessage, RestMessages}.

matchrec(Clauses, Mail, Env) ->
  matchrec(Clauses, Mail, [], Env).


-spec matchrec(Clauses, RemainingMessages, CheckedMessages, Environment) -> Match | nomatch when
  Clauses :: [erl_parse:abstract_clause()], % erl_parse:af_clause_seq()
  RemainingMessages :: [{erl_parse:abstract_expr(), non_neg_integer()}],
  CheckedMessages :: [{erl_parse:abstract_expr(), non_neg_integer()}],
  Environment :: erl_eval:binding_struct(),
  NewEnvironment :: erl_eval:binding_struct(),
  MatchedBody :: [erl_parse:abstract_expr()],
  MatchedMessage :: {erl_parse:abstract_expr(), non_neg_integer()},
  RestMessages :: [{erl_parse:abstract_expr(), non_neg_integer()}],
  Match :: {NewEnvironment, MatchedBody, MatchedMessage, RestMessages}.

matchrec(_Clauses, [], _CheckedMsgs, _Env) ->
  nomatch;
matchrec(Clauses, [CurMsg | RestMsgs], CheckedMsgs, Env) ->
  {MsgValue, _MsgTime} = CurMsg,
  case match_clause(Env, Clauses, MsgValue) of
    {NewEnv, Body} ->
      {NewEnv, Body, CurMsg, lists:reverse(CheckedMsgs, RestMsgs)};
    nomatch ->
      matchrec(Clauses, RestMsgs, [CurMsg | CheckedMsgs])
  end.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(System) -> Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  lists:append(SchedOpts, ProcsOpts).


-spec eval_sched_opts(System) -> Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_sched_opts(#sys{msgs = []}) ->
  [];
eval_sched_opts(System = #sys{msgs = [CurMsg | RestMsgs], procs = Procs}) ->
  #msg{dest = DestPid, time = Time} = CurMsg,
  case lists:any(fun(P) -> P#proc.pid == DestPid end, Procs) of
    false ->
      eval_sched_opts(System#sys{msgs = RestMsgs});
    true ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_MSG,
        id   = Time,
        rule = ?RULE_SCHED
      },
      [Option | eval_sched_opts(System#sys{msgs = RestMsgs})]
  end.


-spec eval_procs_opts(System) -> Options when
  System :: #sys{},
  Options :: [#opt{}].

eval_procs_opts(#sys{procs = []}) ->
  [];
eval_procs_opts(System = #sys{procs = [CurProc | RestProcs]}) ->
  #proc{pid = Pid, env = Env, exp = Exprs, mail = Mail} = CurProc,
  case eval_expr_opt(Exprs, Env, Mail) of
    ?NOT_EXP ->
      eval_procs_opts(System#sys{procs = RestProcs});
    Rule ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_PROC,
        id   = erl_syntax:integer_value(Pid),
        rule = Rule
      },
      [Option | eval_procs_opts(System#sys{procs = RestProcs})]
  end.


-spec eval_expr_opt(Expressions, Environment, Mail) -> Options when
  Expressions :: erl_parse:abstract_expr() | [erl_parse:abstract_expr()],
  Environment :: erl_eval:binding_struct(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Mail) when is_tuple(Expr) ->
  eval_expr_opt([Expr], Env, Mail);
eval_expr_opt([Expr | Exprs], Env, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case is_expr(Env, Expr) of
    false ->
      case Exprs of
        [] ->
          ?NOT_EXP;
        _ ->
          % If `Expr` is not an expression but there are still other expressions
          % to evaluate then it means we just found a literal in the middle of
          % the program, so we allow to continue.
          ?RULE_SEQ
      end;
    true ->
      case erl_syntax:type(Expr) of
        variable ->
          ?RULE_SEQ;
        match_expr ->
          Pattern = erl_syntax:match_expr_pattern(Expr),
          case is_expr(Env, Pattern) of
            true ->
              eval_expr_opt(Pattern, Env, Mail);
            false ->
              Body = erl_syntax:match_expr_body(Expr),
              case is_expr(Env, Body) of
                true ->
                  eval_expr_opt(Body, Env, Mail);
                false ->
                  ?RULE_SEQ
              end
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case is_expr(Env, Left) of
            true ->
              eval_expr_opt(Left, Env, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case is_expr(Env, Right) of
                true ->
                  eval_expr_opt(Right, Env, Mail);
                false ->
                  Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(Expr)),
                  case Op of
                    '!' ->
                      ?RULE_SEND;
                    _ ->
                      ?RULE_SEQ
                  end
              end
          end;
        prefix_expr ->
          Arg = erl_syntax:prefix_expr_argument(Expr),
          case is_expr(Env, Arg) of
            true ->
              eval_expr_opt(Arg, Env, Mail);
            false ->
              ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun(Arg) -> is_expr(Env, Arg) end, Args) of
            true ->
              eval_expr_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case is_expr(Env, Module) of
                    true ->
                      eval_expr_opt(Module, Env, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case is_expr(Env, Name) of
                            true ->
                              eval_expr_opt(Name, Env, Mail);
                            false ->
                              case erl_syntax:atom_value(Name) of
                                'spawn' ->
                                  ?RULE_SPAWN;
                                'self' ->
                                  ?RULE_SELF;
                                _ ->
                                  ?RULE_SEQ
                              end
                          end;
                        _ ->
                          ?RULE_SEQ
                      end
                  end;
                _ ->
                  case is_expr(Env, Op) of
                    true ->
                      eval_expr_opt(Op, Env, Mail);
                    false ->
                      % TODO Check for clashes with functions in the same file and/or directory
                      case erl_syntax:atom_value(Op) of
                        'spawn' ->
                          ?RULE_SPAWN;
                        'self' ->
                          ?RULE_SELF;
                        _ ->
                          ?RULE_SEQ
                      end
                  end
              end
          end;
        list ->
          eval_expr_opt(erl_syntax:list_elements(Expr), Env, Mail);
        tuple ->
          eval_expr_opt(erl_syntax:tuple_elements(Expr), Env, Mail);
        case_expr ->
          Arg = erl_syntax:case_expr_argument(Expr),
          case is_expr(Env, Arg) of
            true ->
              eval_expr_opt(Arg, Env, Mail);
            false ->
              ?RULE_SEQ
          end;
        receive_expr ->
          Clauses = erl_syntax:receive_expr_clauses(Expr),
          case matchrec(Clauses, Mail, Env) of
            nomatch ->
              ?NOT_EXP;
            _Other ->
              ?RULE_RECEIVE
          end
      end
  end.


ref_add(Id, Ref) ->
  ets:insert(?APP_REF, {Id, Ref}).

ref_lookup(Id) ->
  ets:lookup_element(?APP_REF, Id, 2).
