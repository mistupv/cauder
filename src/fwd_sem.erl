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


-type result() :: {cauder_types:environment(), [cauder_types:abstract_expr()], label()}.

-type label() :: label_tau()
               | label_spawn()
               | label_self()
               | label_send()
               | label_rec().

-type label_tau() :: tau.
-type label_spawn() :: {spawn, {cauder_types:af_variable(), cauder_types:af_atom(), list(cauder_types:abstract_expr())}}.
-type label_self() :: {self, cauder_types:af_variable()}.
-type label_send() :: {send, cauder_types:af_integer(), cauder_types:abstract_expr()}.
-type label_rec() :: {rec, cauder_types:af_variable(), cauder_types:af_clause_seq()}.


%% =====================================================================
%% @doc Evaluates the first non-literal expression for the given `Expressions`
%% list and returns a tuple with an updated environment, the list of expressions
%% that resulted from the evaluation, and a label.

-spec eval_expr_list(cauder_types:environment(), [cauder_types:abstract_expr()]) -> result().

eval_expr_list(Env, [Expr | Exprs]) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    true ->
      {NewEnv, NewExprs, Label} = eval_expr(Env, Expr),
      {NewEnv, NewExprs ++ Exprs, Label};
    false ->
      {NewEnv, NewExprs, Label} = eval_expr_list(Env, Exprs),
      {NewEnv, [Expr | NewExprs], Label}
  end.


%% =====================================================================
%% @doc Evaluates the given `Expression` and returns a tuple with an updated
%% environment, the expression that resulted from the evaluation, and a label.

-spec eval_expr(cauder_types:environment(), cauder_types:abstract_expr()) -> result().

eval_expr(Env, Expr) when is_tuple(Expr) ->
  case is_expr(Expr, Env) of
    false ->
      % If we find a literal we just consume it.
      {Env, [], tau};
    true ->
      case erl_syntax:type(Expr) of
        variable -> eval_variable(Env, Expr);
        match_expr -> eval_match_expr(Env, Expr);
        infix_expr -> eval_infix_expr(Env, Expr);
        prefix_expr -> eval_prefix_expr(Env, Expr);
        application -> eval_application(Env, Expr);
        list -> eval_list(Env, Expr);
        tuple -> eval_tuple(Env, Expr);
        case_expr -> eval_case_expr(Env, Expr);
        if_expr -> eval_if_expr(Env, Expr);
        receive_expr -> eval_receive(Env, Expr)
      end
  end.


-spec eval_variable(cauder_types:environment(), cauder_types:af_variable()) -> result().

eval_variable(Env, {var, _, Name}) when is_atom(Name) ->
  Value = orddict:fetch(Name, Env),
  AbstractValue = abstract(Value),
  {Env, [AbstractValue], tau}.


-spec eval_match_expr(cauder_types:environment(), cauder_types:af_match(cauder_types:abstract_expr())) -> result().

eval_match_expr(Env, MatchExpr = {match, Line, Pattern, Body}) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], Label} = eval_expr(Env, Pattern),
      {NewEnv, [{match, Line, NewPattern, Body}], Label};
    false ->
      case is_expr(Body, Env) of
        true ->
          {NewEnv, [NewBody], Label} = eval_expr(Env, Body),
          {NewEnv, [{match, Line, Pattern, NewBody}], Label};
        false ->
          % There should be no variables to evaluate so we pass no bindings
          {value, Value, Bindings} = erl_eval:expr(MatchExpr, []),
          NewEnv = utils:merge_env(Env, Bindings),
          {NewEnv, [(abstract(Value))], tau}
      end
  end.


-spec eval_infix_expr(cauder_types:environment(), cauder_types:af_binary_op(cauder_types:abstract_expr())) -> result().

eval_infix_expr(Env, {op, Line, Operator, Left, Right}) when is_atom(Operator) ->
  case is_expr(Left, Env) of
    true ->
      {NewEnv, [NewLeft], Label} = eval_expr(Env, Left),
      {NewEnv, [{op, Line, Operator, NewLeft, Right}], Label};
    false ->
      case {erl_parse:normalise(Left), Operator} of
        {'false', 'andalso'} -> {Env, Left, tau};
        {'true', 'orelse'} -> {Env, Left, tau};
        _ ->
          case is_expr(Right, Env) of
            true ->
              {NewEnv, [NewRight], Label} = eval_expr(Env, Right),
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


-spec eval_prefix_expr(cauder_types:environment(), cauder_types:af_unary_op(cauder_types:abstract_expr())) -> result().

eval_prefix_expr(Env, {op, Pos, Operator, Argument}) when is_atom(Operator) ->
  % FIXME The '-' prefix causes two steps the show the same expression,
  % however they have two different internal representations:
  %  - The number with the operator e.g -(42)
  %  - The negated number e.g (-42)
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = eval_expr(Env, Argument),
      {NewEnv, [{op, Pos, Operator, NewArgument}], Label};
    false ->
      % Prefix operators are always built-in, so we just evaluate the expression
      Value = apply(erlang, Operator, [erl_parse:normalise(Argument)]),
      {Env, [(abstract(Value))], tau}
  end.


-spec eval_application(cauder_types:environment(), cauder_types:af_local_call() | cauder_types:af_remote_call()) -> result().

eval_application(Env, RemoteCall = {call, CallPos, RemoteFun = {remote, RemotePos, Module, Name}, Arguments}) ->
  case is_expr(Module, Env) of
    true ->
      {NewEnv, [NewModule], Label} = eval_expr(Env, Module),
      {NewEnv, [{call, CallPos, {remote, RemotePos, NewModule, Name}, Arguments}], Label};
    false ->
      case is_expr(Name, Env) of
        true ->
          {NewEnv, [NewName], Label} = eval_expr(Env, Name),
          {NewEnv, [{call, CallPos, {remote, RemotePos, Module, NewName}, Arguments}], Label};
        false ->
          case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
            true ->
              {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
              {NewEnv, [{call, CallPos, RemoteFun, NewArguments}], Label};
            false ->
              case erl_syntax:atom_value(Module) of
                'erlang' -> eval_bif(Env, RemoteCall);
                _ ->
                  % TODO Check if module matches current one
                  % TODO Handle calls to functions in other modules
                  error(not_implemented)
              end
          end
      end
  end;
eval_application(Env, LocalCall = {call, Pos, LocalFun, Arguments}) ->
  case is_expr(LocalFun, Env) of
    true ->
      {NewEnv, [NewLocalFun], Label} = eval_expr(Env, LocalFun),
      {NewEnv, [{call, Pos, NewLocalFun, Arguments}], Label};
    false ->
      case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Arguments) of
        true ->
          {NewEnv, NewArguments, Label} = eval_expr_list(Env, Arguments),
          {NewEnv, [{call, Pos, LocalFun, NewArguments}], Label};
        false ->
          Name = erl_syntax:atom_value(LocalFun),
          % Check if the function is in this file
          case utils:fundef_lookup({Name, length(Arguments)}, ref_lookup(?FUN_DEFS)) of
            {value, FunDef} ->
              FunClauses = erl_syntax:function_clauses(utils:fundef_rename(FunDef)),
              % There should be no variables to evaluate so we pass no bindings
              {Bindings, Body} = match_clause([], FunClauses, Arguments),
              NewEnv = utils:merge_env(Env, Bindings),
              {NewEnv, Body, tau};
            false ->
              % TODO If the function name was a variable then BIF should not be evaluated
              % TODO Look for function in other files in the same directory
              eval_bif(Env, LocalCall)
          end
      end
  end.


-spec eval_bif(cauder_types:environment(), cauder_types:af_local_call() | cauder_types:af_remote_call()) -> result().

eval_bif(Env, {call, CallPos, {atom, NamePos, Name}, Arguments}) ->
  eval_bif(Env, {call, CallPos, {remote, NamePos, erlang, Name}, Arguments});
eval_bif(Env, {call, _, {remote, _, erlang, 'spawn'}, Arguments}) ->
  TmpVar = utils:temp_variable(),
  case Arguments of
    % TODO erlang:spawn/1,2,4
    % erlang:spawn/3
    [_SpawnModule, SpawnFunction, SpawnArgs] ->
      % TODO Handle calls to functions in other modules
      {Env, [TmpVar], {spawn, {TmpVar, SpawnFunction, erl_syntax:list_elements(SpawnArgs)}}}
  end;
eval_bif(Env, {call, _, {remote, _, erlang, 'self'}, {nil, _}}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {self, TmpVar}};
eval_bif(Env, {call, _, {remote, _, erlang, FunName}, Arguments}) ->
  ConcreteArgs = lists:map(fun erl_syntax:concrete/1, Arguments),
  % BIF so we just evaluate it
  Value = apply(erlang, FunName, ConcreteArgs),
  {Env, [(abstract(Value))], tau}.


-spec eval_list(cauder_types:environment(), cauder_types:af_cons(cauder_types:abstract_expr())) -> result().

eval_list(Env, {cons, Pos, Head, Tail}) ->
  case is_expr(Head, Env) of
    true ->
      {NewEnv, [NewHead], Label} = eval_expr(Env, Head),
      {NewEnv, [{cons, Pos, NewHead, Tail}], Label};
    false ->
      {NewEnv, [NewTail], Label} = eval_expr(Env, Tail),
      {NewEnv, [{cons, Pos, Head, NewTail}], Label}
  end.


-spec eval_tuple(cauder_types:environment(), cauder_types:af_tuple(cauder_types:abstract_expr())) -> result().

eval_tuple(Env, {tuple, Pos, Elements}) when is_list(Elements) ->
  {NewEnv, NewElements, Label} = eval_expr_list(Env, Elements),
  {NewEnv, [{tuple, Pos, NewElements}], Label}.


-spec eval_case_expr(cauder_types:environment(), cauder_types:af_case()) -> result().

eval_case_expr(Env, {'case', Pos, Argument, Clauses}) ->
  case is_expr(Argument, Env) of
    true ->
      {NewEnv, [NewArgument], Label} = eval_expr(Env, Argument),
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
  case catch erl_eval:expr({match, erl_anno:new(0), Ps1, Vs1}, []) of
    {value, _Val, Bs} ->
      Env1 = utils:merge_env(Env, Bs),
      {match, Env1};
    {badmatch, _Rhs} -> nomatch
  end;
match(_Env, _Ps, _Vs) -> nomatch.


-spec eval_pattern(cauder_types:environment(), cauder_types:af_pattern()) -> cauder_types:af_pattern().

eval_pattern(Env, Pattern) ->
  case is_expr(Pattern, Env) of
    true ->
      {NewEnv, [NewPattern], tau} = eval_expr(Env, Pattern),
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
          {Env, [NewGuardTest], tau} = eval_expr(Env, GuardTest),
          eval_guard_test(Env, NewGuardTest);
        false -> GuardTest
      end;
    false -> erlang:error(guard_expr) % TODO How to handle error in the interpreted code?
  end.


  {NewEnv, Body} = match_clause(Env, Clauses, []),
  {NewEnv, Body, tau}.


-spec eval_receive(cauder_types:environment(), cauder_types:af_receive()) -> result().

%% TODO Support receive with timeout
eval_receive(Env, {'receive', _, Clauses}) ->
  TmpVar = utils:temp_variable(),
  {Env, [TmpVar], {rec, TmpVar, Clauses}}.


-spec abstract(term()) -> cauder_types:abstract_expr().

abstract(Value) -> erl_syntax:revert(erl_syntax:abstract(Value)).


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), cauder_types:af_integer()) -> cauder_types:system().

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exprs = Exprs, mail = Mail} = Proc,
  [CurExpr | RestExpr] = Exprs,
  {NewEnv, NewExprs, Label} = eval_expr(Env, CurExpr),
  case Label of
    tau ->
      NewProc = Proc#proc{
        hist  = [{tau, Env, Exprs} | Hist],
        env   = NewEnv,
        exprs = NewExprs ++ RestExpr
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {self, TmpVar} ->
      RepExpr = utils:replace_variable(TmpVar, Pid, NewExprs),

      NewProc = Proc#proc{
        hist = [{self, Env, Exprs} | Hist],
        env  = NewEnv,
        exprs = RepExpr ++ RestExpr
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
        exprs = NewExprs ++ RestExpr
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
      SpawnPid = abstract(utils:fresh_pid()),
      SpawnProc = #proc{
        pid   = SpawnPid,
        exprs = [{call, erl_anno:new(0), FunName, FunArgs}],
        spf   = {erl_syntax:atom_value(FunName), length(FunArgs)}
      },

      RepExpr = utils:replace_variable(TmpVar, SpawnPid, NewExprs),

      NewProc = Proc#proc{
        hist = [{spawn, Env, Exprs, SpawnPid} | Hist],
        env  = NewEnv,
        exprs = RepExpr ++ RestExpr
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

      RepExpr = utils:replace_variable(TmpVar, RecExp, NewExprs),

      NewProc = Proc#proc{
        hist  = [{rec, Env, Exprs, ConsMsg, Mail} | Hist],
        env   = utils:merge_env(NewEnv, Bindings),
        exprs = RepExpr ++ RestExpr,
        mail  = NewMail
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

-spec eval_sched(cauder_types:system(), non_neg_integer()) -> cauder_types:system().

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
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  lists:append(SchedOpts, ProcsOpts).


-spec eval_sched_opts(cauder_types:system()) -> [cauder_types:option()].

eval_sched_opts(#sys{msgs = []}) -> [];
eval_sched_opts(System = #sys{msgs = [CurMsg | RestMsgs], procs = Procs}) ->
  #msg{dest = DestPid, time = Time} = CurMsg,
  case lists:any(fun(P) -> P#proc.pid == DestPid end, Procs) of
    false -> eval_sched_opts(System#sys{msgs = RestMsgs});
    true ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_MSG,
        id   = Time,
        rule = ?RULE_SCHED
      },
      [Option | eval_sched_opts(System#sys{msgs = RestMsgs})]
  end.


-spec eval_procs_opts(cauder_types:system()) -> [cauder_types:option()].

eval_procs_opts(#sys{procs = []}) -> [];
eval_procs_opts(System = #sys{procs = [CurProc | RestProcs]}) ->
  #proc{pid = Pid, env = Env, exprs = Exprs, mail = Mail} = CurProc,
  case eval_expr_opt(Exprs, Env, Mail) of
    ?NOT_EXP -> eval_procs_opts(System#sys{procs = RestProcs});
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
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Mail) when is_tuple(Expr) -> eval_expr_opt([Expr], Env, Mail);
eval_expr_opt([Expr | Exprs], Env, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case is_expr(Expr, Env) of
    false ->
      case Exprs of
        [] -> ?NOT_EXP;
        _ ->
          % If `Expr` is not an expression but there are still other expressions
          % to evaluate then it means we just found a literal in the middle of
          % the program, so we allow to continue.
          ?RULE_SEQ
      end;
    true ->
      case erl_syntax:type(Expr) of
        variable -> ?RULE_SEQ;
        match_expr ->
          Pattern = erl_syntax:match_expr_pattern(Expr),
          case is_expr(Pattern, Env) of
            true -> eval_expr_opt(Pattern, Env, Mail);
            false ->
              Body = erl_syntax:match_expr_body(Expr),
              case is_expr(Body, Env) of
                true -> eval_expr_opt(Body, Env, Mail);
                false -> ?RULE_SEQ
              end
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case is_expr(Left, Env) of
            true -> eval_expr_opt(Left, Env, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case is_expr(Right, Env) of
                true -> eval_expr_opt(Right, Env, Mail);
                false ->
                  Op = erl_syntax:atom_value(erl_syntax:infix_expr_operator(Expr)),
                  case Op of
                    '!' -> ?RULE_SEND;
                    _ -> ?RULE_SEQ
                  end
              end
          end;
        prefix_expr ->
          Arg = erl_syntax:prefix_expr_argument(Expr),
          case is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun(Arg) -> is_expr(Arg, Env) end, Args) of
            true -> eval_expr_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case is_expr(Module, Env) of
                    true -> eval_expr_opt(Module, Env, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case is_expr(Name, Env) of
                            true -> eval_expr_opt(Name, Env, Mail);
                            false ->
                              case erl_syntax:atom_value(Name) of
                                'spawn' -> ?RULE_SPAWN;
                                'self' -> ?RULE_SELF;
                                _ -> ?RULE_SEQ
                              end
                          end;
                        _ -> ?RULE_SEQ
                      end
                  end;
                _ ->
                  case is_expr(Op, Env) of
                    true -> eval_expr_opt(Op, Env, Mail);
                    false ->
                      % TODO Check for clashes with functions in the same file and/or directory
                      case erl_syntax:atom_value(Op) of
                        'spawn' -> ?RULE_SPAWN;
                        'self' -> ?RULE_SELF;
                        _ -> ?RULE_SEQ
                      end
                  end
              end
          end;
        list -> eval_expr_opt(erl_syntax:list_elements(Expr), Env, Mail);
        tuple -> eval_expr_opt(erl_syntax:tuple_elements(Expr), Env, Mail);
        case_expr ->
          Arg = erl_syntax:case_expr_argument(Expr),
          case is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        if_expr -> ?RULE_SEQ;
        receive_expr ->
          Clauses = erl_syntax:receive_expr_clauses(Expr),
          case matchrec(Clauses, Mail, Env) of
            nomatch -> ?NOT_EXP;
            _Other -> ?RULE_RECEIVE
          end
      end
  end.


ref_lookup(Id) -> ets:lookup_element(?APP_REF, Id, 2).
