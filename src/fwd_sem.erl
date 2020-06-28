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


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), cauder_types:af_integer()) -> cauder_types:system().

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = Hist, env = Env, exprs = Exprs, mail = Mail} = Proc,
  [CurExpr | RestExpr] = Exprs,
  {NewEnv, NewExprs, Label} = cauder_eval:expr(Env, CurExpr),
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
        hist  = [{self, Env, Exprs} | Hist],
        env   = NewEnv,
        exprs = RepExpr ++ RestExpr
      },
      System#sys{
        procs = [NewProc | RestProcs]
      };
    {send, DestPid, MsgValue} ->
      Time = utils:fresh_time(),
      NewMsg = #msg{dest = DestPid, val = MsgValue, time = Time},

      NewProc = Proc#proc{
        hist  = [{send, Env, Exprs, DestPid, {MsgValue, Time}} | Hist],
        env   = NewEnv,
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
      SpawnPid = utils:fresh_pid(),
      SpawnProc = #proc{
        pid   = SpawnPid,
        exprs = [{call, erl_anno:new(0), FunName, FunArgs}],
        spf   = {erl_syntax:atom_value(FunName), length(FunArgs)}
      },

      RepExpr = utils:replace_variable(TmpVar, SpawnPid, NewExprs),

      NewProc = Proc#proc{
        hist  = [{spawn, Env, Exprs, SpawnPid} | Hist],
        env   = NewEnv,
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
      {Bindings, RecExp, ConsMsg, NewMail} = cauder_eval:matchrec(ReceiveClauses, Mail, NewEnv),
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
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  lists:append(SchedOpts, ProcsOpts).


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


-spec eval_expr_opt(Expressions, Environment, Mail) -> Options when
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Mail) when is_tuple(Expr) -> eval_expr_opt([Expr], Env, Mail);
eval_expr_opt([Expr | Exprs], Env, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case cauder_eval:is_expr(Expr, Env) of
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
          case cauder_eval:is_expr(Pattern, Env) of
            true -> eval_expr_opt(Pattern, Env, Mail);
            false ->
              Body = erl_syntax:match_expr_body(Expr),
              case cauder_eval:is_expr(Body, Env) of
                true -> eval_expr_opt(Body, Env, Mail);
                false -> ?RULE_SEQ
              end
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case cauder_eval:is_expr(Left, Env) of
            true -> eval_expr_opt(Left, Env, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case cauder_eval:is_expr(Right, Env) of
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
          case cauder_eval:is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun(Arg) -> cauder_eval:is_expr(Arg, Env) end, Args) of
            true -> eval_expr_opt(Args, Env, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case cauder_eval:is_expr(Module, Env) of
                    true -> eval_expr_opt(Module, Env, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case cauder_eval:is_expr(Name, Env) of
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
                  case cauder_eval:is_expr(Op, Env) of
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
          case cauder_eval:is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Mail);
            false -> ?RULE_SEQ
          end;
        if_expr -> ?RULE_SEQ;
        receive_expr ->
          Clauses = erl_syntax:receive_expr_clauses(Expr),
          case cauder_eval:matchrec(Clauses, Mail, Env) of
            nomatch -> ?NOT_EXP;
            _Other -> ?RULE_RECEIVE
          end
      end
  end.
