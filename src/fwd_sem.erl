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

eval_step(Sys, Pid) ->
  #sys{msgs = Msgs, procs = Ps0, trace = Trace} = Sys,
  {P, Ps} = utils:select_proc(Ps0, Pid),
  #proc{pid = Pid, hist = H, env = Bs0, exprs = Es0, stack = Stk0, mail = Mail0} = P,
%%  io:format("step_before: ~p\n", [Es0]),
  #result{env = Bs, exprs = Es, stack = Stk, label = Label} = cauder_eval:seq(Bs0, Es0, Stk0),
%%  io:format("step_after: ~p\n", [Es]),
  case Label of
    tau ->
      P1 = P#proc{
        hist  = [{tau, Bs0, Es0, Stk0} | H],
        env   = Bs,
        exprs = Es,
        stack = Stk
      },
      Sys#sys{
        procs = [P1 | Ps]
      };
    {self, VarPid} ->
      P1 = P#proc{
        hist  = [{self, Bs0, Es0, Stk0} | H],
        env   = Bs,
        exprs = utils:replace_variable(Es, VarPid, Pid),
        stack = Stk
      },
      Sys#sys{
        procs = [P1 | Ps]
      };
    {send, Dest, Val} ->
      Time = utils:fresh_time(),
      Msg = #msg{dest = Dest, val = Val, time = Time},

      P1 = P#proc{
        hist  = [{send, Bs0, Es0, Stk0, Dest, {Val, Time}} | H],
        env   = Bs,
        exprs = Es,
        stack = Stk
      },
      TraceItem = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = Dest,
        val  = Val,
        time = Time
      },
      Sys#sys{
        msgs  = [Msg | Msgs],
        procs = [P1 | Ps],
        trace = [TraceItem | Trace]
      };
    {spawn, VarPid, M, F, As} ->
      SpawnPid = utils:fresh_pid(),
      SpawnProc = #proc{
        pid   = SpawnPid,
        exprs = [erl_syntax:revert(erl_syntax:application(M, F, As))],
        spf   = {cauder_eval:concrete(M), cauder_eval:concrete(F), length(As)}
      },

      P1 = P#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | H],
        env   = Bs,
        exprs = utils:replace_variable(Es, VarPid, SpawnPid),
        stack = Stk
      },
      TraceItem = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = [P1 | [SpawnProc | Ps]],
        trace = [TraceItem | Trace]
      };
    {rec, VarBody, Cs} when Es == [VarBody] ->
      {Bs1, Body, Msg = {Val, Time}, Mail} = cauder_eval:match_rec(Cs, Mail0, Bs),

      P1 = P#proc{
        hist  = [{rec, Bs0, Es0, Stk0, Msg, Mail0} | H],
        env   = utils:merge_env(Bs, Bs1),
        exprs = Body,
        stack = Stk,
        mail  = Mail
      },
      TraceItem = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Time
      },
      Sys#sys{
        procs = [P1 | Ps],
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
  #proc{pid = Pid, env = Env, exprs = Exprs, stack = Stack, mail = Mail} = CurProc,
%%  io:format("eval_procs_opts: Exprs: ~p\n", [Exprs]),
  case eval_expr_opt(Exprs, Env, Stack, Mail) of
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


-spec eval_expr_opt(Expressions, Environment, Stack, Mail) -> Options when
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Stack :: cauder_types:stack(),
  Mail :: [#msg{}],
  Options :: ?NOT_EXP | ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

eval_expr_opt(Expr, Env, Stack, Mail) when is_tuple(Expr) -> eval_expr_opt([Expr], Env, Stack, Mail);
eval_expr_opt([Expr | Exprs], Env, Stack, Mail) when is_tuple(Expr), is_list(Exprs) ->
  case cauder_eval:is_expr(Expr, Env) of
    false ->
      case Exprs of
        [] when Stack == [] -> ?NOT_EXP;
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
            true -> eval_expr_opt(Pattern, Env, Stack, Mail);
            false ->
              Body = erl_syntax:match_expr_body(Expr),
              case cauder_eval:is_expr(Body, Env) of
                true -> eval_expr_opt(Body, Env, Stack, Mail);
                false -> ?RULE_SEQ
              end
          end;
        infix_expr ->
          Left = erl_syntax:infix_expr_left(Expr),
          case cauder_eval:is_expr(Left, Env) of
            true -> eval_expr_opt(Left, Env, Stack, Mail);
            false ->
              Right = erl_syntax:infix_expr_right(Expr),
              case cauder_eval:is_expr(Right, Env) of
                true -> eval_expr_opt(Right, Env, Stack, Mail);
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
            true -> eval_expr_opt(Arg, Env, Stack, Mail);
            false -> ?RULE_SEQ
          end;
        application ->
          Args = erl_syntax:application_arguments(Expr),
          case lists:any(fun(Arg) -> cauder_eval:is_expr(Arg, Env) end, Args) of
            true -> eval_expr_opt(Args, Env, Stack, Mail);
            false ->
              Op = erl_syntax:application_operator(Expr),
              case erl_syntax:type(Op) of
                module_qualifier ->
                  Module = erl_syntax:module_qualifier_argument(Op),
                  case cauder_eval:is_expr(Module, Env) of
                    true -> eval_expr_opt(Module, Env, Stack, Mail);
                    false ->
                      case erl_syntax:atom_value(Module) of
                        'erlang' ->
                          Name = erl_syntax:module_qualifier_body(Op),
                          case cauder_eval:is_expr(Name, Env) of
                            true -> eval_expr_opt(Name, Env, Stack, Mail);
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
                    true -> eval_expr_opt(Op, Env, Stack, Mail);
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
        list -> eval_expr_opt(erl_syntax:list_elements(Expr), Env, Stack, Mail);
        tuple -> eval_expr_opt(erl_syntax:tuple_elements(Expr), Env, Stack, Mail);
        case_expr ->
          Arg = erl_syntax:case_expr_argument(Expr),
          case cauder_eval:is_expr(Arg, Env) of
            true -> eval_expr_opt(Arg, Env, Stack, Mail);
            false -> ?RULE_SEQ
          end;
        if_expr -> ?RULE_SEQ;
        receive_expr ->
          Clauses = erl_syntax:receive_expr_clauses(Expr),
          case cauder_eval:match_rec(Clauses, Mail, Env) of
            nomatch -> ?NOT_EXP;
            _Other -> ?RULE_RECEIVE
          end
      end
  end.
