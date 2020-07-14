%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).

-export([eval_step/2, eval_sched/2]).
-export([eval_opts/1, eval_procs_opts/1, eval_sched_opts/1]).

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

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
        exprs = cauder_syntax:replace_variable(Es, VarPid, cauder_eval:abstract(Pid)),
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
        exprs = [{remote_call, 0, M, F, lists:map(fun cauder_eval:abstract/1, As)}],
        spf   = {M, F, length(As)}
      },

      P1 = P#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | H],
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, cauder_eval:abstract(SpawnPid)),
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
  case eval_expr_opt(Exprs, Env, Stack, Mail) of
    ?NOT_EXP -> eval_procs_opts(System#sys{procs = RestProcs});
    Rule ->
      Option = #opt{
        sem  = ?MODULE,
        type = ?TYPE_PROC,
        id   = Pid,
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

eval_expr_opt(E, Bs, Stk, Mail) when not is_list(E) -> eval_expr_opt([E], Bs, Stk, Mail);
eval_expr_opt([E0 | Es0], Bs, Stk, Mail) ->
  case is_reducible(E0, Bs) of
    false ->
      case {Es0, Stk} of
        {[], []} -> ?NOT_EXP;
        _ -> ?RULE_SEQ
      end;
    true ->
      case E0 of
        {var, _, _} -> ?RULE_SEQ;
        {cons, _, H, T} ->
          case is_reducible(H, Bs) of
            true -> eval_expr_opt(H, Bs, Stk, Mail);
            false -> eval_expr_opt(T, Bs, Stk, Mail)
          end;
        {tuple, _, Es} -> eval_expr_opt(Es, Bs, Stk, Mail);
        {'if', _, _} -> ?RULE_SEQ;
        {'case', _, E, _} ->
          case is_reducible(E, Bs) of
            true -> eval_expr_opt(E, Bs, Stk, Mail);
            false -> ?RULE_SEQ
          end;
        {'receive', _, Cs} ->
          case cauder_eval:match_rec(Cs, Mail, Bs) of
            nomatch -> ?NOT_EXP;
            _ -> ?RULE_RECEIVE
          end;
        {'make_fun', _, _, _} -> ?RULE_SEQ;
        {bif, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(As, Bs, Stk, Mail);
            false -> ?RULE_SEQ
          end;
        {self, _} -> ?RULE_SELF;
        {spawn, _, F} ->
          case is_reducible(F, Bs) of
            true -> eval_expr_opt(F, Bs, Stk, Mail);
            false -> ?RULE_SPAWN
          end;
        {spawn, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> eval_expr_opt(M, Bs, Stk, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> eval_expr_opt(F, Bs, Stk, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> eval_expr_opt(As, Bs, Stk, Mail);
                    false -> ?RULE_SPAWN
                  end
              end
          end;
        {send, _, L, R} ->
          case is_reducible(L, Bs) of
            true -> eval_expr_opt(L, Bs, Stk, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> eval_expr_opt(R, Bs, Stk, Mail);
                false -> ?RULE_SEND
              end
          end;
        {local_call, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(As, Bs, Stk, Mail);
            false -> ?RULE_SEQ
          end;
        {remote_call, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(As, Bs, Stk, Mail);
            false -> ?RULE_SEQ
          end;
        {apply, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> eval_expr_opt(M, Bs, Stk, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> eval_expr_opt(F, Bs, Stk, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> eval_expr_opt(As, Bs, Stk, Mail);
                    false -> ?RULE_SEQ
                  end
              end
          end;
        {apply_fun, _, Fun, As} ->
          case is_reducible(Fun, Bs) of
            true -> eval_expr_opt(Fun, Bs, Stk, Mail);
            false ->
              case is_reducible(As, Bs) of
                true -> eval_expr_opt(As, Bs, Stk, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {match, _, P, E} ->
          case is_reducible(E, Bs) of
            true -> eval_expr_opt(E, Bs, Stk, Mail);
            false ->
              case is_reducible(P, Bs) of
                true -> eval_expr_opt(P, Bs, Stk, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {op, _, _, Es} ->
          case is_reducible(Es, Bs) of
            true -> eval_expr_opt(Es, Bs, Stk, Mail);
            false -> ?RULE_SEQ
          end;
        {Op, _, L, R} when Op =:= 'andalso'; Op =:= 'orelse' ->
          case is_reducible(L, Bs) of
            true -> eval_expr_opt(L, Bs, Stk, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> eval_expr_opt(R, Bs, Stk, Mail);
                false -> ?RULE_SEQ
              end
          end
      end
  end.
