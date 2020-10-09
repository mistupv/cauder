%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the forward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(fwd_sem).

-export([eval_step/2, eval_opts/1]).

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_step(Sys, Pid) ->
  #sys{mail = Ms, logs = Logs, trace = Trace} = Sys,
  {P0, PDict0} = orddict:take(Pid, Sys#sys.procs),
  #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
  #result{env = Bs, exprs = Es, stack = Stk, label = Label} = cauder_eval:seq(Bs0, Es0, Stk0),
  case Label of
    tau ->
      P = P0#proc{
        hist  = [{tau, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        procs = orddict:store(Pid, P, PDict0)
      };
    {self, VarPid} ->
      P = P0#proc{
        hist  = [{self, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, Pid)
      },
      Sys#sys{
        procs = orddict:store(Pid, P, PDict0)
      };
    {spawn, VarPid, M, F, As} ->
      case orddict:find(Pid, Logs) of
        {ok, [LogH | LogT]} ->
          {spawn, SpawnPid} = LogH,
          NewLog = LogT;
        _ ->
          SpawnPid = utils:fresh_pid(),
          NewLog = []
      end,

      P1 = P0#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
      },
      P2 = #proc{
        pid   = SpawnPid,
        exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
        spf   = {M, F, length(As)}
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = orddict:store(SpawnPid, P2, orddict:store(Pid, P1, PDict0)),
        logs  = orddict:store(Pid, NewLog, Logs),
        trace = [T | Trace]
      };
    {send, Dest, Val} ->
      case orddict:find(Pid, Logs) of
        {ok, [LogH | LogT]} ->
          {send, Uid} = LogH,
          NewLog = LogT;
        _ ->
          Uid = utils:fresh_uid(),
          NewLog = []
      end,

      M = #msg{
        dest = Dest,
        val  = Val,
        uid  = Uid
      },
      P = P0#proc{
        hist  = [{send, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = Dest,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = [M | Ms],
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, NewLog, Logs),
        trace = [T | Trace]
      };
    {rec, VarBody, Cs} when Es == [VarBody] ->
      case orddict:find(Pid, Logs) of
        {ok, [LogH | LogT]} ->
          {'receive', Uid} = LogH,
          {Bs1, Es1, M = #msg{dest = Pid, val = Val, uid = Uid}, Ms1} = cauder_eval:match_rec_uid(Cs, Bs, Uid, Ms),
          NewLog = LogT;
        _ ->
          {Bs1, Es1, M = #msg{dest = Pid, val = Val, uid = Uid}, Ms1} = cauder_eval:match_rec_pid(Cs, Bs, Pid, Ms),
          NewLog = []
      end,

      P = P0#proc{
        hist  = [{rec, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = utils:merge_bindings(Bs, Bs1),
        exprs = Es1
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = Ms1,
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, NewLog, Logs),
        trace = [T | Trace]
      }
  end.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(#sys{procs = []}) -> [];
eval_opts(#sys{mail = Mail, procs = PDict, logs = Logs}) ->
  lists:filtermap(
    fun
      ({Pid, #proc{pid = Pid, stack = Stk, env = Bs, exprs = Es}}) ->
        Log =
          case orddict:find(Pid, Logs) of
            {ok, L} -> L;
            error -> []
          end,
        case eval_expr_opt(Pid, Es, Bs, Stk, Log, Mail) of
          ?NOT_EXP -> false;
          Rule ->
            {true, #opt{
              sem  = ?MODULE,
              pid  = Pid,
              rule = Rule
            }}
        end
    end,
    orddict:to_list(PDict)
  ).


-spec eval_expr_opt(Pid, Expressions, Environment, Stack, Log, Mail) -> Rule when
  Pid :: cauder_types:proc_id(),
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Stack :: cauder_types:stack(),
  Log :: cauder_types:log(),
  Mail :: list(cauder_types:message()),
  Rule :: cauder_types:rule() | ?NOT_EXP.

eval_expr_opt(Pid, E, Bs, Stk, Log, Mail) when not is_list(E) ->
  eval_expr_opt(Pid, [E], Bs, Stk, Log, Mail);
eval_expr_opt(Pid, [E0 | Es0], Bs, Stk, Log, Mail) ->
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
            true -> eval_expr_opt(Pid, H, Bs, Stk, Log, Mail);
            false -> eval_expr_opt(Pid, T, Bs, Stk, Log, Mail)
          end;
        {tuple, _, Es} -> eval_expr_opt(Pid, Es, Bs, Stk, Log, Mail);
        {'if', _, _} -> ?RULE_SEQ;
        {'case', _, E, _} ->
          case is_reducible(E, Bs) of
            true -> eval_expr_opt(Pid, E, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {'receive', _, Cs} ->
          IsMatch =
            case Log of
              [] -> cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail) =/= nomatch;
              [{'receive', Uid} | _] -> cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
            end,
          case IsMatch of
            true -> ?RULE_RECEIVE;
            false -> ?NOT_EXP
          end;
        {'make_fun', _, _, _} -> ?RULE_SEQ;
        {bif, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {self, _} -> ?RULE_SELF;
        {spawn, _, F} ->
          case is_reducible(F, Bs) of
            true -> eval_expr_opt(Pid, F, Bs, Stk, Log, Mail);
            false -> ?RULE_SPAWN
          end;
        {spawn, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> eval_expr_opt(Pid, M, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> eval_expr_opt(Pid, F, Bs, Stk, Log, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
                    false -> ?RULE_SPAWN
                  end
              end
          end;
        {Send, _, L, R} when Send =:= 'send' orelse Send =:= 'send_op' ->
          case is_reducible(L, Bs) of
            true -> eval_expr_opt(Pid, L, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> eval_expr_opt(Pid, R, Bs, Stk, Log, Mail);
                false -> ?RULE_SEND
              end
          end;
        {local_call, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {remote_call, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {apply, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> eval_expr_opt(Pid, M, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(F, Bs) of
                true -> eval_expr_opt(Pid, F, Bs, Stk, Log, Mail);
                false ->
                  case is_reducible(As, Bs) of
                    true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
                    false -> ?RULE_SEQ
                  end
              end
          end;
        {apply_fun, _, Fun, As} ->
          case is_reducible(Fun, Bs) of
            true -> eval_expr_opt(Pid, Fun, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(As, Bs) of
                true -> eval_expr_opt(Pid, As, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {match, _, P, E} ->
          case is_reducible(E, Bs) of
            true -> eval_expr_opt(Pid, E, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(P, Bs) of
                true -> eval_expr_opt(Pid, P, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end;
        {op, _, _, Es} ->
          case is_reducible(Es, Bs) of
            true -> eval_expr_opt(Pid, Es, Bs, Stk, Log, Mail);
            false -> ?RULE_SEQ
          end;
        {Op, _, L, R} when Op =:= 'andalso'; Op =:= 'orelse' ->
          case is_reducible(L, Bs) of
            true -> eval_expr_opt(Pid, L, Bs, Stk, Log, Mail);
            false ->
              case is_reducible(R, Bs) of
                true -> eval_expr_opt(Pid, R, Bs, Stk, Log, Mail);
                false -> ?RULE_SEQ
              end
          end
      end
  end.
