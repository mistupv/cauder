%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the backward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(bwd_sem).

-export([eval_step/2, eval_opts/1]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_step(Sys, Pid) ->
  #sys{mail = Ms, logs = Logs, trace = Trace} = Sys,
  {P0, PDict0} = orddict:take(Pid, Sys#sys.procs),
  #proc{pid = Pid, hist = [CurHist | RestHist]} = P0,
  {ok, Log} = orddict:find(Pid, Logs),
  case CurHist of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        mail  = Ms,
        procs = orddict:store(Pid, P, PDict0)
      };
    {spawn, Bs, Es, Stk, Gid} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = Gid
      },
      Sys#sys{
        mail  = Ms,
        procs = orddict:store(Pid, P, orddict:erase(Gid, PDict0)),
        logs  = orddict:store(Pid, [{spawn, Gid} | Log], Logs),
        trace = lists:delete(T, Trace)
      };
    {send, Bs, Es, Stk, #msg{dest = Dest, val = Val, uid = Uid}} ->
      {_Msg, OldMsgs} = utils:select_msg(Ms, Uid),
      P = P0#proc{
        hist  = RestHist,
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
        mail  = OldMsgs,
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, [{send, Uid} | Log], Logs),
        trace = lists:delete(T, Trace)
      };
    {rec, Bs, Es, Stk, M = #msg{dest = Pid, val = Val, uid = Uid}} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = [M | Ms],
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, [{'receive', Uid} | Log], Logs),
        trace = lists:delete(T, Trace)
      }
  end.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(Sys = #sys{procs = ProcDict0}) ->
  lists:filtermap(
    fun({_, #proc{pid = Pid}}) ->
      {Proc, ProcDict1} = orddict:take(Pid, ProcDict0),
      case eval_proc_opt(Sys#sys{procs = ProcDict1}, Proc) of
        ?NULL_OPT -> false;
        Opt -> {true, Opt}
      end
    end, ProcDict0
  ).


-spec eval_proc_opt(cauder_types:system(), cauder_types:process()) -> cauder_types:option() | ?NULL_OPT.

eval_proc_opt(#sys{mail = Mail, procs = Procs}, #proc{pid = Pid, hist = Hist}) ->
  Rule =
    case Hist of
      [] -> ?NULL_RULE;
      [CurHist | _] ->
        case CurHist of
          {tau, _Bs, _Es, _Stk} -> ?RULE_SEQ;
          {self, _Bs, _Es, _Stk} -> ?RULE_SELF;
          {spawn, _Bs, _Es, _Stk, SpawnPid} ->
            {SpawnProc, _} = orddict:take(SpawnPid, Procs),
            case SpawnProc#proc.hist of
              [] -> ?RULE_SPAWN;
              _ -> ?NULL_RULE
            end;
          {send, _Bs, _Es, _Stk, #msg{uid = UID}} ->
            case utils:check_msg(Mail, UID) of
              none -> ?NULL_RULE;
              _ -> ?RULE_SEND
            end;
          {rec, _Bs, _Es, _Stk, _Msg} -> ?RULE_RECEIVE
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule -> #opt{sem = ?MODULE, pid = Pid, rule = OtherRule}
  end.
