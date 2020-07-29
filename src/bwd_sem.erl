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

eval_step(System, Pid) ->
  #sys{mail = Mail, procs = Procs, ghosts = Ghosts, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, log = Log, hist = [CurHist | RestHist]} = Proc,
  case CurHist of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      OldProc = Proc#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      System#sys{
        mail  = Mail,
        procs = [OldProc | RestProcs]
      };
    {spawn, Bs, Es, Stk, SpawnPid} ->
      {SpawnProc, RestOldProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldProc = Proc#proc{
        log   = [{spawn, SpawnPid} | Log],
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      TraceEntry = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      OldTrace = lists:delete(TraceEntry, Trace),
      System#sys{
        mail   = Mail,
        procs  = [OldProc | RestOldProcs],
        ghosts = [SpawnProc | Ghosts],
        trace  = OldTrace
      };
    {send, Bs, Es, Stk, #msg{dest = Dest, val = Val, time = Time}} ->
      {_Msg, OldMsgs} = utils:select_msg(Mail, Time),
      OldProc = Proc#proc{
        log   = [{send, Time} | Log],
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      TraceEntry = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = Dest,
        val  = Val,
        time = Time
      },
      OldTrace = lists:delete(TraceEntry, Trace),
      System#sys{
        mail  = OldMsgs,
        procs = [OldProc | RestProcs],
        trace = OldTrace
      };
    {rec, Bs, Es, Stk, OldMsg = #msg{dest = Pid, val = Val, time = Time}} ->
      OldProc = Proc#proc{
        log   = [{'receive', Time} | Log],
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      TraceItem = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Time
      },
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{
        mail  = [OldMsg | Mail],
        procs = [OldProc | RestProcs],
        trace = OldTrace
      }
  end.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(Sys = #sys{procs = Procs}) ->
  ProcPairs = [utils:select_proc(Procs, Proc#proc.pid) || Proc <- Procs],
  Opts = [eval_proc_opt(Sys#sys{procs = RestProcs}, Proc) || {Proc, RestProcs} <- ProcPairs],
  lists:filter(fun(Opt) -> Opt /= ?NULL_OPT end, Opts).


-spec eval_proc_opt(cauder_types:system(), cauder_types:process()) -> cauder_types:option().

eval_proc_opt(#sys{mail = Mail, procs = Procs}, #proc{pid = Pid, hist = Hist}) ->
  Rule =
    case Hist of
      [] -> ?NULL_RULE;
      [CurHist | _] ->
        case CurHist of
          {tau, _Bs, _Es, _Stk} -> ?RULE_SEQ;
          {self, _Bs, _Es, _Stk} -> ?RULE_SELF;
          {spawn, _Bs, _Es, _Stk, SpawnPid} ->
            {SpawnProc, _} = utils:select_proc(Procs, SpawnPid),
            case SpawnProc#proc.hist of
              [] -> ?RULE_SPAWN;
              _ -> ?NULL_RULE
            end;
          {send, _Bs, _Es, _Stk, #msg{time = Time}} ->
            case utils:check_msg(Mail, Time) of
              none -> ?NULL_RULE;
              _ -> ?RULE_SEND
            end;
          {rec, _Bs, _Es, _Stk, _Msg} -> ?RULE_RECEIVE
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule -> #opt{sem = ?MODULE, id = Pid, rule = OtherRule}
  end.
