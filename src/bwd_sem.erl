%%%-------------------------------------------------------------------
%%% @doc Some functions that implement the backward (reversible)
%%% semantics for Erlang. These can be divided into functions to get
%%% the evaluation options and functions to perform the evaluation
%%% @end
%%%-------------------------------------------------------------------

-module(bwd_sem).

-export([eval_step/2, eval_sched/2]).
-export([eval_opts/1, eval_procs_opts/1, eval_sched_opts/1]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Performs an evaluation step in process Pid, given System

-spec eval_step(cauder_types:system(), cauder_types:af_integer()) -> cauder_types:system().

eval_step(System, Pid) ->
  #sys{msgs = Msgs, procs = Procs, trace = Trace} = System,
  {Proc, RestProcs} = utils:select_proc(Procs, Pid),
  #proc{pid = Pid, hist = [CurHist | RestHist]} = Proc,
  case CurHist of
    {tau, OldEnv, OldExprs, OldStack} ->
      OldProc = Proc#proc{
        hist  = RestHist,
        env   = OldEnv,
        exprs = OldExprs,
        stack = OldStack
      },
      System#sys{
        msgs  = Msgs,
        procs = [OldProc | RestProcs]
      };
    {self, OldEnv, OldExprs, OldStack} ->
      OldProc = Proc#proc{
        hist  = RestHist,
        env   = OldEnv,
        exprs = OldExprs,
        stack = OldStack
      },
      System#sys{
        msgs  = Msgs,
        procs = [OldProc | RestProcs]
      };
    {send, OldEnv, OldExprs, OldStack, DestPid, {MsgValue, Time}} ->
      {_Msg, RestMsgs} = utils:select_msg(Msgs, Time),
      OldProc = Proc#proc{
        hist  = RestHist,
        env   = OldEnv,
        exprs = OldExprs,
        stack = OldStack
      },
      TraceItem = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = DestPid,
        val  = MsgValue,
        time = Time
      },
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{
        msgs  = RestMsgs,
        procs = [OldProc | RestProcs],
        trace = OldTrace
      };
    {spawn, OldEnv, OldExprs, OldStack, SpawnPid} ->
      {_SpawnProc, OldRestProcs} = utils:select_proc(RestProcs, SpawnPid),
      OldProc = Proc#proc{
        hist  = RestHist,
        env   = OldEnv,
        exprs = OldExprs,
        stack = OldStack
      },
      TraceItem = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{
        msgs  = Msgs,
        procs = [OldProc | OldRestProcs],
        trace = OldTrace
      };
    {rec, OldEnv, OldExprs, OldStack, OldMsg, OldMail} ->
      {MsgValue, Time} = OldMsg,

      OldProc = Proc#proc{
        hist  = RestHist,
        env   = OldEnv,
        exprs = OldExprs,
        stack = OldStack,
        mail  = OldMail
      },
      TraceItem = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = MsgValue,
        time = Time
      },
      OldTrace = lists:delete(TraceItem, Trace),
      System#sys{
        msgs  = Msgs,
        procs = [OldProc | RestProcs],
        trace = OldTrace
      }
  end.


%% =====================================================================
%% @doc Performs an evaluation step in message Id, given System

-spec eval_sched(cauder_types:system(), non_neg_integer()) -> cauder_types:system().

eval_sched(System, Id) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  {value, Proc} = utils:select_proc_with_time(Procs, Id),
  Pid = Proc#proc.pid,
  {_, RestProcs} = utils:select_proc(Procs, Pid),
  Mail = Proc#proc.mail,
  {LastMsg, RestMail} = utils:last_msg_rest(Mail),
  {Value, Id} = LastMsg,
  OldMsg = #msg{dest = Pid, val = Value, time = Id},
  OldProc = Proc#proc{mail = RestMail},
  OldMsgs = [OldMsg | Msgs],
  OldProcs = [OldProc | RestProcs],
  System#sys{msgs = OldMsgs, procs = OldProcs}.


%% =====================================================================
%% @doc Gets the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(System) ->
  SchedOpts = eval_sched_opts(System),
  ProcsOpts = eval_procs_opts(System),
  SchedOpts ++ ProcsOpts.


-spec eval_procs_opts(cauder_types:system()) -> [cauder_types:option()].

eval_procs_opts(System) ->
  Procs = System#sys.procs,
  Msgs = System#sys.msgs,
  ProcPairs = [utils:select_proc(Procs, Proc#proc.pid) || Proc <- Procs],
  Opts = [eval_proc_opt(#sys{msgs = Msgs, procs = RestProcs}, Proc) || {Proc, RestProcs} <- ProcPairs],
  lists:filter(fun(Opt) -> Opt /= ?NULL_OPT end, Opts).

eval_proc_opt(RestSystem, CurProc) ->
  RestProcs = RestSystem#sys.procs,
  Msgs = RestSystem#sys.msgs,
  Hist = CurProc#proc.hist,
  Rule =
  case Hist of
    [] -> ?NULL_RULE;
    [CurHist | _RestHist] ->
      case CurHist of
        {tau, _, _, _} -> ?RULE_SEQ;
        {self, _, _, _} -> ?RULE_SELF;
        {send, _, _, _, DestPid, {MsgValue, Time}} ->
          MsgList = [M || M <- Msgs, M#msg.time == Time,
                     M#msg.dest == DestPid,
                     M#msg.val == MsgValue],
          case MsgList of
            [] -> ?NULL_RULE;
            _ -> ?RULE_SEND
          end;
        {spawn, _, _, _, SpawnPid} ->
          {SpawnProc, _RestProcs} = utils:select_proc(RestProcs, SpawnPid),
          #proc{hist = SpawnHist, mail = SpawnMail} = SpawnProc,
          case {SpawnHist, SpawnMail} of
            {[], []} -> ?RULE_SPAWN;
            _ -> ?NULL_RULE
          end;
        {rec, _, _, _, ConsMsg, OldMail} ->
          Mail = CurProc#proc.mail,
          case utils:is_queue_minus_msg(OldMail, ConsMsg, Mail) of
            true -> ?RULE_RECEIVE;
            false -> ?NULL_RULE
          end
      end
  end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule ->
      Pid = CurProc#proc.pid,
      #opt{sem = ?MODULE, type = ?TYPE_PROC, id = erl_syntax:concrete(Pid), rule = OtherRule}
  end.


-spec eval_sched_opts(cauder_types:system()) -> [cauder_types:option()].

eval_sched_opts(#sys{procs = Procs}) ->
  Opts = [eval_sched_opt(Proc) || Proc <- Procs],
  lists:filter(fun(Opt) -> Opt /= ?NULL_OPT end, Opts).

eval_sched_opt(Proc) ->
  #proc{hist = Hist, mail = Mail} = Proc,
  Rule =
  case Mail of
    [] -> ?NULL_RULE;
    _ ->
      {LastMsg, _} = utils:last_msg_rest(Mail),
      {_, Time} = LastMsg,
      TopRec = utils:topmost_rec(Hist),
      case TopRec of
        no_rec -> {?RULE_SCHED, Time};
        {rec, _, _, _, OldMsg, OldMail} ->
          case utils:is_queue_minus_msg(OldMail, OldMsg, Mail) of
            false -> {?RULE_SCHED, Time};
            true -> ?NULL_RULE
          end
      end
  end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    {OtherRule, Id} -> #opt{sem = ?MODULE, type = ?TYPE_MSG, id = Id, rule = OtherRule}
  end.
