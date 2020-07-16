%%%-------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(roll).
-export([can_roll/2, can_roll_spawn/2, can_roll_send/2, can_roll_rec/2, can_roll_var/2]).
-export([eval_step/2, eval_roll_spawn/2, eval_roll_send/2, eval_roll_rec/2, eval_roll_var/2]).

-include("cauder.hrl").


-spec can_roll(cauder_types:system(), pos_integer()) -> boolean().

can_roll(#sys{procs = Procs}, Pid) ->
  case utils:pid_exists(Procs, Pid) of
    false -> false;
    true ->
      {Proc, _} = utils:select_proc(Procs, Pid),
      Hist = Proc#proc.hist,
      Mail = Proc#proc.mail,
      case {Hist, Mail} of
        {[], []} -> false;
        _ -> true
      end
  end.


-spec eval_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_step(Sys0, Pid) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  %io:format("eval_roll - hist: ~p\n",[Proc#proc.hist]),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      Log = Sys0#sys.roll ++ utils:gen_log_spawn(Pid, SpawnPid),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(SpawnPid)),
      roll_spawn(Sys, Pid, SpawnPid);
    {send, _Bs, _E, _Stk, DestPid, {MsgValue, Time}} ->
      Log = Sys0#sys.roll ++ utils:gen_log_send(Pid, DestPid, MsgValue, Time),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(Pid) ++ " to " ++ ?TO_STRING(DestPid)),
      roll_send(Sys, Pid, DestPid, Time);
    _ ->
      [Opt | _] = roll_opts(Sys0, Pid),
      cauder:eval_step(Sys0, Opt)
  end.


-spec roll_spawn(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_spawn(Sys0, Pid, SpawnPid) ->
  Opts = roll_opts(Sys0, Pid),
  case lists:dropwhile(fun(#opt{rule = Rule}) -> Rule =/= ?RULE_SPAWN end, Opts) of
    [Opt | _] ->
      cauder:eval_step(Sys0, Opt);
    [] ->
      Sys1 = eval_step(Sys0, SpawnPid),
      roll_spawn(Sys1, Pid, SpawnPid)
  end.


-spec roll_send(cauder_types:system(), pos_integer(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_send(Sys0, Pid, DestPid, Time) ->
  Opts1 = roll_opts(Sys0, Pid),
  case lists:dropwhile(fun(#opt{rule = Rule}) -> Rule =/= ?RULE_SPAWN end, Opts1) of
    [Opt | _] ->
      cauder:eval_step(Sys0, Opt);
    [] ->
      Opts2 = roll_sched_opts(Sys0, DestPid),
      Sys1 =
      case lists:dropwhile(fun(#opt{id = Id}) -> Id =/= Time end, Opts2) of
        [Opt | _] -> cauder:eval_step(Sys0, Opt);
        [] -> eval_step(Sys0, DestPid)
      end,
      roll_send(Sys1, Pid, DestPid, Time)
  end.


%% =====================================================================


-spec can_roll_spawn(cauder_types:system(), pos_integer()) -> boolean().

can_roll_spawn(#sys{procs = Procs}, SpawnPid) -> utils:find_proc_with_spawn(Procs, SpawnPid) =/= false.


-spec can_roll_send(cauder_types:system(), pos_integer()) -> boolean().

can_roll_send(#sys{procs = Procs}, Time) -> utils:find_proc_with_send(Procs, Time) =/= false.


-spec can_roll_rec(cauder_types:system(), pos_integer()) -> boolean().

can_roll_rec(#sys{procs = Procs}, Time) -> utils:find_proc_with_rec(Procs, Time) =/= false.


-spec can_roll_var(cauder_types:system(), atom()) -> boolean().

can_roll_var(#sys{procs = Procs}, Name) -> utils:find_proc_with_var(Procs, Name) =/= false.


%% =====================================================================


-spec eval_roll_spawn(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_roll_spawn(Sys, SpawnPid) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_spawn(Sys#sys.procs, SpawnPid),
  eval_roll_until_spawn(Sys, Pid, SpawnPid).


-spec eval_roll_send(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_roll_send(Sys, Id) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_send(Sys#sys.procs, Id),
  eval_roll_until_send(Sys, Pid, Id).


-spec eval_roll_rec(cauder_types:system(), pos_integer()) -> cauder_types:system().

eval_roll_rec(Sys, Id) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_rec(Sys#sys.procs, Id),
  eval_roll_until_rec(Sys, Pid, Id).


-spec eval_roll_var(cauder_types:system(), atom()) -> cauder_types:system().

eval_roll_var(Sys, Name) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_var(Sys#sys.procs, Name),
  eval_roll_until_var(Sys, Pid, Name).


%% =====================================================================


-spec eval_roll_until_spawn(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

eval_roll_until_spawn(Sys0, Pid, SpawnPid) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      eval_step(Sys0, Pid);
    _ ->
      Sys1 = eval_step(Sys0, Pid),
      eval_roll_until_spawn(Sys1, Pid, SpawnPid)
  end.


-spec eval_roll_until_send(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

eval_roll_until_send(Sys0, Pid, Id) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {send, _Bs, _E, _Stk, _Dest, {_Val, Id}} ->
      eval_step(Sys0, Pid);
    _ ->
      Sys = eval_step(Sys0, Pid),
      eval_roll_until_send(Sys, Pid, Id)
  end.


-spec eval_roll_until_rec(cauder_types:system(), pos_integer(), non_neg_integer()) -> cauder_types:system().

eval_roll_until_rec(Sys0, Pid, Id) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {rec, _Bs, _E, _Stk, {_Val, Id}, _Msgs} ->
      eval_roll_after_rec(Sys0, Pid, Id);
    _ ->
      Sys1 = eval_step(Sys0, Pid),
      eval_roll_until_rec(Sys1, Pid, Id)
  end.


-spec eval_roll_after_rec(cauder_types:system(), pos_integer(), non_neg_integer()) -> cauder_types:system().

eval_roll_after_rec(Sys0, Pid, Id) ->
  Sys1 = eval_step(Sys0, Pid),
  Procs = Sys1#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {rec, _Bs, _E, _Stk, {_Val, Id}, _Msgs} ->
      eval_roll_after_rec(Sys1, Pid, Id);
    _ -> Sys1
  end.


-spec eval_roll_until_var(cauder_types:system(), pos_integer(), atom()) -> cauder_types:system().

eval_roll_until_var(Sys0, Pid, Name) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  Hist = Proc#proc.hist,
  case utils:has_var(Hist, Name) of
    true ->
      Sys = eval_step(Sys0, Pid),
      eval_roll_until_var(Sys, Pid, Name);
    false -> Sys0
  end.


%% =====================================================================


-spec roll_opts(cauder_types:system(), pos_integer()) -> [cauder_types:option()].

roll_opts(System, Pid) ->
  ProcOpts = roll_procs_opts(System, Pid),
  SchedOpts = roll_sched_opts(System, Pid),
  SchedOpts ++ ProcOpts.


-spec roll_procs_opts(cauder_types:system(), pos_integer()) -> [cauder_types:option()].

roll_procs_opts(Sys, Pid) ->
  Opts = bwd_sem:eval_procs_opts(Sys),
  utils:filter_options(Opts, Pid).


-spec roll_sched_opts(cauder_types:system(), pos_integer()) -> [cauder_types:option()].

roll_sched_opts(#sys{procs = Procs}, Pid) ->
  {Proc, _} = utils:select_proc(Procs, Pid),
  SingleProcSys = #sys{procs = [Proc]},
  bwd_sem:eval_sched_opts(SingleProcSys).
