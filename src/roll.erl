%%%-------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang

-module(roll).

-export([can_roll/2, can_roll_spawn/2, can_roll_send/2, can_roll_rec/2, can_roll_var/2]).
-export([roll_step/2, roll_spawn/2, roll_send/2, roll_rec/2, roll_var/2]).

-include("cauder.hrl").


-spec can_roll(cauder_types:system(), pos_integer()) -> boolean().

can_roll(#sys{procs = Procs}, Pid) ->
  case utils:pid_exists(Procs, Pid) of
    false -> false;
    true ->
      {Proc, _} = utils:take_process(Procs, Pid),
      case Proc#proc.hist of
        [] -> false;
        _ -> true
      end
  end.


-spec roll_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

roll_step(Sys0, Pid) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:take_process(Procs, Pid),
  %io:format("eval_roll - hist: ~p\n",[Proc#proc.hist]),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      Log = Sys0#sys.roll ++ utils:gen_log_spawn(SpawnPid),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(SpawnPid)),
      roll_spawn(Sys, Pid, SpawnPid);
    {send, _Bs, _E, _Stk, #msg{dest = Dest, val = Val, uid = UID}} ->
      Log = Sys0#sys.roll ++ utils:gen_log_send(Pid, Dest, Val, UID),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(Pid) ++ " to " ++ ?TO_STRING(Dest)),
      roll_send(Sys, Pid, Dest, UID);
    _ ->
      [#opt{pid = Pid, sem = Sem} | _] = roll_opts(Sys0, Pid),
      cauder:eval_reduce(Sem, Sys0, Pid)
  end.


-spec roll_spawn(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_spawn(Sys0, Pid, SpawnPid) ->
  Opts = roll_opts(Sys0, Pid),
  case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SPAWN end, Opts) of
    false ->
      Sys1 = roll_step(Sys0, SpawnPid),
      roll_spawn(Sys1, Pid, SpawnPid);
    {value, #opt{pid = Pid, sem = Sem}} ->
      cauder:eval_reduce(Sem, Sys0, Pid)
  end.


-spec roll_send(cauder_types:system(), pos_integer(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_send(Sys0, Pid, DestPid, UID) ->
  Opts1 = roll_opts(Sys0, Pid),
  case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts1) of
    false ->
      Sys1 = roll_step(Sys0, DestPid),
      roll_send(Sys1, Pid, DestPid, UID);
    {value, #opt{pid = Pid, sem = Sem}} ->
      cauder:eval_reduce(Sem, Sys0, Pid)
  end.


%% =====================================================================


-spec can_roll_spawn(cauder_types:system(), cauder_types:proc_id()) -> boolean().

can_roll_spawn(#sys{procs = ProcDict}, Pid) -> utils:find_proc_with_spawn(ProcDict, Pid) =/= false.


-spec can_roll_send(cauder_types:system(), cauder_types:msg_id()) -> boolean().

can_roll_send(#sys{procs = ProcDict}, UID) -> utils:find_proc_with_send(ProcDict, UID) =/= false.


-spec can_roll_rec(cauder_types:system(), cauder_types:msg_id()) -> boolean().

can_roll_rec(#sys{procs = ProcDict}, UID) -> utils:find_proc_with_rec(ProcDict, UID) =/= false.


-spec can_roll_var(cauder_types:system(), atom()) -> boolean().

can_roll_var(#sys{procs = ProcDict}, Name) -> utils:find_proc_with_var(ProcDict, Name) =/= false.


%% =====================================================================


-spec roll_spawn(cauder_types:system(), cauder_types:proc_id()) -> cauder_types:system().

roll_spawn(Sys, Pid) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_spawn(Sys#sys.procs, Pid),
  roll_until_spawn(Sys, Pid, Pid).


-spec roll_send(cauder_types:system(), cauder_types:msg_id()) -> cauder_types:system().

roll_send(Sys, UID) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_send(Sys#sys.procs, UID),
  roll_until_send(Sys, Pid, UID).


-spec roll_rec(cauder_types:system(), cauder_types:msg_id()) -> cauder_types:system().

roll_rec(Sys, UID) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_rec(Sys#sys.procs, UID),
  roll_until_rec(Sys, Pid, UID).


-spec roll_var(cauder_types:system(), atom()) -> cauder_types:system().

roll_var(Sys, Name) ->
  {value, #proc{pid = Pid}} = utils:find_proc_with_var(Sys#sys.procs, Name),
  roll_until_var(Sys, Pid, Name).


%% =====================================================================


-spec roll_until_spawn(cauder_types:system(), cauder_types:proc_id(), cauder_types:proc_id()) -> cauder_types:system().

roll_until_spawn(Sys0, Pid, SpawnPid) ->
  {Proc, _} = utils:take_process(Sys0#sys.procs, Pid),
  [Hist | _] = Proc#proc.hist,
  Sys1 = roll_step(Sys0, Pid),
  case Hist of
    {spawn, _Bs, _Es, _Stk, SpawnPid} -> Sys1;
    _ -> roll_until_spawn(Sys1, Pid, SpawnPid)
  end.


-spec roll_until_send(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

roll_until_send(Sys0, Pid, UID) ->
  {Proc, _} = utils:take_process(Sys0#sys.procs, Pid),
  [Hist | _] = Proc#proc.hist,
  Sys1 = roll_step(Sys0, Pid),
  case Hist of
    {send, _Bs, _Es, _Stk, #msg{uid = UID}} -> Sys1;
    _ -> roll_until_send(Sys1, Pid, UID)
  end.


-spec roll_until_rec(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

roll_until_rec(Sys0, Pid, UID) ->
  {Proc, _} = utils:take_process(Sys0#sys.procs, Pid),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {rec, _Bs, _Es, _Stk, #msg{uid = UID}} ->
      roll_after_rec(Sys0, Pid, UID);
    _ ->
      Sys1 = roll_step(Sys0, Pid),
      roll_until_rec(Sys1, Pid, UID)
  end.


-spec roll_after_rec(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

roll_after_rec(Sys0, Pid, UID) ->
  Sys1 = roll_step(Sys0, Pid),
  {Proc, _} = utils:take_process(Sys1#sys.procs, Pid),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {rec, _Bs, _E, _Stk, #msg{uid = UID}} ->
      roll_after_rec(Sys1, Pid, UID);
    _ -> Sys1
  end.


-spec roll_until_var(cauder_types:system(), cauder_types:proc_id(), atom()) -> cauder_types:system().

roll_until_var(Sys0, Pid, Name) ->
  {#proc{env = Bs}, _} = utils:take_process(Sys0#sys.procs, Pid),
  Sys1 = roll_step(Sys0, Pid),
  case utils:has_var(Bs, Name) of
    false -> Sys1;
    true -> roll_until_var(Sys1, Pid, Name)
  end.


%% =====================================================================


-spec roll_opts(cauder_types:system(), cauder_types:proc_id()) -> [cauder_types:option()].

roll_opts(Sys, Pid) ->
  Opts = bwd_sem:eval_opts(Sys),
  utils:filter_options(Opts, Pid).
