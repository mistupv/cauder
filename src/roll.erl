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
      {Proc, _} = utils:select_proc(Procs, Pid),
      case Proc#proc.hist of
        [] -> false;
        _ -> true
      end
  end.


-spec roll_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

roll_step(Sys0, Pid) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  %io:format("eval_roll - hist: ~p\n",[Proc#proc.hist]),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      Log = Sys0#sys.roll ++ utils:gen_log_spawn(SpawnPid),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(SpawnPid)),
      roll_spawn(Sys, Pid, SpawnPid);
    {send, _Bs, _E, _Stk, #msg{dest = Dest, val = Val, time = Time}} ->
      Log = Sys0#sys.roll ++ utils:gen_log_send(Pid, Dest, Val, Time),
      Sys = Sys0#sys{roll = Log},
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(Pid) ++ " to " ++ ?TO_STRING(Dest)),
      roll_send(Sys, Pid, Dest, Time);
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
      Sys1 = roll_step(Sys0, SpawnPid),
      roll_spawn(Sys1, Pid, SpawnPid)
  end.


-spec roll_send(cauder_types:system(), pos_integer(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_send(Sys0, Pid, DestPid, Time) ->
  Opts1 = roll_opts(Sys0, Pid),
  case lists:dropwhile(fun(#opt{rule = Rule}) -> Rule =/= ?RULE_SEND end, Opts1) of
    [Opt | _] ->
      cauder:eval_step(Sys0, Opt);
    [] ->
      Sys1 = roll_step(Sys0, DestPid),
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


-spec roll_spawn(cauder_types:system(), pos_integer()) -> cauder_types:system().

roll_spawn(Sys, SpawnPid) ->
  #proc{pid = Pid} = utils:find_proc_with_spawn(Sys#sys.procs, SpawnPid),
  roll_until_spawn(Sys, Pid, SpawnPid).


-spec roll_send(cauder_types:system(), pos_integer()) -> cauder_types:system().

roll_send(Sys, Id) ->
  #proc{pid = Pid} = utils:find_proc_with_send(Sys#sys.procs, Id),
  roll_until_send(Sys, Pid, Id).


-spec roll_rec(cauder_types:system(), pos_integer()) -> cauder_types:system().

roll_rec(Sys, Id) ->
  #proc{pid = Pid} = utils:find_proc_with_rec(Sys#sys.procs, Id),
  roll_until_rec(Sys, Pid, Id).


-spec roll_var(cauder_types:system(), atom()) -> cauder_types:system().

roll_var(Sys, Name) ->
  #proc{pid = Pid} = utils:find_proc_with_var(Sys#sys.procs, Name),
  roll_until_var(Sys, Pid, Name).


%% =====================================================================


-spec roll_until_spawn(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_until_spawn(Sys0, Pid, SpawnPid) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {spawn, _Bs, _Es, _Stk, SpawnPid} ->
      roll_step(Sys0, Pid);
    _ ->
      Sys1 = roll_step(Sys0, Pid),
      roll_until_spawn(Sys1, Pid, SpawnPid)
  end.


-spec roll_until_send(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

roll_until_send(Sys0, Pid, Time) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {send, _Bs, _Es, _Stk, #msg{time = Time}} ->
      roll_step(Sys0, Pid);
    _ ->
      Sys = roll_step(Sys0, Pid),
      roll_until_send(Sys, Pid, Time)
  end.


-spec roll_until_rec(cauder_types:system(), pos_integer(), non_neg_integer()) -> cauder_types:system().

roll_until_rec(Sys0, Pid, Time) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {rec, _Bs, _Es, _Stk, #msg{time = Time}} ->
      roll_after_rec(Sys0, Pid, Time);
    _ ->
      Sys1 = roll_step(Sys0, Pid),
      roll_until_rec(Sys1, Pid, Time)
  end.


-spec roll_after_rec(cauder_types:system(), pos_integer(), non_neg_integer()) -> cauder_types:system().

roll_after_rec(Sys0, Pid, Time) ->
  Sys1 = roll_step(Sys0, Pid),
  Procs = Sys1#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  [CurHist | _] = Proc#proc.hist,
  case CurHist of
    {rec, _Bs, _E, _Stk, #msg{time = Time}} ->
      roll_after_rec(Sys1, Pid, Time);
    _ -> Sys1
  end.


-spec roll_until_var(cauder_types:system(), pos_integer(), atom()) -> cauder_types:system().

roll_until_var(Sys0, Pid, Name) ->
  Procs = Sys0#sys.procs,
  {Proc, _} = utils:select_proc(Procs, Pid),
  Hist = Proc#proc.hist,
  case utils:has_var(Hist, Name) of
    true ->
      Sys = roll_step(Sys0, Pid),
      roll_until_var(Sys, Pid, Name);
    false -> Sys0
  end.


%% =====================================================================


-spec roll_opts(cauder_types:system(), pos_integer()) -> [cauder_types:option()].

roll_opts(Sys, Pid) ->
  Opts = bwd_sem:eval_opts(Sys),
  utils:filter_options(Opts, Pid).
