%%%-----------------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_rollback).

-export([can_rollback_step/2, can_rollback_spawn/2, can_rollback_send/2, can_rollback_receive/2, can_rollback_variable/2]).
-export([rollback_step/2, rollback_spawn/2, rollback_send/2, rollback_receive/2, rollback_variable/2]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can rollback a step in the
%% given system, or not.

-spec can_rollback_step(System, Pid) -> CanRollback when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanRollback :: boolean().

can_rollback_step(#sys{procs = PDict}, Pid) ->
  case orddict:is_key(Pid, PDict) of
    false -> false;
    true ->
      {Proc, _} = orddict:take(Pid, PDict),
      case Proc#proc.hist of
        [] -> false;
        _ -> true
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% rolled back in the given system, or not.

-spec can_rollback_spawn(System, Pid) -> CanRollback when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanRollback :: boolean().

can_rollback_spawn(#sys{procs = PDict}, Pid) -> cauder_utils:find_process_with_spawn(PDict, Pid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_send(System, Uid) -> CanRollback when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanRollback :: boolean().

can_rollback_send(#sys{procs = PDict}, UID) -> cauder_utils:find_process_with_send(PDict, UID) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_receive(System, Uid) -> CanRollback when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanRollback :: boolean().

can_rollback_receive(#sys{procs = PDict}, UID) -> cauder_utils:find_process_with_receive(PDict, UID) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the binding of the variable with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_variable(System, Name) -> CanRollback when
  System :: cauder_types:system(),
  Name :: atom(),
  CanRollback :: boolean().

can_rollback_variable(#sys{procs = PDict}, Name) -> cauder_utils:find_process_with_variable(PDict, Name) =/= false.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Rolls back a single step in the process with the given pid, in given the
%% system.

-spec rollback_step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_step(#sys{procs = PDict, roll = RollLog} = Sys0, Pid) ->
  {Proc, _} = orddict:take(Pid, PDict),
  case hd(Proc#proc.hist) of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(SpawnPid)),
      rollback_spawn(Sys, Pid, SpawnPid);
    {send, _Bs, _E, _Stk, #msg{dest = Dest, val = Val, uid = UID}} ->
      Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Dest, Val, UID)},
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(Pid) ++ " to " ++ ?TO_STRING(Dest)),
      rollback_send(Sys, Pid, Dest, UID);
    _ ->
      [#opt{pid = Pid, sem = Sem} | _] = options(Sys0, Pid),
      Sem:step(Sys0, Pid)
  end.


%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid, in the given
%% system.

-spec rollback_spawn(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_spawn(#sys{procs = PDict} = Sys, Pid) ->
  {value, #proc{pid = Pid}} = cauder_utils:find_process_with_spawn(PDict, Pid),
  rollback_until_spawn(Sys, Pid, Pid).


%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_send(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_send(#sys{procs = PDict} = Sys, UID) ->
  {value, #proc{pid = Pid}} = cauder_utils:find_process_with_send(PDict, UID),
  rollback_until_send(Sys, Pid, UID).


%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid, in the given
%% system.

-spec rollback_receive(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_receive(#sys{procs = PDict} = Sys, UID) ->
  {value, #proc{pid = Pid}} = cauder_utils:find_process_with_receive(PDict, UID),
  rollback_until_receive(Sys, Pid, UID).


%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name, in the given
%% system.

-spec rollback_variable(System, Name) -> NewSystem when
  System :: cauder_types:system(),
  Name :: atom(),
  NewSystem :: cauder_types:system().

rollback_variable(#sys{procs = PDict} = Sys, Name) ->
  {value, #proc{pid = Pid}} = cauder_utils:find_process_with_variable(PDict, Name),
  rollback_until_variable(Sys, Pid, Name).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec rollback_spawn(System, Pid, SpawnPid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  SpawnPid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_spawn(Sys0, Pid, SpawnPid) ->
  Opts = options(Sys0, Pid),
  case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SPAWN end, Opts) of
    false ->
      Sys1 = rollback_step(Sys0, SpawnPid),
      rollback_spawn(Sys1, Pid, SpawnPid);
    {value, #opt{pid = Pid, sem = Sem}} ->
      Sem:step(Sys0, Pid)
  end.


-spec rollback_send(System, Pid, DestPid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  DestPid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_send(Sys0, Pid, DestPid, UID) ->
  Opts = options(Sys0, Pid),
  case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts) of
    false ->
      Sys1 = rollback_step(Sys0, DestPid),
      rollback_send(Sys1, Pid, DestPid, UID);
    {value, #opt{pid = Pid, sem = Sem}} ->
      Sem:step(Sys0, Pid)
  end.


%%%=============================================================================


-spec rollback_until_spawn(System, Pid, SpawnPid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  SpawnPid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_until_spawn(#sys{procs = PDict} = Sys0, Pid, SpawnPid) ->
  {Proc, _} = orddict:take(Pid, PDict),
  [Hist | _] = Proc#proc.hist,
  Sys1 = rollback_step(Sys0, Pid),
  case Hist of
    {spawn, _Bs, _Es, _Stk, SpawnPid} -> Sys1;
    _ -> rollback_until_spawn(Sys1, Pid, SpawnPid)
  end.


-spec rollback_until_send(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_until_send(#sys{procs = PDict} = Sys0, Pid, Uid) ->
  {Proc, _} = orddict:take(Pid, PDict),
  [Hist | _] = Proc#proc.hist,
  Sys1 = rollback_step(Sys0, Pid),
  case Hist of
    {send, _Bs, _Es, _Stk, #msg{uid = Uid}} -> Sys1;
    _ -> rollback_until_send(Sys1, Pid, Uid)
  end.


-spec rollback_until_receive(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_until_receive(#sys{procs = PDict} = Sys0, Pid, UID) ->
  {Proc, _} = orddict:take(Pid, PDict),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {rec, _Bs, _Es, _Stk, #msg{uid = UID}} ->
      rollback_after_receive(Sys0, Pid, UID);
    _ ->
      Sys1 = rollback_step(Sys0, Pid),
      rollback_until_receive(Sys1, Pid, UID)
  end.


-spec rollback_after_receive(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_after_receive(Sys0, Pid, UID) ->
  Sys1 = rollback_step(Sys0, Pid),
  {Proc, _} = orddict:take(Pid, Sys1#sys.procs),
  [Hist | _] = Proc#proc.hist,
  case Hist of
    {rec, _Bs, _E, _Stk, #msg{uid = UID}} ->
      rollback_after_receive(Sys1, Pid, UID);
    _ -> Sys1
  end.


-spec rollback_until_variable(System, Pid, Name) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Name :: atom(),
  NewSystem :: cauder_types:system().

rollback_until_variable(#sys{procs = PDict} = Sys0, Pid, Name) ->
  {#proc{env = Bs}, _} = orddict:take(Pid, PDict),
  Sys1 = rollback_step(Sys0, Pid),
  case cauder_utils:has_var(Bs, Name) of
    false -> Sys1;
    true -> rollback_until_variable(Sys1, Pid, Name)
  end.


%%%=============================================================================


-spec options(System, Pid) -> Options when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Options :: [cauder_types:option()].

options(Sys, Pid) ->
  Opts = cauder_semantics_backwards:options(Sys),
  cauder_utils:filter_options(Opts, Pid).
