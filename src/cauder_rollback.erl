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

%can_rollback_step(#sys{procs = #{Pid := #proc{hist = Hist}}}, Pid) when Hist =/= []                        -> true; % TODO Doesn't compile: variable 'Pid' is unbound
%can_rollback_step(#sys{procs = PMap}, Pid) when is_map_key(Pid, PMap), map_get(Pid, PMap)#proc.hist =/= [] -> true; % TODO Doesn't compile: syntax error before: '#'

can_rollback_step(#sys{procs = PMap}, Pid) when is_map_key(Pid, PMap) ->
  #proc{hist = Hist} = maps:get(Pid, PMap),
  Hist =/= [];
can_rollback_step(_, _) -> false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% rolled back in the given system, or not.

-spec can_rollback_spawn(System, Pid) -> CanRollback when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanRollback :: boolean().

can_rollback_spawn(#sys{procs = PMap}, Pid) -> cauder_utils:find_process_with_spawn(PMap, Pid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_send(System, Uid) -> CanRollback when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanRollback :: boolean().

can_rollback_send(#sys{procs = PMap}, Uid) -> cauder_utils:find_process_with_send(PMap, Uid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_receive(System, Uid) -> CanRollback when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanRollback :: boolean().

can_rollback_receive(#sys{procs = PMap}, Uid) -> cauder_utils:find_process_with_receive(PMap, Uid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the binding of the variable with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_variable(System, Name) -> CanRollback when
  System :: cauder_types:system(),
  Name :: atom(),
  CanRollback :: boolean().

can_rollback_variable(#sys{procs = PMap}, Name) -> cauder_utils:find_process_with_variable(PMap, Name) =/= false.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Rolls back a single step in the process with the given pid, in given the
%% system.

-spec rollback_step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_step(#sys{procs = PMap, roll = RollLog} = Sys0, Pid) ->
  #proc{hist = [Entry | _]} = maps:get(Pid, PMap),
  case Entry of
    {spawn, _Bs, _E, _Stk, SpawnPid} ->
      Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
      ?LOG("ROLLing back SPAWN of " ++ ?TO_STRING(SpawnPid)),
      rollback_spawn(Sys, Pid, SpawnPid);
    {send, _Bs, _E, _Stk, #msg{dest = Dest, val = Val, uid = Uid}} ->
      Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Dest, Val, Uid)},
      ?LOG("ROLLing back SEND from " ++ ?TO_STRING(Pid) ++ " to " ++ ?TO_STRING(Dest)),
      rollback_send(Sys, Pid, Dest, Uid);
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

rollback_spawn(#sys{procs = PMap} = Sys, Pid) ->
  {value, #proc{pid = ParentPid}} = cauder_utils:find_process_with_spawn(PMap, Pid),
  rollback_until_spawn(Sys#sys{roll = []}, ParentPid, Pid).


%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_send(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_send(#sys{procs = PMap} = Sys, Uid) ->
  {value, #proc{pid = SenderPid}} = cauder_utils:find_process_with_send(PMap, Uid),
  rollback_until_send(Sys#sys{roll = []}, SenderPid, Uid).


%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid, in the given
%% system.

-spec rollback_receive(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_receive(#sys{procs = PMap} = Sys, Uid) ->
  {value, #proc{pid = ReceiverPid}} = cauder_utils:find_process_with_receive(PMap, Uid),
  rollback_until_receive(Sys#sys{roll = []}, ReceiverPid, Uid).


%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name, in the given
%% system.

-spec rollback_variable(System, Name) -> NewSystem when
  System :: cauder_types:system(),
  Name :: atom(),
  NewSystem :: cauder_types:system().

rollback_variable(#sys{procs = PMap} = Sys, Name) ->
  {value, #proc{pid = Pid}} = cauder_utils:find_process_with_variable(PMap, Name),
  rollback_until_variable(Sys#sys{roll = []}, Pid, Name).


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

rollback_send(Sys0, Pid, DestPid, Uid) ->
  Opts = options(Sys0, Pid),
  case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts) of
    false ->
      Sys1 = rollback_step(Sys0, DestPid),
      rollback_send(Sys1, Pid, DestPid, Uid);
    {value, #opt{pid = Pid, sem = Sem}} ->
      Sem:step(Sys0, Pid)
  end.


%%%=============================================================================


-spec rollback_until_spawn(System, Pid, SpawnPid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  SpawnPid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

rollback_until_spawn(#sys{procs = PMap} = Sys0, Pid, SpawnPid) ->
  #proc{hist = [Entry | _]} = maps:get(Pid, PMap),
  case Entry of
    {spawn, _Bs, _Es, _Stk, SpawnPid} ->
      rollback_step(Sys0, Pid);
    _ ->
      Sys1 = rollback_step(Sys0, Pid),
      rollback_until_spawn(Sys1, Pid, SpawnPid)
  end.


-spec rollback_until_send(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_until_send(#sys{procs = PMap} = Sys0, Pid, Uid) ->
  #proc{hist = [Entry | _]} = maps:get(Pid, PMap),
  case Entry of
    {send, _Bs, _Es, _Stk, #msg{uid = Uid}} ->
      rollback_step(Sys0, Pid);
    _ ->
      Sys1 = rollback_step(Sys0, Pid),
      rollback_until_send(Sys1, Pid, Uid)
  end.


-spec rollback_until_receive(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_until_receive(#sys{procs = PMap} = Sys0, Pid, Uid) ->
  #proc{hist = [Entry | _]} = maps:get(Pid, PMap),
  case Entry of
    {rec, _Bs, _Es, _Stk, #msg{uid = Uid}} ->
      rollback_after_receive(Sys0, Pid, Uid);
    _ ->
      Sys1 = rollback_step(Sys0, Pid),
      rollback_until_receive(Sys1, Pid, Uid)
  end.


-spec rollback_after_receive(System, Pid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

rollback_after_receive(Sys0, Pid, Uid) ->
  #sys{procs = PMap} = Sys1 = rollback_step(Sys0, Pid),
  #proc{hist = [Entry | _]} = maps:get(Pid, PMap),
  case Entry of
    {rec, _Bs, _E, _Stk, #msg{uid = Uid}} ->
      rollback_after_receive(Sys1, Pid, Uid);
    _ -> Sys1
  end.


-spec rollback_until_variable(System, Pid, Name) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Name :: atom(),
  NewSystem :: cauder_types:system().

rollback_until_variable(#sys{procs = PMap} = Sys0, Pid, Name) ->
  #proc{env = Bs} = maps:get(Pid, PMap),
  case maps:is_key(Name, Bs) of
    true -> Sys0;
    false ->
      Sys1 = rollback_step(Sys0, Pid),
      rollback_until_variable(Sys1, Pid, Name)
  end.


%%%=============================================================================


-spec options(System, Pid) -> Options when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Options :: [cauder_types:option()].

options(Sys, Pid) ->
  lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, cauder_semantics_backwards:options(Sys)).
