%%%-----------------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_replay).

-export([can_replay_step/2, can_replay_spawn/2, can_replay_send/2, can_replay_receive/2, can_replay_full/1]).
-export([replay_step/2, replay_spawn/2, replay_send/2, replay_receive/2, replay_full/1]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can replay a step in the
%% given system, or not.

-spec can_replay_step(System, Pid) -> CanReplay when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanReplay :: boolean().

can_replay_step(#sys{procs = PMap, logs = LMap}, Pid) when is_map_key(Pid, PMap), is_map_key(Pid, LMap), map_get(Pid, LMap) =/= [] -> true;
can_replay_step(_, _)                                                                                                              -> false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% replayed in the given system, or not.

-spec can_replay_spawn(System, Pid) -> CanReplay when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanReplay :: boolean().

can_replay_spawn(#sys{logs = LMap}, Pid) -> cauder_utils:find_spawn_parent(LMap, Pid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_send(System, Uid) -> CanReplay when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanReplay :: boolean().

can_replay_send(#sys{logs = LMap}, Uid) -> cauder_utils:find_msg_sender(LMap, Uid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_receive(System, Uid) -> CanReplay when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanReplay :: boolean().

can_replay_receive(#sys{logs = LMap}, Uid) -> cauder_utils:find_msg_receiver(LMap, Uid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can replay a step in the
%% given system, or not.

-spec can_replay_full(System) -> CanReplay when
  System :: cauder_types:system(),
  CanReplay :: boolean().

can_replay_full(#sys{logs = LMap})  ->
  lists:any(fun(Log) -> Log =/= [] end, maps:values(LMap)).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Replays a single step in the process with the given pid, in given the
%% system.

-spec replay_step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_step(#sys{logs = LMap} = Sys, Pid) ->
  case cauder_semantics_forwards:can_step(Sys, Pid) of
    false ->
      case maps:get(Pid, LMap) of
        [{spawn, SpawnPid} | _] -> replay_spawn(Sys, SpawnPid);
        [{send, Uid} | _] -> replay_send(Sys, Uid);
        [{'receive', Uid} | _] -> replay_receive(Sys, Uid)
      end;
    true -> cauder_semantics_forwards:step(Sys, Pid)
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid, in the given
%% system.

-spec replay_spawn(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_spawn(#sys{procs = PMap} = Sys, Pid) when is_map_key(Pid, PMap) -> Sys;
replay_spawn(#sys{logs = LMap} = Sys, Pid) ->
  case cauder_utils:find_spawn_parent(LMap, Pid) of
    {value, ParentPid} -> replay_until_spawn(Sys, ParentPid, Pid);
    false -> Sys
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid, in the given
%% system.

-spec replay_send(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_send(#sys{logs = LMap, mail = Mail} = Sys, Uid) ->
  case lists:any(fun(Msg) -> Msg#msg.uid =:= Uid end, Mail) of
    true -> Sys; % The message has already been sent
    false ->
      case cauder_utils:find_msg_sender(LMap, Uid) of
        {value, SenderPid} -> replay_until_send(Sys, SenderPid, Uid);
        false -> Sys
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid, in the given
%% system.

-spec replay_receive(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_receive(#sys{logs = LMap} = Sys, Uid) ->
  case cauder_utils:find_msg_receiver(LMap, Uid) of
    {value, ReceiverPid} -> replay_until_receive(Sys, ReceiverPid, Uid);
    false -> Sys
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the entire log of each process, in the given system.

-spec replay_full(System) -> NewSystem when
  System :: cauder_types:system(),
  NewSystem :: cauder_types:system().

replay_full( #sys{logs = LMap}=Sys0) ->
  lists:foldl(
    fun
      ([], Sys) -> Sys;
      (Log, Sys) ->
        case lists:last(Log) of
          {spawn, Pid} -> cauder_replay:replay_spawn(Sys, Pid);
          {send, Uid} -> cauder_replay:replay_send(Sys, Uid);
          {'receive', Uid} -> cauder_replay:replay_receive(Sys, Uid)
        end
    end,
    Sys0,
    maps:values(LMap)
  ).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec replay_until_spawn(System, ParentPid, Pid) -> NewSystem when
  System :: cauder_types:system(),
  ParentPid :: cauder_types:proc_id(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_until_spawn(Sys0, ParentPid, Pid) ->
  #sys{logs = #{ParentPid := ParentLog}} = Sys1 = replay_spawn(Sys0, ParentPid),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_spawn_1(System, ParentPid, Pid) -> NewSystem when
  System :: cauder_types:system(),
  ParentPid :: cauder_types:proc_id(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_until_spawn_1(Sys0, ParentPid, Pid) ->
  #sys{logs = #{ParentPid := ParentLog}} = Sys1 = replay_step(Sys0, ParentPid),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_send(System, SenderPid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  SenderPid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_until_send(Sys0, SenderPid, Uid) ->
  #sys{logs = #{SenderPid := SenderLog}} = Sys1 = replay_spawn(Sys0, SenderPid),
  case lists:member({send, Uid}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Uid)
  end.


-spec replay_until_send_1(System, SenderPid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  SenderPid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_until_send_1(Sys0, SenderPid, Uid) ->
  #sys{logs = #{SenderPid := SenderLog}} = Sys1 = replay_step(Sys0, SenderPid),
  case lists:member({send, Uid}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Uid)
  end.


-spec replay_until_receive(System, ReceiverPid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  ReceiverPid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_until_receive(Sys0, ReceiverPid, Uid) ->
  Sys1 = replay_spawn(Sys0, ReceiverPid),
  #sys{logs = #{ReceiverPid := ReceiverLog}} = Sys2 = replay_send(Sys1, Uid), % TODO Review Sys1 or Sys2?
  case lists:member({'receive', Uid}, ReceiverLog) of
    false -> Sys0;
    true -> replay_until_receive_1(Sys2, ReceiverPid, Uid)
  end.


-spec replay_until_receive_1(System, ReceiverPid, Uid) -> NewSystem when
  System :: cauder_types:system(),
  ReceiverPid :: cauder_types:proc_id(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_until_receive_1(Sys0, ReceiverPid, Uid) ->
  #sys{logs = #{ReceiverPid := ReceiverLog}} = Sys1 = replay_step(Sys0, ReceiverPid),
  case lists:member({'receive', Uid}, ReceiverLog) of
    false -> Sys1;
    true -> replay_until_receive_1(Sys1, ReceiverPid, Uid)
  end.
