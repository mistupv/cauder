%%%-----------------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_replay).

-export([can_replay_step/2, can_replay_spawn/2, can_replay_send/2, can_replay_receive/2]).
-export([replay_step/2, replay_spawn/2, replay_send/2, replay_receive/2]).

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

can_replay_step(#sys{procs = PDict, logs = Logs}, Pid) ->
  case orddict:is_key(Pid, PDict) of
    false -> false;
    true ->
      case orddict:find(Pid, Logs) of
        {ok, []} -> false;
        {ok, _Log} -> true
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% replayed in the given system, or not.

-spec can_replay_spawn(System, Pid) -> CanReplay when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanReplay :: boolean().

can_replay_spawn(#sys{logs = Logs}, Pid) -> cauder_utils:find_spawn_parent(Logs, Pid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_send(System, Uid) -> CanReplay when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanReplay :: boolean().

can_replay_send(#sys{logs = Logs}, Uid) -> cauder_utils:find_msg_sender(Logs, Uid) =/= false.


%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_receive(System, Uid) -> CanReplay when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  CanReplay :: boolean().

can_replay_receive(#sys{logs = Logs}, Uid) -> cauder_utils:find_msg_receiver(Logs, Uid) =/= false.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Replays a single step in the process with the given pid, in given the
%% system.

-spec replay_step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_step(#sys{logs = Logs} = Sys, Pid) ->
  case options(Sys, Pid) of
    [] ->
      {ok, Log} = orddict:find(Pid, Logs),
      case hd(Log) of
        {spawn, SpawnPid} -> replay_spawn(Sys, SpawnPid);
        {send, Uid} -> replay_send(Sys, Uid);
        {'receive', Uid} -> replay_receive(Sys, Uid)
      end;
    _ -> cauder_semantics_forwards:step(Sys, Pid)
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid, in the given
%% system.

-spec replay_spawn(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_spawn(#sys{procs = PDict, logs = Logs} = Sys, Pid) ->
  case orddict:is_key(Pid, PDict) of
    true -> Sys; % The process has already been spawned
    false ->
      case cauder_utils:find_spawn_parent(Logs, Pid) of
        {value, ParentPid} -> replay_until_spawn(Sys, ParentPid, Pid);
        false -> Sys
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid, in the given
%% system.

-spec replay_send(System, Uid) -> NewSystem when
  System :: cauder_types:system(),
  Uid :: cauder_types:msg_id(),
  NewSystem :: cauder_types:system().

replay_send(#sys{logs = Logs, mail = Mail} = Sys, Uid) ->
  case lists:any(fun(Msg) -> Msg#msg.uid =:= Uid end, Mail) of
    true -> Sys; % The message has already been sent
    false ->
      case cauder_utils:find_msg_sender(Logs, Uid) of
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

replay_receive(#sys{logs = Logs} = Sys, Uid) ->
  case cauder_utils:find_msg_receiver(Logs, Uid) of
    {value, ReceiverPid} -> replay_until_receive(Sys, ReceiverPid, Uid);
    false -> Sys
  end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec replay_until_spawn(System, ParentPid, Pid) -> NewSystem when
  System :: cauder_types:system(),
  ParentPid :: cauder_types:proc_id(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

replay_until_spawn(Sys0, ParentPid, Pid) ->
  Sys1 = replay_spawn(Sys0, ParentPid),
  {ok, ParentLog} = orddict:find(ParentPid, Sys1#sys.logs),
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
  Sys1 = replay_step(Sys0, ParentPid),
  {ok, ParentLog} = orddict:find(ParentPid, Sys1#sys.logs),
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
  Sys1 = replay_spawn(Sys0, SenderPid),
  {ok, SenderLog} = orddict:find(SenderPid, Sys1#sys.logs),
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
  Sys1 = replay_step(Sys0, SenderPid),
  {ok, SenderLog} = orddict:find(SenderPid, Sys1#sys.logs),
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
  Sys2 = replay_send(Sys1, Uid),
  {ok, ReceiverLog} = orddict:find(ReceiverPid, Sys2#sys.logs), % TODO Review Sys1 or Sys2?
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
  Sys1 = replay_step(Sys0, ReceiverPid),
  {ok, ReceiverLog} = orddict:find(ReceiverPid, Sys1#sys.logs),
  case lists:member({'receive', Uid}, ReceiverLog) of
    false -> Sys1;
    true -> replay_until_receive_1(Sys1, ReceiverPid, Uid)
  end.


%%%=============================================================================


-spec options(System, Pid) -> Options when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Options :: [cauder_types:option()].

options(Sys, Pid) ->
  Opts = cauder_semantics_forwards:options(Sys),
  cauder_utils:filter_options(Opts, Pid).
