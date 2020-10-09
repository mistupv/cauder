%%%-------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang

-module(replay).

-export([can_replay/2, can_replay_spawn/2, can_replay_send/2, can_replay_rec/2]).
-export([replay_step/2, replay_spawn/2, replay_send/2, replay_rec/2]).

-include("cauder.hrl").


-spec can_replay(cauder_types:system(), cauder_types:proc_id()) -> boolean().

can_replay(#sys{procs = Procs, logs = Logs}, Pid) ->
  case orddict:is_key(Pid, Procs) of
    false -> false;
    true ->
      {ok, Log} = orddict:find(Pid, Logs),
      case utils:check_log(Log) of
        none -> false;
        _ -> true
      end
  end.


-spec replay_step(cauder_types:system(), cauder_types:proc_id()) -> cauder_types:system().

replay_step(System, Pid) ->
  Opts = fwd_sem:eval_opts(System),
  FiltOpts = [Opt || Opt <- Opts, Opt#opt.pid == Pid],
  case FiltOpts of
    [] ->
      {ok, Log} = orddict:find(Pid, System#sys.logs),
      case utils:check_log(Log) of
        {spawn, SpawnPid} -> replay_spawn(System, SpawnPid);
        {send, UID} -> replay_send(System, UID);
        {'receive', UID} -> replay_rec(System, UID)
      end;
    _ -> fwd_sem:eval_step(System, Pid)
  end.


%% =====================================================================


-spec can_replay_spawn(cauder_types:system(), cauder_types:proc_id()) -> boolean().

can_replay_spawn(#sys{logs = Logs}, Pid) -> utils:find_spawn_parent(Logs, Pid) =/= false.


-spec can_replay_send(cauder_types:system(), cauder_types:msg_id()) -> boolean().

can_replay_send(#sys{logs = Logs}, Uid) -> utils:find_msg_sender(Logs, Uid) =/= false.


-spec can_replay_rec(cauder_types:system(), cauder_types:msg_id()) -> boolean().

can_replay_rec(#sys{logs = Logs}, Uid) -> utils:find_msg_receiver(Logs, Uid) =/= false.


%% =====================================================================


-spec replay_spawn(cauder_types:system(), cauder_types:proc_id()) -> cauder_types:system().

replay_spawn(#sys{procs = PDict, logs = Logs} = Sys, Pid) ->
  case orddict:is_key(Pid, PDict) of
    true -> Sys; % The process has already been spawned
    false ->
      case utils:find_spawn_parent(Logs, Pid) of
        {value, ParentPid} -> replay_until_spawn(Sys, ParentPid, Pid);
        false -> Sys
      end
  end.


-spec replay_send(cauder_types:system(), cauder_types:msg_id()) -> cauder_types:system().

replay_send(#sys{logs = Logs, mail = Mail} = Sys, Uid) ->
  case lists:any(fun(Msg) -> Msg#msg.uid =:= Uid end, Mail) of
    true -> Sys; % The message has already been sent
    false ->
      case utils:find_msg_sender(Logs, Uid) of
        {value, SenderPid} -> replay_until_send(Sys, SenderPid, Uid);
        false -> Sys
      end
  end.


-spec replay_rec(cauder_types:system(), cauder_types:msg_id()) -> cauder_types:system().

replay_rec(#sys{logs = Logs} = Sys, Uid) ->
  case utils:find_msg_receiver(Logs, Uid) of
    {value, ReceiverPid} -> replay_until_rec(Sys, ReceiverPid, Uid);
    false -> Sys
  end.


%% =====================================================================


-spec replay_until_spawn(cauder_types:system(), cauder_types:proc_id(), cauder_types:proc_id()) -> cauder_types:system().

replay_until_spawn(Sys0, ParentPid, Pid) ->
  Sys1 = replay_spawn(Sys0, ParentPid),
  {ok, ParentLog} = orddict:find(ParentPid, Sys1#sys.logs),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_spawn_1(cauder_types:system(), cauder_types:proc_id(), cauder_types:proc_id()) -> cauder_types:system().

replay_until_spawn_1(Sys0, ParentPid, Pid) ->
  Sys1 = replay_step(Sys0, ParentPid),
  {ok, ParentLog} = orddict:find(ParentPid, Sys1#sys.logs),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_send(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

replay_until_send(Sys0, SenderPid, Uid) ->
  Sys1 = replay_spawn(Sys0, SenderPid),
  {ok, SenderLog} = orddict:find(SenderPid, Sys1#sys.logs),
  case lists:member({send, Uid}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Uid)
  end.


-spec replay_until_send_1(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

replay_until_send_1(Sys0, SenderPid, Uid) ->
  Sys1 = replay_step(Sys0, SenderPid),
  {ok, SenderLog} = orddict:find(SenderPid, Sys1#sys.logs),
  case lists:member({send, Uid}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Uid)
  end.


-spec replay_until_rec(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

replay_until_rec(Sys0, ReceiverPid, Uid) ->
  Sys1 = replay_spawn(Sys0, ReceiverPid),
  Sys2 = replay_send(Sys1, Uid),
  {ok, ReceiverLog} = orddict:find(ReceiverPid, Sys2#sys.logs), % TODO Review Sys1 or Sys2?
  case lists:member({'receive', Uid}, ReceiverLog) of
    false -> Sys0;
    true -> replay_until_rec_1(Sys2, ReceiverPid, Uid)
  end.


-spec replay_until_rec_1(cauder_types:system(), cauder_types:proc_id(), cauder_types:msg_id()) -> cauder_types:system().

replay_until_rec_1(Sys0, ReceiverPid, Uid) ->
  Sys1 = replay_step(Sys0, ReceiverPid),
  {ok, ReceiverLog} = orddict:find(ReceiverPid, Sys1#sys.logs),
  case lists:member({'receive', Uid}, ReceiverLog) of
    false -> Sys1;
    true -> replay_until_rec_1(Sys1, ReceiverPid, Uid)
  end.
