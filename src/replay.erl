%%%-------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang

-module(replay).

-export([can_replay/2, can_replay_spawn/2, can_replay_send/2, can_replay_rec/2]).
-export([replay_step/2, replay_spawn/2, replay_send/2, replay_rec/2]).

-include("cauder.hrl").


-spec can_replay(cauder_types:system(), pos_integer()) -> boolean().

can_replay(#sys{procs = Procs}, Pid) ->
  case utils:pid_exists(Procs, Pid) of
    false -> false;
    true ->
      {#proc{log = Log}, _} = utils:select_proc(Procs, Pid),
      case utils:check_log(Log) of
        none -> false;
        _ -> true
      end
  end.


-spec replay_step(cauder_types:system(), pos_integer()) -> cauder_types:system().

replay_step(System, Pid) ->
  Opts = fwd_sem:eval_opts(System),
  FiltOpts = [Opt || Opt <- Opts, Opt#opt.id == Pid],
  case FiltOpts of
    [] ->
      {#proc{log = Log}, _} = utils:select_proc(System#sys.procs, Pid),
      case utils:check_log(Log) of
        {spawn, SpawnPid} -> replay_spawn(System, SpawnPid);
        {send, Stamp} -> replay_send(System, Stamp);
        {'receive', Stamp} -> replay_rec(System, Stamp)
      end;
    _ -> fwd_sem:eval_step(System, Pid)
  end.


%% =====================================================================


-spec can_replay_spawn(cauder_types:system(), pos_integer()) -> boolean().

can_replay_spawn(#sys{procs = Procs, ghosts = Ghosts}, Pid) ->
  length(utils:find_spawn_parent(Procs, Pid)) > 0 orelse
  length(utils:find_spawn_parent(Ghosts, Pid)) > 0.


-spec can_replay_send(cauder_types:system(), pos_integer()) -> boolean().

can_replay_send(#sys{procs = Procs, ghosts = Ghosts}, Stamp) ->
  length(utils:find_msg_sender(Procs, Stamp)) > 0 orelse
  length(utils:find_msg_sender(Ghosts, Stamp)) > 0.


-spec can_replay_rec(cauder_types:system(), pos_integer()) -> boolean().

can_replay_rec(#sys{procs = Procs, ghosts = Ghosts}, Stamp) ->
  length(utils:find_msg_receiver(Procs, Stamp)) > 0 orelse
  length(utils:find_msg_receiver(Ghosts, Stamp)) > 0.


%% =====================================================================


-spec replay_spawn(cauder_types:system(), pos_integer()) -> cauder_types:system().

replay_spawn(Sys, Pid) ->
  #sys{procs = Procs, ghosts = Ghosts} = Sys,
  case utils:pid_exists(Procs, Pid) of
    true -> Sys;
    false ->
      [ParentPid] = utils:find_spawn_parent(Procs, Pid) ++
                    utils:find_spawn_parent(Ghosts, Pid),
      replay_until_spawn(Sys, ParentPid, Pid)
  end.


-spec replay_send(cauder_types:system(), pos_integer()) -> cauder_types:system().

replay_send(Sys, Stamp) ->
  #sys{procs = Procs, ghosts = Ghosts} = Sys,
  case can_replay_send(Sys, Stamp) of
    false -> Sys;
    true ->
      [SenderPid] = utils:find_msg_sender(Procs, Stamp) ++
                    utils:find_msg_sender(Ghosts, Stamp),
      replay_until_send(Sys, SenderPid, Stamp)
  end.


-spec replay_rec(cauder_types:system(), pos_integer()) -> cauder_types:system().

replay_rec(Sys, Stamp) ->
  #sys{procs = Procs, ghosts = Ghosts} = Sys,
  case can_replay_rec(Sys, Stamp) of
    false -> Sys;
    true ->
      [RecPid] = utils:find_msg_receiver(Procs, Stamp) ++
                 utils:find_msg_receiver(Ghosts, Stamp),
      replay_until_rec(Sys, RecPid, Stamp)
  end.


%% =====================================================================


-spec replay_until_spawn(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_spawn(Sys0, ParentPid, Pid) ->
  Sys1 = replay_spawn(Sys0, ParentPid),
  {#proc{log = ParentLog}, _} = utils:select_proc(Sys1#sys.procs, ParentPid),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_spawn_1(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_spawn_1(Sys0, ParentPid, Pid) ->
  Sys1 = replay_step(Sys0, ParentPid),
  {#proc{log = ParentLog}, _} = utils:select_proc(Sys1#sys.procs, ParentPid),
  case lists:member({spawn, Pid}, ParentLog) of
    false -> Sys1;
    true -> replay_until_spawn_1(Sys1, ParentPid, Pid)
  end.


-spec replay_until_send(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_send(Sys0, SenderPid, Stamp) ->
  Sys1 = replay_spawn(Sys0, SenderPid),
  {#proc{log = SenderLog}, _} = utils:select_proc(Sys1#sys.procs, SenderPid),
  case lists:member({send, Stamp}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Stamp)
  end.


-spec replay_until_send_1(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_send_1(Sys0, SenderPid, Stamp) ->
  Sys1 = replay_step(Sys0, SenderPid),
  {#proc{log = SenderLog}, _} = utils:select_proc(Sys1#sys.procs, SenderPid),
  case lists:member({send, Stamp}, SenderLog) of
    false -> Sys1;
    true -> replay_until_send_1(Sys1, SenderPid, Stamp)
  end.


-spec replay_until_rec(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_rec(Sys0, ReceiverPid, Stamp) ->
  Sys1 = replay_spawn(Sys0, ReceiverPid),
  Sys2 = replay_send(Sys1, Stamp),
  {#proc{log = ReceiverLog}, _} = utils:select_proc(Sys2#sys.procs, ReceiverPid),
  case lists:member({'receive', Stamp}, ReceiverLog) of
    false -> Sys0;
    true -> replay_until_rec_1(Sys2, ReceiverPid, Stamp)
  end.


-spec replay_until_rec_1(cauder_types:system(), pos_integer(), pos_integer()) -> cauder_types:system().

replay_until_rec_1(Sys0, ReceiverPid, Stamp) ->
  Sys1 = replay_step(Sys0, ReceiverPid),
  {#proc{log = ReceiverLog}, _} = utils:select_proc(Sys1#sys.procs, ReceiverPid),
  case lists:member({'receive', Stamp}, ReceiverLog) of
    false -> Sys1;
    true -> replay_until_rec_1(Sys1, ReceiverPid, Stamp)
  end.
