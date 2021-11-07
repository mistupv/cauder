%%%-----------------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_replay).

-export([
    can_replay_step/2,
    can_replay_spawn/2,
    can_replay_start/2,
    can_replay_send/2,
    can_replay_receive/2
]).
-export([
    replay_step/2,
    replay_spawn/3,
    replay_start/2,
    replay_send/2,
    replay_receive/2
]).

-include("cauder.hrl").
-include("cauder_system.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can replay a step in the
%% given system, or not.

-spec can_replay_step(Pid, System) -> boolean() when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

can_replay_step(Pid, #system{pool = Pool, traces = Traces}) ->
    cauder_pool:is_element(Pid, Pool) andalso maps:get(Pid, Traces, []) =/= [].

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% replayed in the given system, or not.

-spec can_replay_spawn(Pid, System) -> boolean() when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

can_replay_spawn(Pid, #system{traces = LMap}) ->
    cauder_utils:find_spawn_parent(LMap, Pid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the starting of the node with the given name can be
%% replayed in the given system, or not.

-spec can_replay_start(Node, System) -> boolean() when
    Node :: node(),
    System :: cauder_system:system().

can_replay_start(Node, #system{traces = LMap}) ->
    cauder_utils:find_node_parent(LMap, Node) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_send(Uid, System) -> boolean() when
    Uid :: cauder_mailbox:uid(),
    System :: cauder_system:system().

can_replay_send(Uid, #system{traces = LMap}) ->
    cauder_utils:find_msg_sender(LMap, Uid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_receive(Uid, System) -> boolean() when
    Uid :: cauder_mailbox:uid(),
    System :: cauder_system:system().

can_replay_receive(Uid, #system{traces = LMap}) ->
    cauder_utils:find_msg_receiver(LMap, Uid) =/= false.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Replays a single step in the process with the given pid, in given the
%% system.

-spec replay_step(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_step(Pid, #system{traces = LMap} = Sys) ->
    case options(Pid, Sys) of
        [] ->
            case maps:get(Pid, LMap) of
                [{send, Uid} | _] ->
                    replay_send(Uid, Sys);
                [{'receive', Uid} | _] ->
                    replay_receive(Uid, Sys);
                [{start, Node, success} | _] ->
                    replay_start(Node, Sys);
                [{spawn, {Node, ChildPid}, success} | _] ->
                    replay_spawn('_', Sys, {spawn, {Node, ChildPid}, success})
            end;
        _ ->
            cauder_semantics_forwards:step(Pid, Sys, ?SCHEDULER_Random, replay)
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid, in the given
%% system.

-spec replay_spawn(Pid, System, SpawnInfo) -> NewSystem when
    Pid :: cauder_process:id() | '_',
    System :: cauder_system:system(),
    SpawnInfo :: cauder_trace:trace_action() | '_',
    NewSystem :: cauder_system:system().

replay_spawn(Pid, #system{pool = Pool, traces = LMap} = Sys, _) when Pid =/= '_' ->
    case cauder_pool:is_element(Pid, Pool) of
        true ->
            Sys;
        false ->
            LogItem = cauder_utils:find_spawn_log(LMap, Pid),
            replay_spawn('_', Sys, LogItem)
    end;
replay_spawn(_, #system{traces = LMap} = Sys, {spawn, {_, Pid}, failure}) ->
    case cauder_utils:find_spawn_parent(LMap, Pid) of
        {value, ParentPid} -> replay_until_spawn(ParentPid, Sys, Pid);
        false -> Sys
    end;
replay_spawn(_, Sys0, {spawn, {Node, Pid}, success}) ->
    #system{traces = LMap} = Sys = replay_start(Node, Sys0),
    {value, ProcParent} = cauder_utils:find_spawn_parent(LMap, Pid),
    replay_until_spawn(ProcParent, Sys, Pid).

%%------------------------------------------------------------------------------
%% @doc Replays the starting of the node with the given name, in the given
%% system.

-spec replay_start(Node, System) -> NewSystem when
    Node :: node(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_start(Node, #system{nodes = Nodes, traces = LMap} = Sys) ->
    NodeExists = lists:member(Node, Nodes),
    FutureReads = cauder_utils:find_process_with_future_reads(LMap, Node),
    FailedSpawns = cauder_utils:find_process_with_failed_spawn(LMap, Node),
    case NodeExists of
        true ->
            Sys;
        false ->
            NewSys =
                case {FutureReads, FailedSpawns} of
                    {{value, ProcWithRead}, _} ->
                        replay_step(ProcWithRead, Sys);
                    {_, {value, ProcWithFailedSpawn}} ->
                        replay_step(ProcWithFailedSpawn, Sys);
                    {_, _} ->
                        {value, ProcParent} = cauder_utils:find_node_parent(LMap, Node),
                        replay_until_start(ProcParent, Sys, Node)
                end,
            replay_start(Node, NewSys)
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid, in the given
%% system.

-spec replay_send(Uid, System) -> NewSystem when
    Uid :: cauder_mailbox:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_send(Uid, #system{mail = Mail, traces = LMap} = Sys) ->
    case cauder_mailbox:uid_member(Uid, Mail) of
        % The message has already been sent
        true ->
            Sys;
        false ->
            case cauder_utils:find_msg_sender(LMap, Uid) of
                {value, SenderPid} -> replay_until_send(SenderPid, Sys, Uid);
                false -> Sys
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid, in the given
%% system.

-spec replay_receive(Uid, System) -> NewSystem when
    Uid :: cauder_mailbox:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_receive(Uid, #system{traces = LMap} = Sys) ->
    case cauder_utils:find_msg_receiver(LMap, Uid) of
        {value, ReceiverPid} -> replay_until_receive(ReceiverPid, Sys, Uid);
        false -> Sys
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec replay_until_spawn(ParentPid, System, Pid) -> NewSystem when
    ParentPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_until_spawn(ParentPid, Sys0, Pid) ->
    #system{pool = Pool} = Sys1 = replay_spawn(ParentPid, Sys0, '_'),
    case cauder_pool:is_element(Pid, Pool) of
        true -> Sys1;
        false -> replay_until_spawn1(ParentPid, Sys1, Pid)
    end.

-spec replay_until_spawn1(ParentPid, System, Pid) -> NewSystem when
    ParentPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_until_spawn1(ParentPid, Sys0, Pid) ->
    #system{traces = #{ParentPid := ParentLog}} = Sys1 = replay_step(ParentPid, Sys0),
    case cauder_utils:find_spawn_parent(#{ParentPid => ParentLog}, Pid) of
        false -> Sys1;
        _ -> replay_until_spawn1(ParentPid, Sys1, Pid)
    end.

-spec replay_until_start(ParentPid, System, Node) -> NewSystem when
    ParentPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start(ParentPid, Sys0, Node) ->
    #system{traces = LMap} = Sys1 = replay_spawn(ParentPid, Sys0, '_'),
    case cauder_utils:find_node_parent(LMap, Node) of
        false -> Sys1;
        _ -> replay_until_start1(ParentPid, Sys1, Node)
    end.

-spec replay_until_start1(ParentPid, System, Node) -> NewSystem when
    ParentPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start1(ParentPid, Sys0, Node) ->
    #system{traces = LMap} = Sys1 = replay_step(ParentPid, Sys0),
    case cauder_utils:find_node_parent(LMap, Node) of
        false -> Sys1;
        _ -> replay_until_start1(ParentPid, Sys1, Node)
    end.

-spec replay_until_send(SenderPid, System, Uid) -> NewSystem when
    SenderPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_send(SenderPid, Sys0, Uid) ->
    #system{traces = #{SenderPid := SenderLog}} = Sys1 = replay_spawn(SenderPid, Sys0, '_'),
    case lists:member({send, Uid}, SenderLog) of
        false -> Sys1;
        true -> replay_until_send1(SenderPid, Sys1, Uid)
    end.

-spec replay_until_send1(SenderPid, System, Uid) -> NewSystem when
    SenderPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_send1(SenderPid, Sys0, Uid) ->
    #system{traces = #{SenderPid := SenderLog}} = Sys1 = replay_step(SenderPid, Sys0),
    case lists:member({send, Uid}, SenderLog) of
        false -> Sys1;
        true -> replay_until_send1(SenderPid, Sys1, Uid)
    end.

-spec replay_until_receive(ReceiverPid, System, Uid) -> NewSystem when
    ReceiverPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive(ReceiverPid, Sys0, Uid) ->
    Sys1 = replay_spawn(ReceiverPid, Sys0, '_'),
    % TODO Review Sys1 or Sys2?
    #system{traces = #{ReceiverPid := ReceiverLog}} = Sys2 = replay_send(Uid, Sys1),
    case lists:member({'receive', Uid}, ReceiverLog) of
        false -> Sys0;
        true -> replay_until_receive1(ReceiverPid, Sys2, Uid)
    end.

-spec replay_until_receive1(ReceiverPid, System, Uid) -> NewSystem when
    ReceiverPid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive1(ReceiverPid, Sys0, Uid) ->
    #system{traces = #{ReceiverPid := ReceiverLog}} = Sys1 = replay_step(ReceiverPid, Sys0),
    case lists:member({'receive', Uid}, ReceiverLog) of
        false -> Sys1;
        true -> replay_until_receive1(ReceiverPid, Sys1, Uid)
    end.

%%%=============================================================================

-spec options(Pid, System) -> Options when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Options :: [cauder_types:option()].

options(Pid, Sys) ->
    Opts = cauder_semantics_forwards:options(Sys, replay),
    cauder_utils:filter_options(Pid, Opts).
