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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can replay a step in the
%% given system, or not.

-spec can_replay_step(System, Pid) -> CanReplay when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    CanReplay :: boolean().

can_replay_step(#sys{pool = Pool, traces = Traces}, Pid) ->
    cauder_pool:is_element(Pid, Pool) andalso maps:get(Pid, Traces, []) =/= [].

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% replayed in the given system, or not.

-spec can_replay_spawn(System, Pid) -> CanReplay when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    CanReplay :: boolean().

can_replay_spawn(#sys{traces = LMap}, Pid) -> cauder_utils:find_spawn_parent(LMap, Pid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the starting of the node with the given name can be
%% replayed in the given system, or not.

-spec can_replay_start(System, Node) -> CanReplay when
    System :: cauder_system:system(),
    Node :: node(),
    CanReplay :: boolean().

can_replay_start(#sys{traces = LMap}, Node) -> cauder_utils:find_node_parent(LMap, Node) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_send(System, Uid) -> CanReplay when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    CanReplay :: boolean().

can_replay_send(#sys{traces = LMap}, Uid) -> cauder_utils:find_msg_sender(LMap, Uid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_receive(System, Uid) -> CanReplay when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    CanReplay :: boolean().

can_replay_receive(#sys{traces = LMap}, Uid) -> cauder_utils:find_msg_receiver(LMap, Uid) =/= false.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Replays a single step in the process with the given pid, in given the
%% system.

-spec replay_step(System, Pid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_step(#sys{traces = LMap} = Sys, Pid) ->
    case options(Sys, Pid) of
        [] ->
            case maps:get(Pid, LMap) of
                [{send, Uid} | _] -> replay_send(Sys, Uid);
                [{'receive', Uid} | _] -> replay_receive(Sys, Uid);
                [{start, Node, success} | _] -> replay_start(Sys, Node);
                [{spawn, {Node, ChildPid}, success} | _] -> replay_spawn(Sys, '_', {spawn, {Node, ChildPid}, success})
            end;
        _ ->
            cauder_semantics_forwards:step(Sys, Pid, ?SCHEDULER_Random, replay)
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid, in the given
%% system.

-spec replay_spawn(System, Pid, SpawnInfo) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id() | '_',
    SpawnInfo :: cauder_trace:trace_action() | '_',
    NewSystem :: cauder_system:system().

replay_spawn(#sys{pool = Pool, traces = LMap} = Sys, Pid, _) when Pid =/= '_' ->
    case cauder_pool:is_element(Pid, Pool) of
        true ->
            Sys;
        false ->
            LogItem = cauder_utils:find_spawn_log(LMap, Pid),
            replay_spawn(Sys, '_', LogItem)
    end;
replay_spawn(#sys{traces = LMap} = Sys, _, {spawn, {_, Pid}, failure}) ->
    case cauder_utils:find_spawn_parent(LMap, Pid) of
        {value, ParentPid} -> replay_until_spawn(Sys, ParentPid, Pid);
        false -> Sys
    end;
replay_spawn(Sys0, _, {spawn, {Node, Pid}, success}) ->
    #sys{traces = LMap} = Sys = replay_start(Sys0, Node),
    {value, ProcParent} = cauder_utils:find_spawn_parent(LMap, Pid),
    replay_until_spawn(Sys, ProcParent, Pid).

%%------------------------------------------------------------------------------
%% @doc Replays the starting of the node with the given name, in the given
%% system.

-spec replay_start(System, Node) -> NewSystem when
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_start(#sys{nodes = Nodes, traces = LMap} = Sys, Node) ->
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
                        replay_step(Sys, ProcWithRead);
                    {_, {value, ProcWithFailedSpawn}} ->
                        replay_step(Sys, ProcWithFailedSpawn);
                    {_, _} ->
                        {value, ProcParent} = cauder_utils:find_node_parent(LMap, Node),
                        replay_until_start(Sys, ProcParent, Node)
                end,
            replay_start(NewSys, Node)
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid, in the given
%% system.

-spec replay_send(System, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_send(#sys{traces = LMap, mail = Mail} = Sys, Uid) ->
    case cauder_mailbox:uid_member(Uid, Mail) of
        % The message has already been sent
        true ->
            Sys;
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
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_receive(#sys{traces = LMap} = Sys, Uid) ->
    case cauder_utils:find_msg_receiver(LMap, Uid) of
        {value, ReceiverPid} -> replay_until_receive(Sys, ReceiverPid, Uid);
        false -> Sys
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec replay_until_spawn(System, ParentPid, Pid) -> NewSystem when
    System :: cauder_system:system(),
    ParentPid :: cauder_process:id(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_until_spawn(Sys0, ParentPid, Pid) ->
    #sys{pool = Pool} = Sys1 = replay_spawn(Sys0, ParentPid, '_'),
    case cauder_pool:is_element(Pid, Pool) of
        true -> Sys1;
        false -> replay_until_spawn1(Sys1, ParentPid, Pid)
    end.

-spec replay_until_spawn1(System, ParentPid, Pid) -> NewSystem when
    System :: cauder_system:system(),
    ParentPid :: cauder_process:id(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_until_spawn1(Sys0, ParentPid, Pid) ->
    #sys{traces = #{ParentPid := ParentLog}} = Sys1 = replay_step(Sys0, ParentPid),
    case cauder_utils:find_spawn_parent(#{ParentPid => ParentLog}, Pid) of
        false -> Sys1;
        _ -> replay_until_spawn1(Sys1, ParentPid, Pid)
    end.

-spec replay_until_start(System, ParentPid, Node) -> NewSystem when
    System :: cauder_system:system(),
    ParentPid :: cauder_process:id(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start(Sys0, ParentPid, Node) ->
    #sys{traces = LMap} = Sys1 = replay_spawn(Sys0, ParentPid, '_'),
    case cauder_utils:find_node_parent(LMap, Node) of
        false -> Sys1;
        _ -> replay_until_start1(Sys1, ParentPid, Node)
    end.

-spec replay_until_start1(System, ParentPid, Node) -> NewSystem when
    System :: cauder_system:system(),
    ParentPid :: cauder_process:id(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start1(Sys0, ParentPid, Node) ->
    #sys{traces = LMap} = Sys1 = replay_step(Sys0, ParentPid),
    case cauder_utils:find_node_parent(LMap, Node) of
        false -> Sys1;
        _ -> replay_until_start1(Sys1, ParentPid, Node)
    end.

-spec replay_until_send(System, SenderPid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    SenderPid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_send(Sys0, SenderPid, Uid) ->
    #sys{traces = #{SenderPid := SenderLog}} = Sys1 = replay_spawn(Sys0, SenderPid, '_'),
    case lists:member({send, Uid}, SenderLog) of
        false -> Sys1;
        true -> replay_until_send1(Sys1, SenderPid, Uid)
    end.

-spec replay_until_send1(System, SenderPid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    SenderPid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_send1(Sys0, SenderPid, Uid) ->
    #sys{traces = #{SenderPid := SenderLog}} = Sys1 = replay_step(Sys0, SenderPid),
    case lists:member({send, Uid}, SenderLog) of
        false -> Sys1;
        true -> replay_until_send1(Sys1, SenderPid, Uid)
    end.

-spec replay_until_receive(System, ReceiverPid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    ReceiverPid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive(Sys0, ReceiverPid, Uid) ->
    Sys1 = replay_spawn(Sys0, ReceiverPid, '_'),
    % TODO Review Sys1 or Sys2?
    #sys{traces = #{ReceiverPid := ReceiverLog}} = Sys2 = replay_send(Sys1, Uid),
    case lists:member({'receive', Uid}, ReceiverLog) of
        false -> Sys0;
        true -> replay_until_receive1(Sys2, ReceiverPid, Uid)
    end.

-spec replay_until_receive1(System, ReceiverPid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    ReceiverPid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive1(Sys0, ReceiverPid, Uid) ->
    #sys{traces = #{ReceiverPid := ReceiverLog}} = Sys1 = replay_step(Sys0, ReceiverPid),
    case lists:member({'receive', Uid}, ReceiverLog) of
        false -> Sys1;
        true -> replay_until_receive1(Sys1, ReceiverPid, Uid)
    end.

%%%=============================================================================

-spec options(System, Pid) -> Options when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Options :: [cauder_types:option()].

options(Sys, Pid) ->
    Opts = cauder_semantics_forwards:options(Sys, replay),
    cauder_utils:filter_options(Opts, Pid).
