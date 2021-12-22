%%%-----------------------------------------------------------------------------
%%% @doc Replay operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_replay).

-export([
    can_replay_step/2,
    can_replay_start/2,
    can_replay_spawn/2,
    can_replay_send/2,
    can_replay_receive/2
]).
-export([
    replay_step/2,
    replay_start/2,
    replay_spawn/3,
    replay_send/2,
    replay_receive/2
]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_log.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can replay a step in the
%% given system, or not.

-spec can_replay_step(Pid, System) -> boolean() when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

can_replay_step(Pid, #system{pool = Pool, log = Log}) ->
    cauder_pool:is_element(Pid, Pool) andalso
        cauder_log:is_element(Pid, Log) andalso
        cauder_log:get(Pid, Log) =/= [].

%%------------------------------------------------------------------------------
%% @doc Checks whether the starting of the node with the given name can be
%% replayed in the given system, or not.

-spec can_replay_start(Node, System) -> boolean() when
    Node :: node(),
    System :: cauder_system:system().

can_replay_start(Node, #system{log = Log}) ->
    cauder_log:find_start(Node, Log) =/= error.

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% replayed in the given system, or not.

-spec can_replay_spawn(Pid, System) -> boolean() when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

can_replay_spawn(Pid, #system{log = Log}) ->
    cauder_log:find_spawn(Pid, Log) =/= error.

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_send(Uid, System) -> boolean() when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

can_replay_send(Uid, #system{log = Log}) ->
    cauder_log:find_send(Uid, Log) =/= error.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_receive(Uid, System) -> boolean() when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

can_replay_receive(Uid, #system{log = Log}) ->
    cauder_log:find_receive(Uid, Log) =/= error.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Replays a single step in the process with the given pid, in given the
%% system.

-spec replay_step(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_step(Pid, #system{log = Log} = Sys) ->
    Opts = cauder_semantics_forwards:options(Sys, replay),
    case maps:is_key(Pid, Opts) of
        true ->
            cauder_semantics_forwards:step(Pid, Sys, ?SCHEDULER_Random, replay);
        false ->
            case cauder_log:peek(Pid, Log) of
                {value, #log_send{uid = Uid}} ->
                    replay_send(Uid, Sys);
                {value, #log_receive{uid = Uid}} ->
                    replay_receive(Uid, Sys);
                {value, #log_start{node = Node, success = 'true'}} ->
                    replay_start(Node, Sys);
                {value, #log_spawn{node = Node, pid = ChildPid, success = 'true'}} ->
                    replay_spawn('_', Sys, {spawn, {Node, ChildPid}, success})
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the starting of the node with the given name, in the given
%% system.

-spec replay_start(Node, System) -> NewSystem when
    Node :: node(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_start(Node, #system{nodes = Nodes, log = Log} = Sys) ->
    case lists:member(Node, Nodes) of
        true ->
            Sys;
        false ->
            Sys1 =
                case cauder_log:find_nodes(Node, Log) of
                    [Pid | _] ->
                        replay_step(Pid, Sys);
                    [] ->
                        case cauder_log:find_failed_spawns(Node, Log) of
                            [Pid | _] ->
                                replay_step(Pid, Sys);
                            [] ->
                                {ok, Pid} = cauder_log:find_start(Node, Log),
                                replay_until_start(Pid, Sys, Node)
                        end
                end,
            replay_start(Node, Sys1)
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid, in the given
%% system.

-spec replay_spawn(Pid, System, SpawnInfo) -> NewSystem when
    Pid :: cauder_process:id() | '_',
    System :: cauder_system:system(),
    SpawnInfo :: cauder_log:action_spawn() | '_',
    NewSystem :: cauder_system:system().

replay_spawn(Pid, #system{pool = Pool, log = Log} = Sys, _) when Pid =/= '_' ->
    case cauder_pool:is_element(Pid, Pool) of
        true ->
            Sys;
        false ->
            {ok, Action} = cauder_log:find_spawn_action(Pid, Log),
            replay_spawn('_', Sys, Action)
    end;
replay_spawn(_, #system{log = Log} = Sys, {spawn, {_, Pid}, failure}) ->
    case cauder_log:find_spawn(Pid, Log) of
        {ok, ParentPid} ->
            replay_until_spawn(ParentPid, Sys, Pid);
        error ->
            Sys
    end;
replay_spawn(_, Sys0, #log_spawn{node = Node, pid = Pid, success = 'true'}) ->
    #system{log = Log} = Sys = replay_start(Node, Sys0),
    {ok, ProcParent} = cauder_log:find_spawn(Pid, Log),
    replay_until_spawn(ProcParent, Sys, Pid).

%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid, in the given
%% system.

-spec replay_send(Uid, System) -> NewSystem when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_send(Uid, #system{mail = Mail, log = Log} = Sys) ->
    case cauder_mailbox:is_element(Uid, Mail) of
        % The message has already been sent
        true ->
            Sys;
        false ->
            case cauder_log:find_send(Uid, Log) of
                {ok, Pid} -> replay_until_send(Pid, Sys, Uid);
                error -> Sys
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid, in the given
%% system.

-spec replay_receive(Uid, System) -> NewSystem when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_receive(Uid, #system{log = Log} = Sys) ->
    case cauder_log:find_receive(Uid, Log) of
        {ok, Pid} -> replay_until_receive(Pid, Sys, Uid);
        error -> Sys
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec replay_until_start(Pid, System, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start(Pid, Sys0, Node) ->
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0, '_'),
    case cauder_log:find_start(Node, Log) of
        {ok, _} -> replay_until_start1(Pid, Sys1, Node);
        error -> Sys1
    end.

-spec replay_until_start1(Pid, System, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

replay_until_start1(Pid, Sys0, Node) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:find_start(Node, Log) of
        {ok, _} -> replay_until_start1(Pid, Sys1, Node);
        error -> Sys1
    end.

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

-spec replay_until_spawn1(Pid, System, ChildPid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    ChildPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

replay_until_spawn1(Pid, Sys0, ChildPid) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:has_spawn(Pid, ChildPid, Log) of
        true -> replay_until_spawn1(Pid, Sys1, ChildPid);
        false -> Sys1
    end.

-spec replay_until_send(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

replay_until_send(Pid, Sys0, Uid) ->
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0, '_'),
    case cauder_log:has_send(Pid, Uid, Log) of
        true -> replay_until_send1(Pid, Sys1, Uid);
        false -> Sys1
    end.

-spec replay_until_send1(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

replay_until_send1(Pid, Sys0, Uid) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:has_send(Pid, Uid, Log) of
        true -> replay_until_send1(Pid, Sys1, Uid);
        false -> Sys1
    end.

-spec replay_until_receive(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive(Pid, Sys0, Uid) ->
    Sys1 = replay_spawn(Pid, Sys0, '_'),
    #system{log = Log} = Sys2 = replay_send(Uid, Sys1),
    case cauder_log:has_receive(Pid, Uid, Log) of
        true -> replay_until_receive1(Pid, Sys2, Uid);
        false -> Sys0
    end.

-spec replay_until_receive1(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

replay_until_receive1(Pid, Sys0, Uid) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:has_receive(Pid, Uid, Log) of
        true -> replay_until_receive1(Pid, Sys1, Uid);
        false -> Sys1
    end.
