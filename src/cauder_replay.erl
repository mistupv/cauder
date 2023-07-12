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
    can_replay_receive/2,
    can_replay_register/2,
    can_replay_delete/2
]).
-export([
    replay_step/2,
    replay_start/2,
    replay_spawn/2,
    replay_send/2,
    replay_receive/2,
    replay_register/2,
    replay_delete/2
]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_log.hrl").
-include("cauder_process.hrl").

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

%%------------------------------------------------------------------------------
%% @doc Checks whether the  of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_delete(El, System) -> boolean() when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

can_replay_delete(El, #system{log = Log}) ->
    cauder_log:find_delete(El, Log) =/= error.

%%------------------------------------------------------------------------------
%% @doc Checks whether the  of the message with the given uid can be
%% replayed in the given system, or not.

-spec can_replay_register(El, System) -> boolean() when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

can_replay_register(El, #system{log = Log}) ->
    cauder_log:find_register(El, Log) =/= error.

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
                {value, #log_sendA{el = El, uid = Uid}} ->
                    replay_reg_del((cauder_pool:get(Pid, Sys#system.pool))#process.node, [El], Sys),
                    replay_send(Uid, Sys);
                {value, #log_receive{uid = Uid}} ->
                    replay_receive(Uid, Sys);
                {value, #log_start{node = Node, success = 'true'}} ->
                    replay_start(Node, Sys);
                {value, #log_spawn{pid = ChildPid, success = 'true'}} ->
                    replay_spawn(ChildPid, Sys);
                {value, #log_reg{atom = A, pid = P, key = Key, map = LogMap}} ->
                    replay_register({A, P, Key, top}, LogMap, Sys);
                {value, #log_del{atom = A, pid = P, key = Key, map = LogMap}} ->
                    replay_delete({A, P, Key, bot}, LogMap, Sys);
                {value, #log_read{map = LogMap}} ->
                    replay_read(Pid, (cauder_pool:get(Pid, Sys#system.pool))#process.node, LogMap, Sys)
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

-spec replay_spawn(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_spawn(ChildPid, #system{pool = Pool, log = Log} = Sys0) ->
    case cauder_pool:is_element(ChildPid, Pool) of
        true ->
            Sys0;
        false ->
            case cauder_log:find_spawn_action(ChildPid, Log) of
                {ok, {Pid, ChildNode}} ->
                    Sys1 = replay_start(ChildNode, Sys0),
                    replay_until_spawn(Pid, Sys1, ChildPid);
                error ->
                    Sys0
            end
    end.

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

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid, in the given
%% system.

-spec replay_register(El, System) -> NewSystem when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_register(El, #system{log = Log} = Sys) ->
    case cauder_log:find_register(El, Log) of
        {ok, Pid} -> replay_until_register(Pid, Sys, El);
        error -> Sys
    end.

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid, in the given
%% system.

-spec replay_delete(El, System) -> NewSystem when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_delete(El, #system{log = Log} = Sys) ->
    case cauder_log:find_delete(El, Log) of
        {ok, Pid} -> replay_until_delete(Pid, Sys, El);
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
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0),
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
    #system{pool = Pool} = Sys1 = replay_spawn(ParentPid, Sys0),
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
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0),
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
    Sys1 = replay_spawn(Pid, Sys0),
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

-spec replay_until_register(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_register(Pid, Sys0, El) ->
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0),
    case cauder_log:has_register(Pid, El, Log) of
        true -> replay_until_register1(Pid, Sys1, El);
        false -> Sys0
    end.

-spec replay_until_register1(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_register1(Pid, Sys0, El) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:has_register(Pid, El, Log) of
        true -> replay_until_register1(Pid, Sys1, El);
        false -> Sys1
    end.

-spec replay_register(El, LogMap, System) -> NewSystem when
    El :: cauder_map:map_element(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_register(El, LogMap, #system{log = Log} = Sys) ->
    case cauder_log:find_register(El, Log) of
        {ok, Pid} -> replay_until_register(Pid, LogMap, Sys, El);
        error -> Sys
    end.

-spec replay_until_register(Pid, LogMap, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_register(Pid, LogMap, Sys0, El) ->
    Proc = cauder_pool:get(Pid, Sys0#system.pool),
    SystemMap = cauder_map:get_map(Sys0#system.maps, Proc#process.node),
    Sys1 = replay_registereds(SystemMap, Sys0),
    Sys2 = replay_fail_reads(El, LogMap, Sys1),
    Sys3 = replay_spawn(Pid, Sys2),
    Proc = cauder_pool:get(Pid, Sys3#system.pool),
    #system{log = Log2} = Sys4 = replay_reg_del(Proc#process.node, LogMap, Sys3),
    case cauder_log:has_register(Pid, El, Log2) of
        true -> replay_until_register1(Pid, Sys4, El);
        false -> Sys4
    end.

-spec replay_fail_reads(El, Map, System) -> NewSystem when
    El :: cauder_map:map_element(),
    Map :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_fail_reads(El, LogMap, #system{log = Log} = Sys0) ->
    %io:format("\nreplay_fail_reads\n", []),
    case cauder_log:find_fail_read(El, LogMap, Log) of
        {ok, Pid} ->
            Sys1 = replay_until_fail_read(Pid, Sys0, LogMap, El),
            replay_fail_reads(El, LogMap, Sys1);
        error ->
            Sys0
    end.

-spec replay_until_fail_read(Pid, System, Map, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Map :: [cauder_map:map_element()],
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_fail_read(Pid, Sys0, Map, El) ->
    %io:format("\nreplay_until_fail_read\n", []),
    case cauder_log:has_fail_read(Pid, Map, El, Sys0#system.log) of
        true ->
            Sys1 = replay_step(Pid, Sys0),
            replay_until_fail_read(Pid, Sys1, Map, El);
        false ->
            Sys0
    end.

-spec replay_registereds(Map, System) -> NewSystem when
    Map :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_registereds(Map, #system{log = Log} = Sys0) ->
    %io:format("\nreplay_registereds\n", []),
    case cauder_log:find_registered(Map, Log) of
        {ok, Pid} ->
            Sys1 = replay_until_registered(Pid, Sys0, Map),
            Proc = cauder_pool:get(Pid, Sys1#system.pool),
            SystemMap = cauder_map:get_map(Sys1#system.maps, Proc#process.node),
            replay_registereds(SystemMap, Sys1);
        error ->
            Sys0
    end.

-spec replay_until_registered(Pid, System, Map) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Map :: [cauder_map:map_element()],
    NewSystem :: cauder_system:system().

replay_until_registered(Pid, Sys0, Map) ->
    %io:format("\nreplay_until_registered~p\n", [Sys0#system.log]),

    case cauder_log:has_registered(Pid, Map, Sys0#system.log) of
        true ->
            Sys1 = replay_step(Pid, Sys0),
            Proc = cauder_pool:get(Pid, Sys1#system.pool),
            SystemMap = cauder_map:get_map(Sys1#system.maps, Proc#process.node),
            replay_until_registered(Pid, Sys1, SystemMap);
        false ->
            Sys0
    end.

-spec replay_until_delete(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_delete(Pid, Sys0, El) ->
    #system{log = Log} = Sys1 = replay_spawn(Pid, Sys0),
    case cauder_log:has_delete(Pid, El, Log) of
        true -> replay_until_delete1(Pid, Sys1, El);
        false -> Sys0
    end.

-spec replay_until_delete1(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_delete1(Pid, Sys0, El) ->
    #system{log = Log} = Sys1 = replay_step(Pid, Sys0),
    case cauder_log:has_delete(Pid, El, Log) of
        true -> replay_until_delete1(Pid, Sys1, El);
        false -> Sys1
    end.

-spec replay_delete(El, LogMap, System) -> NewSystem when
    El :: cauder_map:map_element(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_delete(Key, LogMap, #system{log = Log} = Sys) ->
    case cauder_log:find_delete(Key, Log) of
        {ok, Pid} -> replay_until_delete(Pid, LogMap, Sys, Key);
        error -> Sys
    end.

-spec replay_until_delete(Pid, LogMap, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_delete(Pid, LogMap, Sys0, El) ->
    Proc = cauder_pool:get(Pid, Sys0#system.pool),
    SystemMap = cauder_map:get_map(Sys0#system.maps, Proc#process.node),
    Sys1 = replay_registereds(SystemMap, Sys0),
    Sys2 = replay_fail_reads(El, LogMap, Sys1),
    Sys3 = replay_register(El, Sys2),

    Sys4 = replay_success_reads(El, Sys3),

    Sys5 = replay_spawn(Pid, Sys4),
    Proc = cauder_pool:get(Pid, Sys5#system.pool),
    #system{log = Log2} = Sys6 = replay_reg_del(Proc#process.node, LogMap, Sys5),

    case cauder_log:has_delete(Pid, El, Log2) of
        true -> replay_until_delete1(Pid, Sys6, El);
        false -> Sys6
    end.

-spec replay_success_reads(El, System) -> NewSystem when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_success_reads({A, P, K, _}, #system{log = Log} = Sys0) ->
    %io:format("\nreplay_success_reads\n", []),
    case cauder_log:find_success_read({A, P, K, top}, Log) of
        {ok, Pid} ->
            Sys1 = replay_until_success_read(Pid, Sys0, {A, P, K, top}),
            replay_success_reads({A, P, K, top}, Sys1);
        error ->
            Sys0
    end.

-spec replay_until_success_read(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

replay_until_success_read(Pid, Sys0, El) ->
    %io:format("\nreplay_until_success_read\n", []),
    case cauder_log:has_success_read(Pid, El, Sys0#system.log) of
        true ->
            Sys1 = replay_step(Pid, Sys0),
            replay_until_success_read(Pid, Sys1, El);
        false ->
            Sys0
    end.

-spec replay_read(Pid, Node, LogMap, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Node :: node(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_read(Pid, Node, LogMap, Sys0) ->
    Sys1 = replay_reg_del(Node, LogMap, Sys0),
    replay_step(Pid, Sys1).

%  -spec replay_SendA(Pid, Node, LogMap, System) -> NewSystem when
%      Pid :: cauder_process:id(),
%      Node :: node(),
%      LogMap :: [cauder_map:map_element()],
%      System :: cauder_system:system(),
%      NewSystem :: cauder_system:system().

%  replay_SendA(Pid, Node, LogMap, Sys0) ->
%      replay_reg_del(Node, LogMap, Sys0).

-spec replay_reg_del(Node, LogMap, System) -> NewSystem when
    Node :: node(),
    LogMap :: [cauder_map:map_element()],
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_reg_del(Node, LogMap, OldSystem) ->
    SystemMap = cauder_map:get_map(OldSystem#system.maps, Node),
    %io:format("\nLOGMAP ~p\n", [LogMap]),
    %io:format("\nSystemMap ~p\n", [SystemMap]),
    case LogMap -- SystemMap of
        [{_, _, K, top} = El | _] ->
            %io:format("\nREG ~p\n", [K]),
            NewSystem = replay_register(El, OldSystem),
            replay_reg_del(Node, LogMap, NewSystem);
        [{_, _, K, bot} = El | _] ->
            %io:format("\nDEL ~p\n", [K]),
            NewSystem = replay_delete(El, OldSystem),
            replay_reg_del(Node, LogMap, NewSystem);
        [] ->
            %io:format("I am Here", []),
            OldSystem
    end.
