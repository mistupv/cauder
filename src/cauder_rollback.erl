%%%-----------------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_rollback).

-export([
    can_rollback_step/2,
    can_rollback_spawn/2,
    can_rollback_start/2,
    can_rollback_send/2,
    can_rollback_receive/2,
    can_rollback_variable/2
]).
-export([
    rollback_step/2,
    rollback_spawn/2,
    rollback_start/2,
    rollback_send/2,
    rollback_receive/2,
    rollback_variable/2
]).

-include("cauder.hrl").
-include("cauder_history.hrl").
-include("cauder_process.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can rollback a step in the
%% given system, or not.

-spec can_rollback_step(System, Pid) -> CanRollback when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    CanRollback :: boolean().

can_rollback_step(#sys{pool = Pool}, Pid) ->
    case cauder_pool:find(Pid, Pool) of
        {value, #process{hist = Hist}} ->
            not cauder_history:is_empty(Hist);
        false ->
            false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% rolled back in the given system, or not.

-spec can_rollback_spawn(System, Pid) -> CanRollback when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    CanRollback :: boolean().

can_rollback_spawn(#sys{pool = Pool}, Pid) ->
    cauder_pool:find_history_spawn(Pid, Pool) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the start of the node with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_start(System, Node) -> CanRollback when
    System :: cauder_system:system(),
    Node :: node(),
    CanRollback :: boolean().

can_rollback_start(#sys{nodes = Nodes, pool = Pool}, Node) ->
    #process{node = FirstNode} = cauder_pool:first(Pool),
    lists:member(Node, Nodes -- [FirstNode]).

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_send(System, Uid) -> CanRollback when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    CanRollback :: boolean().

can_rollback_send(#sys{pool = Pool}, Uid) ->
    cauder_pool:find_history_send(Uid, Pool) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_receive(System, Uid) -> CanRollback when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    CanRollback :: boolean().

can_rollback_receive(#sys{pool = Pool}, Uid) ->
    cauder_pool:find_history_receive(Uid, Pool) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the binding of the variable with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_variable(System, Name) -> CanRollback when
    System :: cauder_system:system(),
    Name :: atom(),
    CanRollback :: boolean().

can_rollback_variable(#sys{pool = Pool}, Name) ->
    cauder_pool:find_variable(Name, Pool) =/= false.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Rolls back a single step in the process with the given pid, in given the
%% system.

-spec rollback_step(System, Pid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_step(#sys{pool = Pool, nodes = SysNodes, roll = RollLog} = Sys0, Pid) ->
    #process{hist = Hist, node = Node} = cauder_pool:get(Pid, Pool),
    ProcViewOfNodes = SysNodes -- [Node],
    case cauder_history:peek(Hist) of
        {value, #h_spawn{pid = SpawnPid}} ->
            Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
            rollback_spawn(Sys, Pid, SpawnPid);
        {value, #h_start{node = StartNode, success = true}} ->
            Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_start(StartNode)},
            rollback_start(Sys, Pid, StartNode);
        {value, #h_nodes{nodes = Nodes}} when ProcViewOfNodes =/= Nodes ->
            Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_nodes(Pid)},
            rollback_nodes(Sys, Pid, Nodes);
        {value, #h_send{msg = #message{dest = Dest, uid = Uid} = Msg}} ->
            Sys = Sys0#sys{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Msg)},
            rollback_send(Sys, Pid, Dest, Uid);
        {value, _} ->
            [#opt{pid = Pid, sem = Sem} | _] = options(Sys0, Pid),
            case Sem of
                % FIXME Why does it go forwards??
                ?FWD_SEM -> cauder_semantics_forwards:step(Sys0, Pid, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid, in the given
%% system.

-spec rollback_spawn(System, Pid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_spawn(#sys{pool = Pool} = Sys, Pid) ->
    {value, #process{pid = ParentPid}} = cauder_pool:find_history_spawn(Pid, Pool),
    rollback_until_spawn(Sys#sys{roll = []}, ParentPid, Pid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the start of the node with the given name, in the given
%% system.

-spec rollback_start(System, Node) -> NewSystem when
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_start(#sys{pool = Pool} = Sys, Node) ->
    {value, #process{pid = ParentPid}} = cauder_pool:find_history_start(Node, Pool),
    rollback_until_start(Sys#sys{roll = []}, ParentPid, Node).

%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_send(System, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_send(#sys{pool = Pool} = Sys, Uid) ->
    {value, #process{pid = SenderPid}} = cauder_pool:find_history_send(Uid, Pool),
    rollback_until_send(Sys#sys{roll = []}, SenderPid, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid, in the given
%% system.

-spec rollback_receive(System, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_receive(#sys{pool = Pool} = Sys, Uid) ->
    {value, #process{pid = ReceiverPid}} = cauder_pool:find_history_receive(Uid, Pool),
    rollback_until_receive(Sys#sys{roll = []}, ReceiverPid, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name, in the given
%% system.

-spec rollback_variable(System, Name) -> NewSystem when
    System :: cauder_system:system(),
    Name :: atom(),
    NewSystem :: cauder_system:system().

rollback_variable(#sys{pool = Pool} = Sys, Name) ->
    {value, #process{pid = Pid}} = cauder_pool:find_variable(Name, Pool),
    rollback_until_variable(Sys#sys{roll = []}, Pid, Name).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec rollback_nodes(System, Pid, Nodes) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Nodes :: [node()],
    NewSystem :: cauder_system:system().

rollback_nodes(#sys{nodes = SysNodes, pool = Pool} = Sys0, Pid, Nodes) ->
    #process{node = Node} = cauder_pool:get(Pid, Pool),
    ProcViewOfNodes = SysNodes -- [Node],
    [FirstNode | _] = ProcViewOfNodes -- Nodes,
    {value, #process{pid = ParentPid}} = cauder_pool:find_history_start(FirstNode, Pool),
    rollback_until_start(Sys0, ParentPid, FirstNode).

-spec rollback_spawn(System, Pid, SpawnPid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_spawn(Sys0, Pid, SpawnPid) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SPAWN end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, SpawnPid),
            rollback_spawn(Sys1, Pid, SpawnPid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Sys0, Pid, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

-spec rollback_start(System, Pid, SpawnNode) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    SpawnNode :: node(),
    NewSystem :: cauder_system:system().

rollback_start(Sys0, Pid, SpawnNode) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_START end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_start(Sys1, Pid, SpawnNode);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Sys0, Pid, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

-spec rollback_send(System, Pid, DestPid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    DestPid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_send(Sys0, Pid, DestPid, Uid) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, DestPid),
            rollback_send(Sys1, Pid, DestPid, Uid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Sys0, Pid, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

%%%=============================================================================

-spec rollback_until_spawn(System, Pid, SpawnPid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_until_spawn(#sys{pool = Pool} = Sys0, Pid, SpawnPid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_spawn{pid = SpawnPid}} ->
            rollback_step(Sys0, Pid);
        {value, _} ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_spawn(Sys1, Pid, SpawnPid)
    end.

-spec rollback_until_start(System, Pid, Node) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_until_start(#sys{pool = Pool} = Sys0, Pid, StartNode) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_start{node = StartNode, success = true}} ->
            case cauder_pool:find_on_node(StartNode, Pool) of
                [P1 | _] ->
                    {value, P2} = cauder_pool:find_history_spawn(P1#process.pid, Pool),
                    rollback_step(Sys0, P2#process.pid);
                [] ->
                    case cauder_pool:find_history_nodes(StartNode, Pool) of
                        {value, P1} ->
                            rollback_step(Sys0, P1#process.pid);
                        false ->
                            case cauder_pool:find_history_failed_start(StartNode, Pool) of
                                {value, P1} -> rollback_step(Sys0, P1#process.pid);
                                false -> undo_step(Sys0, Pid)
                            end
                    end
            end;
        {value, _} ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_start(Sys1, Pid, StartNode)
    end.

-spec rollback_until_send(System, Pid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_until_send(#sys{pool = Pool} = Sys0, Pid, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_send{msg = #message{uid = Uid}}} ->
            rollback_step(Sys0, Pid);
        {value, _} ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_send(Sys1, Pid, Uid)
    end.

-spec rollback_until_receive(System, Pid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_until_receive(#sys{pool = Pool} = Sys0, Pid, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_receive{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Sys0, Pid, Uid);
        {value, _} ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_receive(Sys1, Pid, Uid)
    end.

-spec rollback_after_receive(System, Pid, Uid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_system:system().

rollback_after_receive(Sys0, Pid, Uid) ->
    #sys{pool = Pool} = Sys1 = rollback_step(Sys0, Pid),
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_send{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Sys1, Pid, Uid);
        {value, _} ->
            Sys1
    end.

-spec rollback_until_variable(System, Pid, Name) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Name :: atom(),
    NewSystem :: cauder_system:system().

rollback_until_variable(#sys{pool = Pool} = Sys0, Pid, Name) ->
    #process{env = Bs} = cauder_pool:get(Pid, Pool),
    case cauder_bindings:is_bound(Name, Bs) of
        true ->
            Sys0;
        false ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_variable(Sys1, Pid, Name)
    end.

%%%=============================================================================

-spec options(System, Pid) -> Options when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Options :: [cauder_types:option()].

options(Sys, Pid) ->
    Opts = cauder_semantics_backwards:options(Sys),
    cauder_utils:filter_options(Opts, Pid).

-spec undo_step(System, Pid) -> System when
    System :: cauder_system:system(),
    Pid :: cauder_process:id().

undo_step(System, Pid) ->
    [#opt{pid = Pid, sem = Sem} | _] = options(System, Pid),
    case Sem of
        ?FWD_SEM -> cauder_semantics_forwards:step(System, Pid, ?SCHEDULER_Random, normal);
        ?BWD_SEM -> cauder_semantics_backwards:step(System, Pid)
    end.
