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
-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can rollback a step in the
%% given system, or not.

-spec can_rollback_step(Pid, System) -> CanRollback when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_step(Pid, #system{pool = Pool}) ->
    case cauder_pool:find(Pid, Pool) of
        {ok, #process{hist = Hist}} ->
            not cauder_history:is_empty(Hist);
        error ->
            false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% rolled back in the given system, or not.

-spec can_rollback_spawn(Pid, System) -> CanRollback when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_spawn(Pid, #system{pool = Pool}) ->
    case cauder_pool:find_history_spawn(Pid, Pool) of
        {ok, _} -> true;
        error -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether the start of the node with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_start(Node, System) -> CanRollback when
    Node :: node(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_start(Node, #system{pool = Pool, nodes = Nodes}) ->
    #process{node = FirstNode} = cauder_pool:first(Pool),
    lists:member(Node, Nodes -- [FirstNode]).

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_send(Uid, System) -> CanRollback when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_send(Uid, #system{pool = Pool}) ->
    case cauder_pool:find_history_send(Uid, Pool) of
        {ok, _} -> true;
        error -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_receive(Uid, System) -> CanRollback when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_receive(Uid, #system{pool = Pool}) ->
    case cauder_pool:find_history_receive(Uid, Pool) of
        {ok, _} -> true;
        error -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether the binding of the variable with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_variable(System, Name) -> CanRollback when
    System :: cauder_system:system(),
    Name :: atom(),
    CanRollback :: boolean().

can_rollback_variable(#system{pool = Pool}, Name) ->
    case cauder_pool:find_variable(Name, Pool) of
        {ok, _} -> true;
        error -> false
    end.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Rolls back a single step in the process with the given pid, in given the
%% system.

-spec rollback_step(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_step(Pid, #system{pool = Pool, nodes = SysNodes, roll = RollLog} = Sys0) ->
    #process{hist = Hist, node = Node} = cauder_pool:get(Pid, Pool),
    ProcViewOfNodes = SysNodes -- [Node],
    case cauder_history:peek(Hist) of
        {value, #h_spawn{pid = SpawnPid}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
            rollback_spawn(Pid, Sys, SpawnPid);
        {value, #h_start{node = StartNode, success = true}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_start(StartNode)},
            rollback_start(Pid, Sys, StartNode);
        {value, #h_nodes{nodes = Nodes}} when ProcViewOfNodes =/= Nodes ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_nodes(Pid)},
            rollback_nodes(Pid, Sys, Nodes);
        {value, #h_send{msg = #message{uid = Uid, dst = Dst} = Msg}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Msg)},
            rollback_send(Pid, Sys, Dst, Uid);
        {value, _} ->
            [#opt{pid = Pid, sem = Sem} | _] = options(Pid, Sys0),
            case Sem of
                % FIXME Why does it go forwards??
                ?FWD_SEM -> cauder_semantics_forwards:step(Pid, Sys0, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Pid, Sys0)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid, in the given
%% system.

-spec rollback_spawn(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_spawn(Pid, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = ParentPid}} = cauder_pool:find_history_spawn(Pid, Pool),
    rollback_until_spawn(ParentPid, Sys#system{roll = []}, Pid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the start of the node with the given name, in the given
%% system.

-spec rollback_start(Node, System) -> NewSystem when
    Node :: node(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_start(Node, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = ParentPid}} = cauder_pool:find_history_start(Node, Pool),
    rollback_until_start(ParentPid, Sys#system{roll = []}, Node).

%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_send(Uid, System) -> NewSystem when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_send(Uid, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = SenderPid}} = cauder_pool:find_history_send(Uid, Pool),
    rollback_until_send(SenderPid, Sys#system{roll = []}, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid, in the given
%% system.

-spec rollback_receive(Uid, System) -> NewSystem when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_receive(Uid, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = ReceiverPid}} = cauder_pool:find_history_receive(Uid, Pool),
    rollback_until_receive(ReceiverPid, Sys#system{roll = []}, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name, in the given
%% system.

-spec rollback_variable(System, Name) -> NewSystem when
    System :: cauder_system:system(),
    Name :: atom(),
    NewSystem :: cauder_system:system().

rollback_variable(#system{pool = Pool} = Sys, Name) ->
    {ok, #process{pid = Pid}} = cauder_pool:find_variable(Name, Pool),
    rollback_until_variable(Pid, Sys#system{roll = []}, Name).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec rollback_nodes(Pid, System, Nodes) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Nodes :: [node()],
    NewSystem :: cauder_system:system().

rollback_nodes(Pid, #system{pool = Pool, nodes = SysNodes} = Sys0, Nodes) ->
    #process{node = Node} = cauder_pool:get(Pid, Pool),
    ProcViewOfNodes = SysNodes -- [Node],
    [FirstNode | _] = ProcViewOfNodes -- Nodes,
    {ok, #process{pid = ParentPid}} = cauder_pool:find_history_start(FirstNode, Pool),
    rollback_until_start(ParentPid, Sys0, FirstNode).

-spec rollback_spawn(Pid, System, SpawnPid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_spawn(Pid, Sys0, SpawnPid) ->
    Opts = options(Pid, Sys0),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SPAWN end, Opts) of
        false ->
            Sys1 = rollback_step(SpawnPid, Sys0),
            rollback_spawn(Pid, Sys1, SpawnPid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Pid, Sys0, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Pid, Sys0)
            end
    end.

-spec rollback_start(Pid, System, SpawnNode) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnNode :: node(),
    NewSystem :: cauder_system:system().

rollback_start(Pid, Sys0, SpawnNode) ->
    Opts = options(Pid, Sys0),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_START end, Opts) of
        false ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_start(Pid, Sys1, SpawnNode);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Pid, Sys0, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Pid, Sys0)
            end
    end.

-spec rollback_send(Pid, System, DestPid, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    DestPid :: cauder_process:id(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_send(Pid, Sys0, DestPid, Uid) ->
    Opts = options(Pid, Sys0),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts) of
        false ->
            Sys1 = rollback_step(DestPid, Sys0),
            rollback_send(Pid, Sys1, DestPid, Uid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?FWD_SEM -> cauder_semantics_forwards:step(Pid, Sys0, ?SCHEDULER_Random, normal);
                ?BWD_SEM -> cauder_semantics_backwards:step(Pid, Sys0)
            end
    end.

%%%=============================================================================

-spec rollback_until_spawn(Pid, System, SpawnPid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_until_spawn(Pid, #system{pool = Pool} = Sys0, SpawnPid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_spawn{pid = SpawnPid}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_spawn(Pid, Sys1, SpawnPid)
    end.

-spec rollback_until_start(Pid, System, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_until_start(Pid, #system{pool = Pool} = Sys0, StartNode) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_start{node = StartNode, success = true}} ->
            case cauder_pool:find_on_node(StartNode, Pool) of
                [P1 | _] ->
                    {ok, P2} = cauder_pool:find_history_spawn(P1#process.pid, Pool),
                    rollback_step(P2#process.pid, Sys0);
                [] ->
                    case cauder_pool:find_history_nodes(StartNode, Pool) of
                        {ok, P1} ->
                            rollback_step(P1#process.pid, Sys0);
                        error ->
                            case cauder_pool:find_history_failed_start(StartNode, Pool) of
                                {ok, P1} -> rollback_step(P1#process.pid, Sys0);
                                error -> undo_step(Pid, Sys0)
                            end
                    end
            end;
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_start(Pid, Sys1, StartNode)
    end.

-spec rollback_until_send(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_until_send(Pid, #system{pool = Pool} = Sys0, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_send{msg = #message{uid = Uid}}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_send(Pid, Sys1, Uid)
    end.

-spec rollback_until_receive(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_until_receive(Pid, #system{pool = Pool} = Sys0, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_receive{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Pid, Sys0, Uid);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_receive(Pid, Sys1, Uid)
    end.

-spec rollback_after_receive(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_after_receive(Pid, Sys0, Uid) ->
    #system{pool = Pool} = Sys1 = rollback_step(Pid, Sys0),
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #h_send{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Pid, Sys1, Uid);
        {value, _} ->
            Sys1
    end.

-spec rollback_until_variable(Pid, System, Name) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Name :: atom(),
    NewSystem :: cauder_system:system().

rollback_until_variable(Pid, #system{pool = Pool} = Sys0, Name) ->
    #process{env = Bs} = cauder_pool:get(Pid, Pool),
    case cauder_bindings:is_bound(Name, Bs) of
        true ->
            Sys0;
        false ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_variable(Pid, Sys1, Name)
    end.

%%%=============================================================================

-spec options(Pid, System) -> Options when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Options :: [cauder_types:option()].

options(Pid, Sys) ->
    Opts = cauder_semantics_backwards:options(Sys),
    cauder_utils:filter_options(Pid, Opts).

-spec undo_step(Pid, System) -> System when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

undo_step(Pid, System) ->
    [#opt{pid = Pid, sem = Sem} | _] = options(Pid, System),
    case Sem of
        ?FWD_SEM -> cauder_semantics_forwards:step(Pid, System, ?SCHEDULER_Random, normal);
        ?BWD_SEM -> cauder_semantics_backwards:step(Pid, System)
    end.
