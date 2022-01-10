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

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks whether the process with the given pid can rollback a step in the
%% given system, or not.

-spec can_rollback_step(System, Pid) -> CanRollback when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    CanRollback :: boolean().

can_rollback_step(#system{pool = PMap}, Pid) when is_map_key(Pid, PMap) ->
    #process{hist = Hist} = maps:get(Pid, PMap),
    Hist =/= [];
can_rollback_step(_, _) ->
    false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the spawning of the process with the given pid can be
%% rolled back in the given system, or not.

-spec can_rollback_spawn(System, Pid) -> CanRollback when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    CanRollback :: boolean().

can_rollback_spawn(#system{pool = PMap}, Pid) -> cauder_utils:find_process_with_spawn(PMap, Pid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the start of the node with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_start(System, Node) -> CanRollback when
    System :: cauder_types:system(),
    Node :: node(),
    CanRollback :: boolean().

can_rollback_start(#system{nodes = Nodes, pool = PMap}, Node) ->
    #process{node = FirstNode} = maps:get(lists:min(maps:keys(PMap)), PMap),
    lists:member(Node, Nodes -- [FirstNode]).

%%------------------------------------------------------------------------------
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_send(System, Uid) -> CanRollback when
    System :: cauder_types:system(),
    Uid :: cauder_mailbox:uid(),
    CanRollback :: boolean().

can_rollback_send(#system{pool = PMap}, Uid) -> cauder_utils:find_process_with_send(PMap, Uid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the reception of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_receive(System, Uid) -> CanRollback when
    System :: cauder_types:system(),
    Uid :: cauder_mailbox:uid(),
    CanRollback :: boolean().

can_rollback_receive(#system{pool = PMap}, Uid) -> cauder_utils:find_process_with_receive(PMap, Uid) =/= false.

%%------------------------------------------------------------------------------
%% @doc Checks whether the binding of the variable with the given name can be
%% rolled back in the given system, or not.

-spec can_rollback_variable(System, Name) -> CanRollback when
    System :: cauder_types:system(),
    Name :: atom(),
    CanRollback :: boolean().

can_rollback_variable(#system{pool = PMap}, Name) -> cauder_utils:find_process_with_variable(PMap, Name) =/= false.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Rolls back a single step in the process with the given pid, in given the
%% system.

-spec rollback_step(System, Pid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

rollback_step(#system{pool = PMap, nodes = SysNodes, roll = RollLog} = Sys0, Pid) ->
    #process{hist = [Entry | _], node = Node} = maps:get(Pid, PMap),
    ProcViewOfNodes = SysNodes -- [Node],
    case Entry of
        {spawn, _Bs, _E, _Stk, _Node, SpawnPid} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
            rollback_spawn(Sys, Pid, SpawnPid);
        {start, success, _Bs, _E, _Stk, SpawnNode} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_start(SpawnNode)},
            rollback_start(Sys, Pid, SpawnNode);
        {nodes, _Bs, _E, _Stk, Nodes} when ProcViewOfNodes =/= Nodes ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_nodes(Pid)},
            rollback_nodes(Sys, Pid, Nodes);
        {send, _Bs, _E, _Stk, #message{dst = Dest, uid = Uid} = Msg} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Msg)},
            rollback_send(Sys, Pid, Dest, Uid);
        _ ->
            [#opt{pid = Pid, sem = Sem} | _] = options(Sys0, Pid),
            case Sem of
                ?SEM_FWD -> cauder_semantics_forwards:step(Sys0, Pid);
                ?SEM_BWD -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid, in the given
%% system.

-spec rollback_spawn(System, Pid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

rollback_spawn(#system{pool = PMap} = Sys, Pid) ->
    {value, #process{pid = ParentPid}} = cauder_utils:find_process_with_spawn(PMap, Pid),
    rollback_until_spawn(Sys#system{roll = []}, ParentPid, Pid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the start of the node with the given name, in the given
%% system.

-spec rollback_start(System, Node) -> NewSystem when
    System :: cauder_types:system(),
    Node :: node(),
    NewSystem :: cauder_types:system().

rollback_start(#system{pool = PMap} = Sys, Node) ->
    {value, #process{pid = ParentPid}} = cauder_utils:find_process_with_start(PMap, Node),
    rollback_until_start(Sys#system{roll = []}, ParentPid, Node).

%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_send(System, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_send(#system{pool = PMap} = Sys, Uid) ->
    {value, #process{pid = SenderPid}} = cauder_utils:find_process_with_send(PMap, Uid),
    rollback_until_send(Sys#system{roll = []}, SenderPid, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid, in the given
%% system.

-spec rollback_receive(System, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_receive(#system{pool = PMap} = Sys, Uid) ->
    {value, #process{pid = ReceiverPid}} = cauder_utils:find_process_with_receive(PMap, Uid),
    rollback_until_receive(Sys#system{roll = []}, ReceiverPid, Uid).

%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name, in the given
%% system.

-spec rollback_variable(System, Name) -> NewSystem when
    System :: cauder_types:system(),
    Name :: atom(),
    NewSystem :: cauder_types:system().

rollback_variable(#system{pool = PMap} = Sys, Name) ->
    {value, #process{pid = Pid}} = cauder_utils:find_process_with_variable(PMap, Name),
    rollback_until_variable(Sys#system{roll = []}, Pid, Name).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec rollback_nodes(System, Pid, Nodes) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Nodes :: [node()],
    NewSystem :: cauder_types:system().

rollback_nodes(#system{nodes = SysNodes, pool = PMap} = Sys0, Pid, Nodes) ->
    #process{node = Node} = maps:get(Pid, PMap),
    ProcViewOfNodes = SysNodes -- [Node],
    [FirstNode | _] = ProcViewOfNodes -- Nodes,
    {value, #process{pid = ParentPid}} = cauder_utils:find_process_with_start(PMap, FirstNode),
    rollback_until_start(Sys0, ParentPid, FirstNode).

-spec rollback_spawn(System, Pid, SpawnPid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    SpawnPid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

rollback_spawn(Sys0, Pid, SpawnPid) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SPAWN end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, SpawnPid),
            rollback_spawn(Sys1, Pid, SpawnPid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?SEM_FWD -> cauder_semantics_forwards:step(Sys0, Pid);
                ?SEM_BWD -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

-spec rollback_start(System, Pid, SpawnNode) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    SpawnNode :: node(),
    NewSystem :: cauder_types:system().

rollback_start(Sys0, Pid, SpawnNode) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_START end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_start(Sys1, Pid, SpawnNode);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?SEM_FWD -> cauder_semantics_forwards:step(Sys0, Pid);
                ?SEM_BWD -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

-spec rollback_send(System, Pid, DestPid, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    DestPid :: cauder_types:proc_id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_send(Sys0, Pid, DestPid, Uid) ->
    Opts = options(Sys0, Pid),
    case lists:search(fun(#opt{rule = Rule}) -> Rule =:= ?RULE_SEND end, Opts) of
        false ->
            Sys1 = rollback_step(Sys0, DestPid),
            rollback_send(Sys1, Pid, DestPid, Uid);
        {value, #opt{pid = Pid, sem = Sem}} ->
            case Sem of
                ?SEM_FWD -> cauder_semantics_forwards:step(Sys0, Pid);
                ?SEM_BWD -> cauder_semantics_backwards:step(Sys0, Pid)
            end
    end.

%%%=============================================================================

-spec rollback_until_spawn(System, Pid, SpawnPid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    SpawnPid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

rollback_until_spawn(#system{pool = PMap} = Sys0, Pid, SpawnPid) ->
    #process{hist = [Entry | _]} = maps:get(Pid, PMap),
    case Entry of
        {spawn, _Bs, _Es, _Stk, _Node, SpawnPid} ->
            rollback_step(Sys0, Pid);
        _ ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_spawn(Sys1, Pid, SpawnPid)
    end.

-spec rollback_until_start(System, Pid, Node) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Node :: node(),
    NewSystem :: cauder_types:system().

rollback_until_start(#system{pool = PMap} = Sys0, Pid, SpawnNode) ->
    #process{hist = [Entry | _]} = maps:get(Pid, PMap),
    case Entry of
        {start, success, _Bs, _Es, _Stk, SpawnNode} ->
            ProcsOnNode = cauder_utils:find_process_on_node(PMap, SpawnNode),
            ProcsWithRead = cauder_utils:find_process_with_read(PMap, SpawnNode),
            ProcsWithFailedStart = cauder_utils:find_process_with_failed_start(PMap, SpawnNode),
            case {ProcsOnNode, ProcsWithRead, ProcsWithFailedStart} of
                {false, false, false} ->
                    undo_step(Sys0, Pid);
                {{value, #process{pid = ProcOnNodePid}}, _, _} ->
                    {value, #process{pid = ParentPid}} = cauder_utils:find_process_with_spawn(PMap, ProcOnNodePid),
                    rollback_step(Sys0, ParentPid);
                {_, {value, #process{pid = FirstProcPid}}, _} ->
                    rollback_step(Sys0, FirstProcPid);
                {_, _, {value, #process{pid = FirstProcPid}}} ->
                    rollback_step(Sys0, FirstProcPid)
            end;
        _ ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_start(Sys1, Pid, SpawnNode)
    end.

-spec rollback_until_send(System, Pid, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_until_send(#system{pool = PMap} = Sys0, Pid, Uid) ->
    #process{hist = [Entry | _]} = maps:get(Pid, PMap),
    case Entry of
        {send, _Bs, _Es, _Stk, #message{uid = Uid}} ->
            rollback_step(Sys0, Pid);
        _ ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_send(Sys1, Pid, Uid)
    end.

-spec rollback_until_receive(System, Pid, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_until_receive(#system{pool = PMap} = Sys0, Pid, Uid) ->
    #process{hist = [Entry | _]} = maps:get(Pid, PMap),
    case Entry of
        {rec, _Bs, _Es, _Stk, #message{uid = Uid}, _QPos} ->
            rollback_after_receive(Sys0, Pid, Uid);
        _ ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_receive(Sys1, Pid, Uid)
    end.

-spec rollback_after_receive(System, Pid, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rollback_after_receive(Sys0, Pid, Uid) ->
    #system{pool = PMap} = Sys1 = rollback_step(Sys0, Pid),
    #process{hist = [Entry | _]} = maps:get(Pid, PMap),
    case Entry of
        {rec, _Bs, _E, _Stk, #message{uid = Uid}, _QPos} ->
            rollback_after_receive(Sys1, Pid, Uid);
        _ ->
            Sys1
    end.

-spec rollback_until_variable(System, Pid, Name) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Name :: atom(),
    NewSystem :: cauder_types:system().

rollback_until_variable(#system{pool = PMap} = Sys0, Pid, Name) ->
    #process{env = Bs} = maps:get(Pid, PMap),
    case maps:is_key(Name, Bs) of
        true ->
            Sys0;
        false ->
            Sys1 = rollback_step(Sys0, Pid),
            rollback_until_variable(Sys1, Pid, Name)
    end.

%%%=============================================================================

-spec options(System, Pid) -> Options when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    Options :: [cauder_types:option()].

options(Sys, Pid) ->
    Opts = cauder_semantics_backwards:options(Sys),
    cauder_utils:filter_options(Opts, Pid).

-spec undo_step(System, Pid) -> System when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id().

undo_step(System, Pid) ->
    [#opt{pid = Pid, sem = Sem} | _] = options(System, Pid),
    case Sem of
        ?SEM_FWD -> cauder_semantics_forwards:step(System, Pid);
        ?SEM_BWD -> cauder_semantics_backwards:step(System, Pid)
    end.
