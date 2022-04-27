%%%-----------------------------------------------------------------------------
%%% @doc Rollback operator for the reversible semantics for Erlang
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_rollback).

-export([
    can_rollback_step/2,
    can_rollback_start/2,
    can_rollback_spawn/2,
    can_rollback_send/2,
    can_rollback_senda/2,
    can_rollback_receive/2,
    can_rollback_variable/2,
    can_rollback_reg/2,
    can_rollback_del/2
]).
-export([
    rollback_step/2,
    rollback_start/2,
    rollback_spawn/2,
    rollback_send/2,
    rollback_senda/2,
    rollback_receive/2,
    rollback_variable/2,
    rollback_reg/2,
    rollback_del/2
]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").
-include("cauder_semantics.hrl").

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
%% @doc Checks whether the sending of the message with the given uid can be
%% rolled back in the given system, or not.

-spec can_rollback_senda(Uid, System) -> CanRollback when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_senda(Uid, #system{pool = Pool}) ->
    case cauder_pool:find_history_senda(Uid, Pool) of
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
%% @doc Checks whether  can be
%% rolled back in the given system, or not.

-spec can_rollback_reg(El, System) -> CanRollback when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_reg(El, #system{pool = Pool}) ->
    case cauder_pool:find_history_reg(El, Pool) of
        {ok, _} -> true;
        error -> false
    end.

%%------------------------------------------------------------------------------
%% @doc Checks whether  can be
%% rolled back in the given system, or not.

-spec can_rollback_del(El, System) -> CanRollback when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    CanRollback :: boolean().

can_rollback_del(El, #system{pool = Pool}) ->
    case cauder_pool:find_history_del(El, Pool) of
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
        {value, #hist_spawn{pid = SpawnPid}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_spawn(SpawnPid)},
            rollback_spawn(Pid, Sys, SpawnPid);
        {value, #hist_start{node = StartNode, success = true}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_start(StartNode)},
            rollback_start(Pid, Sys, StartNode);
        {value, #hist_nodes{nodes = Nodes}} when ProcViewOfNodes =/= Nodes ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_nodes(Pid)},
            rollback_nodes(Pid, Sys, Nodes);
        {value, #hist_send{msg = #message{uid = Uid, dst = Dst} = Msg}} ->
            Sys = Sys0#system{roll = RollLog ++ cauder_utils:gen_log_send(Pid, Msg)},
            rollback_send(Pid, Sys, Dst, Uid);
        {value, #hist_sendA{node = Node, mapEl = El, msg = #message{uid = Uid, dst = Dst}}} ->
            Sys = Sys0#system{},
            rollback_senda(Pid, Sys, Dst, Uid, El, Node);
        {value, #hist_regS{mapEl = El, node = Node}} ->
            Sys = Sys0#system{},
            rollback_reg(Pid, Sys, El, Node);
        {value, #hist_del{mapEl = El, map = Map, node = Node}} ->
            Sys = Sys0#system{},
            rollback_del(Pid, Sys, El, Map, Node);
        {value, #hist_registered{map = Map, node = Node}} ->
            Sys = Sys0#system{},
            rollback_registered(Pid, Sys, Map, Node);
        {value, #hist_readF{atom = El, mapGhost = MapGhost, node = Node}} ->
            Sys = Sys0#system{},
            rollback_read_f(Pid, Sys, El, MapGhost, Node);
        {value, #hist_readS{mapEl = El, node = Node}} ->
            Sys = Sys0#system{},
            rollback_read_s(Pid, Sys, El, Node);
        {value, _} ->
            cauder_semantics_backwards:step(Pid, Sys0)
    end.

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
%% @doc Rolls back the sending of the message with the given uid, in the given
%% system.

-spec rollback_senda(Uid, System) -> NewSystem when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_senda(Uid, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = SenderPid}} = cauder_pool:find_history_senda(Uid, Pool),
    rollback_until_senda(SenderPid, Sys#system{roll = []}, Uid).

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
%% @doc Rolls back , in the given
%% system.

-spec rollback_reg(El, System) -> NewSystem when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_reg(El, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = Pid}} = cauder_pool:find_history_reg(El, Pool),
    rollback_until_reg(Pid, Sys#system{roll = []}, El).

%%------------------------------------------------------------------------------
%% @doc Rolls back , in the given
%% system.

-spec rollback_del(El, System) -> NewSystem when
    El :: cauder_map:map_element(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rollback_del(El, #system{pool = Pool} = Sys) ->
    {ok, #process{pid = Pid}} = cauder_pool:find_history_del(El, Pool),
    rollback_until_del(Pid, Sys#system{roll = []}, El).

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

-spec rollback_start(Pid, System, SpawnNode) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnNode :: node(),
    NewSystem :: cauder_system:system().

rollback_start(Pid, Sys0, SpawnNode) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_START} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_start(Pid, Sys1, SpawnNode)
    end.

-spec rollback_spawn(Pid, System, SpawnPid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_spawn(Pid, Sys0, SpawnPid) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_SPAWN} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Sys1 = rollback_step(SpawnPid, Sys0),
            rollback_spawn(Pid, Sys1, SpawnPid)
    end.

-spec rollback_send(Pid, System, DestPid, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    DestPid :: cauder_process:id(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_send(Pid, Sys0, DestPid, Uid) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_SEND} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Sys1 = rollback_step(DestPid, Sys0),
            rollback_send(Pid, Sys1, DestPid, Uid)
    end.

-spec rollback_senda(Pid, System, DestPid, Uid, El, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    DestPid :: cauder_process:id(),
    Uid :: cauder_message:uid(),
    El :: cauder_map:map_element(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_senda(Pid, Sys0, DestPid, Uid, El, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_SEND} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Map = cauder_map:get_map(Sys0#system.maps, Node),
            case cauder_map:is_in_map(Map, El) of
                true ->
                    Sys1 = rollback_step(DestPid, Sys0),
                    rollback_senda(Pid, Sys1, DestPid, Uid, El, Node);
                false ->
                    {ok, #process{pid = DelPid}} = cauder_pool:find_history_del(El, Sys0#system.pool),
                    Sys1 = rollback_step(DelPid, Sys0),
                    rollback_senda(Pid, Sys1, DestPid, Uid, El, Node)
            end
    end.

-spec rollback_reg(Pid, System, El, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_reg(Pid, Sys0, El, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_REG} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Map = cauder_map:get_map(Sys0#system.maps, Node),
            PidUndo =
                case cauder_map:is_in_map(Map, El) of
                    true ->
                        {ok, #process{pid = ReadPid}} = cauder_pool:find_read_map(Sys0#system.pool, Node, El),
                        ReadPid;
                    false ->
                        {ok, #process{pid = DelPid}} = cauder_pool:find_history_del(El, Sys0#system.pool),
                        DelPid
                end,
            Sys1 = rollback_step(PidUndo, Sys0),
            rollback_reg(Pid, Sys1, El, Node)
    end.

-spec rollback_del(Pid, System, El, MapG, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    MapG :: [cauder_map:map_element()],
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_del(Pid, Sys0, El = {A, P, K, _}, MapG, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_DEL} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            case cauder_pool:find_failed_read_map(Sys0#system.pool, Node, {A, P, K, bot}) of
                {ok, #process{pid = PidUndo}} ->
                    Sys1 = rollback_step(PidUndo, Sys0),
                    rollback_del(Pid, Sys1, El, MapG, Node);
                error ->
                    Map = cauder_map:get_map(Sys0#system.maps, Node),
                    LG = cauder_map:get_ghost_list(A, Map, []) ++ cauder_map:get_ghost_list(P, Map, []),
                    case LG -- MapG of
                        [{Atom1, Pid1, K1, bot} | _] ->
                            {ok, #process{pid = PidUndo}} = cauder_pool:find_history_del(
                                {Atom1, Pid1, K1, top}, Sys0#system.pool
                            ),
                            Sys1 = rollback_step(PidUndo, Sys0),
                            rollback_del(Pid, Sys1, El, MapG, Node);
                        [] ->
                            case cauder_map:find_atom(P, Map) of
                                {Atom, Key} ->
                                    {ok, #process{pid = PidUndo}} = cauder_pool:find_history_reg(
                                        {Atom, P, Key, top}, Sys0#system.pool
                                    ),
                                    Sys1 = rollback_step(PidUndo, Sys0),
                                    rollback_del(Pid, Sys1, El, MapG, Node);
                                undefined ->
                                    {PidReg, Key} = cauder_map:find_pid(A, Map),
                                    {ok, #process{pid = PidUndo}} = cauder_pool:find_history_reg(
                                        {A, PidReg, Key, top}, Sys0#system.pool
                                    ),
                                    Sys1 = rollback_step(PidUndo, Sys0),
                                    rollback_del(Pid, Sys1, El, MapG, Node)
                            end
                    end
            end
    end.

-spec rollback_read_s(Pid, System, El, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: [cauder_map:map_element()],
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_read_s(Pid, Sys0, El, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_READ} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            SystemMap = cauder_map:get_map(Sys0#system.maps, Node),
            PidUndo =
              case El of
                    [{A, P, K, _}] ->
                        {ok, #process{pid = PidEl}} = cauder_pool:find_history_del({A, P, K, top}, Sys0#system.pool),
                        PidEl;
                    [{A1, P1, K1, _} | [{A2, P2, K2, _}]] ->
                        case cauder_map:is_in_map(SystemMap, {A1, P1, K1, top}) of
                            true ->
                                {ok, #process{pid = PidEl1}} = cauder_pool:find_history_del({A2, P2, K2, top}, Sys0#system.pool),
                                PidEl1;
                            false ->
                                {ok, #process{pid = PidEl2}} = cauder_pool:find_history_del({A1, P1, K1, top}, Sys0#system.pool),
                                PidEl2
                        end
                end,
            Sys1 = rollback_step(PidUndo, Sys0),
            rollback_read_s(Pid, Sys1, El, Node)
    end.

-spec rollback_read_f(Pid, System, El, MapGhost, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: atom() | cauder_process:id(),
    MapGhost :: [cauder_map:map_element()],
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_read_f(Pid, Sys0, El, MapGhost, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_READ} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            Map = cauder_map:get_map(Sys0#system.maps, Node),
            SystemGhost = cauder_map:get_ghost_list(El, Map, []),
            PidUndo =
                case SystemGhost -- MapGhost of
                    [] ->
                        Tuple = cauder_map:find_el(El, Map),
                        {ok, #process{pid = PidU}} = cauder_pool:find_history_reg(Tuple, Sys0#system.pool),
                        PidU;
                    [{A, P, K, _} | _] ->
                        {ok, #process{pid = DelPid}} = cauder_pool:find_history_del({A, P, K, top}, Sys0#system.pool),
                        DelPid
                end,
            Sys1 = rollback_step(PidUndo, Sys0),
            rollback_read_f(Pid, Sys1, El, MapGhost, Node)
    end.

-spec rollback_registered(Pid, System, Map, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Map :: [cauder_map:map_element()],
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_registered(Pid, Sys0, Map, Node) ->
    Opts = cauder_semantics_backwards:options(Sys0),
    case maps:find(Pid, Opts) of
        {ok, ?RULE_READ} ->
            cauder_semantics_backwards:step(Pid, Sys0);
        error ->
            SystemMap = cauder_map:get_map(Sys0#system.maps, Node),
            PidUndo =
                case SystemMap -- Map of
                    [{Atom1, Pid1, K1, top} | _] ->
                        {ok, #process{pid = ReadPid}} = cauder_pool:find_history_reg(
                            {Atom1, Pid1, K1, top}, Sys0#system.pool
                        ),
                        ReadPid;
                    [{Atom1, Pid1, K1, bot} | _] ->
                        {ok, #process{pid = DelPid}} = cauder_pool:find_history_del(
                            {Atom1, Pid1, K1, top}, Sys0#system.pool
                        ),
                        DelPid
                end,
            Sys1 = rollback_step(PidUndo, Sys0),
            rollback_registered(Pid, Sys1, Map, Node)
    end.

%%%=============================================================================

-spec rollback_until_start(Pid, System, Node) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Node :: node(),
    NewSystem :: cauder_system:system().

rollback_until_start(Pid, #system{pool = Pool} = Sys0, StartNode) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_start{node = StartNode, success = true}} ->
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
                                error -> cauder_semantics_backwards:step(Pid, Sys0)
                            end
                    end
            end;
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_start(Pid, Sys1, StartNode)
    end.

-spec rollback_until_spawn(Pid, System, SpawnPid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    SpawnPid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

rollback_until_spawn(Pid, #system{pool = Pool} = Sys0, SpawnPid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_spawn{pid = SpawnPid}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_spawn(Pid, Sys1, SpawnPid)
    end.

-spec rollback_until_send(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_until_send(Pid, #system{pool = Pool} = Sys0, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_send{msg = #message{uid = Uid}}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_send(Pid, Sys1, Uid)
    end.

-spec rollback_until_senda(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_until_senda(Pid, #system{pool = Pool} = Sys0, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_sendA{msg = #message{uid = Uid}}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_senda(Pid, Sys1, Uid)
    end.

-spec rollback_until_receive(Pid, System, Uid) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Uid :: cauder_message:uid(),
    NewSystem :: cauder_system:system().

rollback_until_receive(Pid, #system{pool = Pool} = Sys0, Uid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_receive{msg = #message{uid = Uid}}} ->
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
        {value, #hist_send{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Pid, Sys1, Uid);
        {value, #hist_sendA{msg = #message{uid = Uid}}} ->
            rollback_after_receive(Pid, Sys1, Uid);
        {value, _} ->
            Sys1
    end.

-spec rollback_until_reg(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

rollback_until_reg(Pid, #system{pool = Pool} = Sys0, El) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_regS{mapEl = El}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_reg(Pid, Sys1, El)
    end.

-spec rollback_until_del(Pid, System, El) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    El :: cauder_map:map_element(),
    NewSystem :: cauder_system:system().

rollback_until_del(Pid, #system{pool = Pool} = Sys0, El) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        {value, #hist_del{mapEl = El}} ->
            rollback_step(Pid, Sys0);
        {value, _} ->
            Sys1 = rollback_step(Pid, Sys0),
            rollback_until_del(Pid, Sys1, El)
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
