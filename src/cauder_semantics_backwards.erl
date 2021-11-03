%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-include("cauder.hrl").
-include("cauder_history.hrl").
-include("cauder_process.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    NewSystem :: cauder_system:system().

step(#sys{pool = Pool} = Sys0, Pid) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    {{value, Entry}, RestHist} = cauder_history:pop(Hist),
    Sys = Sys0#sys{
        pool = cauder_pool:update_with(
            Pid,
            fun(P) -> P#process{hist = RestHist} end,
            Pool
        )
    },
    case Entry of
        #h_tau{} ->
            step_tau(Pid, Entry, Sys);
        #h_self{} ->
            step_self(Pid, Entry, Sys);
        #h_node{} ->
            step_node(Pid, Entry, Sys);
        #h_nodes{} ->
            step_nodes(Pid, Entry, Sys);
        #h_spawn{} ->
            step_spawn(Pid, Entry, Sys);
        #h_start{} ->
            step_start(Pid, Entry, Sys);
        #h_send{} ->
            step_send(Pid, Entry, Sys);
        #h_receive{} ->
            step_receive(Pid, Entry, Sys)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
    System :: cauder_system:system(),
    Options :: [cauder_types:option()].

options(#sys{pool = Pool} = Sys) ->
    lists:filtermap(
        fun(#process{pid = Pid}) ->
            case process_option(Pid, Sys#sys{pool = cauder_pool:remove(Pid, Pool)}) of
                ?NULL_OPT -> false;
                Opt -> {true, Opt}
            end
        end,
        cauder_pool:to_list(Pool)
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

step_tau(Pid, #h_tau{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

step_self(Pid, #h_self{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

step_node(Pid, #h_node{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

step_simple(Pid, {Bs, Es, Stk}, #sys{pool = Pool} = Sys) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    Sys#sys{
        pool = cauder_pool:update(P1, Pool)
    }.

step_nodes(
    Pid,
    #h_nodes{
        env = Bs,
        expr = Es,
        stack = Stk,
        nodes = HistNodes
    },
    #sys{
        pool = Pool,
        traces = LMap
    } = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    Sys#sys{
        pool = cauder_pool:update(P, Pool),
        traces = maps:update_with(Pid, fun(Log) -> [{nodes, {HistNodes}} | Log] end, [], LMap)
    }.

step_spawn(
    Pid,
    #h_spawn{
        env = Bs,
        expr = Es,
        stack = Stk,
        node = Node,
        pid = Gid
    },
    #sys{
        pool = Pool,
        traces = LMap,
        x_trace = Trace
    } = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = Gid
    },
    Result =
        case cauder_utils:process_node(Pool, Gid) of
            false -> failure;
            _ -> success
        end,
    NewLog = {spawn, {Node, Gid}, Result},
    Sys#sys{
        pool = cauder_pool:update(P, cauder_pool:remove(Gid, Pool)),
        traces = maps:update_with(Pid, fun(Log) -> [NewLog | Log] end, [NewLog], LMap),
        x_trace = lists:delete(T, Trace)
    }.

step_start(
    Pid,
    #h_start{
        env = Bs,
        expr = Es,
        stack = Stk,
        node = Node,
        success = Success
    },
    #sys{
        pool = Pool,
        traces = LMap,
        nodes = Nodes,
        x_trace = Trace
    } = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    Result =
        case Success of
            true -> success;
            false -> failure
        end,
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = Result,
        node = Node
    },
    Entry1 = {start, Node, Result},
    Sys#sys{
        nodes = lists:delete(Node, Nodes),
        pool = cauder_pool:update(P, Pool),
        x_trace = lists:delete(T, Trace),
        traces = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap)
    }.

step_send(
    Pid,
    #h_send{
        env = Bs,
        expr = Es,
        stack = Stk,
        msg = #message{dest = Dest, value = Val, uid = Uid} = Msg
    },
    #sys{
        mail = Mail,
        pool = Pool,
        traces = LMap,
        x_trace = Trace
    } = Sys
) ->
    {_, OldMsgs} = cauder_mailbox:delete(Msg, Mail),
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    T = #x_trace{
        type = ?RULE_SEND,
        from = Pid,
        to = Dest,
        val = Val,
        time = Uid
    },
    Entry1 = {send, Uid},
    Sys#sys{
        mail = OldMsgs,
        pool = cauder_pool:update(P, Pool),
        traces = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
        x_trace = lists:delete(T, Trace)
    }.

step_receive(
    Pid,
    #h_receive{
        env = Bs,
        expr = Es,
        stack = Stk,
        msg = M = #message{uid = Uid, value = Value, dest = Pid},
        q_pos = QPos
    },
    #sys{
        mail = Mail,
        pool = Pool,
        traces = LMap,
        x_trace = Trace
    } = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    T = #x_trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val = Value,
        time = Uid
    },
    Entry1 = {'receive', Uid},
    Sys#sys{
        mail = cauder_mailbox:insert(M, QPos, Mail),
        pool = cauder_pool:update(P1, Pool),
        traces = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
        x_trace = lists:delete(T, Trace)
    }.

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(Pid, System) -> Option when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Option :: cauder_types:option() | ?NULL_OPT.

process_option(Pid, #sys{pool = Pool} = Sys) ->
    #process{node = Node, hist = Hist} = cauder_pool:get(Pid, Pool),
    case cauder_history:peek(Hist) of
        empty ->
            ?NULL_OPT;
        {value, #h_tau{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEQ};
        {value, #h_self{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SELF};
        {value, #h_node{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODE};
        {value, #h_nodes{nodes = Nodes}} ->
            ProcViewOfNodes = lists:delete(Node, Sys#sys.nodes),
            case ProcViewOfNodes =:= Nodes of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODES};
                false -> ?NULL_OPT
            end;
        {value, #h_spawn{pid = SpawnPid}} ->
            try cauder_pool:get(SpawnPid, Pool) of
                P1 ->
                    case cauder_history:is_empty(P1#process.hist) of
                        true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
                        false -> ?NULL_OPT
                    end
            catch
                % this case covers the scenario of a failed spawn
                error:{badkey, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN}
            end;
        {value, #h_start{node = StartNode, success = true}} ->
            Bool =
                cauder_pool:find_on_node(StartNode, Pool) =:= [] andalso
                    cauder_pool:find_history_nodes(StartNode, Pool) =:= false andalso
                    cauder_pool:find_history_failed_start(StartNode, Pool) =:= false,
            case Bool of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
                false -> ?NULL_OPT
            end;
        {value, #h_start{success = false}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
        {value, #h_send{msg = #message{uid = Uid}}} ->
            case cauder_mailbox:uid_member(Uid, Sys#sys.mail) of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
                false -> ?NULL_OPT
            end;
        {value, #h_receive{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}
    end.
