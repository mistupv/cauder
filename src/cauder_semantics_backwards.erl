%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-include("cauder.hrl").
-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").
-include("cauder_log.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(Pid, System) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

step(Pid, #system{pool = Pool} = Sys0) ->
    #process{hist = Hist} = cauder_pool:get(Pid, Pool),
    {{value, Entry}, RestHist} = cauder_history:pop(Hist),
    Sys = Sys0#system{
        pool = cauder_pool:update_with(
            Pid,
            fun(P) -> P#process{hist = RestHist} end,
            Pool
        )
    },
    case Entry of
        #h_tau{} ->
            rule_local(Pid, Entry, Sys);
        #h_self{} ->
            rule_self(Pid, Entry, Sys);
        #h_node{} ->
            rule_node(Pid, Entry, Sys);
        #h_nodes{} ->
            rule_nodes(Pid, Entry, Sys);
        #h_spawn{} ->
            rule_spawn(Pid, Entry, Sys);
        #h_start{} ->
            rule_start(Pid, Entry, Sys);
        #h_send{} ->
            rule_send(Pid, Entry, Sys);
        #h_receive{} ->
            rule_receive(Pid, Entry, Sys)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
    System :: cauder_system:system(),
    Options :: [cauder_types:option()].

options(#system{pool = Pool} = Sys) ->
    lists:filtermap(
        fun(#process{pid = Pid}) ->
            case process_option(Pid, Sys#system{pool = cauder_pool:remove(Pid, Pool)}) of
                ?NULL_OPT -> false;
                Opt -> {true, Opt}
            end
        end,
        cauder_pool:to_list(Pool)
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

rule_local(Pid, #h_tau{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

rule_self(Pid, #h_self{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

rule_node(Pid, #h_node{env = Bs, expr = Es, stack = Stk}, Sys) ->
    step_simple(Pid, {Bs, Es, Stk}, Sys).

step_simple(
    Pid,
    {Bs, Es, Stk},
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Bs,
        expr = Es,
        stack = Stk
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool)
    }.

rule_nodes(
    Pid,
    #h_nodes{} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#h_nodes.env,
        expr = Entry#h_nodes.expr,
        stack = Entry#h_nodes.stack
    },
    LogAction = #log_nodes{
        nodes = Entry#h_nodes.nodes
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log)
    }.

rule_spawn(
    Pid,
    #h_spawn{pid = Gid} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#h_spawn.env,
        expr = Entry#h_spawn.expr,
        stack = Entry#h_spawn.stack
    },
    LogAction = #log_spawn{
        node = Entry#h_spawn.node,
        pid = Gid,
        success = cauder_pool:is_element(Gid, Pool)
    },
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = Gid
    },
    Sys#system{
        pool = cauder_pool:update(P1, cauder_pool:remove(Gid, Pool)),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        x_trace = lists:delete(T, Sys#system.x_trace)
    }.

rule_start(
    Pid,
    #h_start{node = Node, success = Success} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Entry#h_start.env,
        expr = Entry#h_start.expr,
        stack = Entry#h_start.stack
    },
    LogAction = #log_start{
        node = Node,
        success = Success
    },
    Res =
        case Success of
            true -> success;
            false -> failure
        end,
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = Res,
        node = Node
    },
    Sys#system{
        pool = cauder_pool:update(P, Pool),
        nodes = lists:delete(Node, Sys#system.nodes),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        x_trace = lists:delete(T, Sys#system.x_trace)
    }.

rule_send(
    Pid,
    #h_send{msg = #message{uid = Uid, dst = Dst, val = Val} = Msg} = Entry,
    #system{mail = Mail, pool = Pool} = Sys
) ->
    {_, OldMail} = cauder_mailbox:remove(Msg, Mail),
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Entry#h_send.env,
        expr = Entry#h_send.expr,
        stack = Entry#h_send.stack
    },
    LogAction = #log_send{
        uid = Uid
    },
    T = #x_trace{
        type = ?RULE_SEND,
        from = Pid,
        to = Dst,
        val = Val,
        time = Uid
    },
    Sys#system{
        mail = OldMail,
        pool = cauder_pool:update(P, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        x_trace = lists:delete(T, Sys#system.x_trace)
    }.

rule_receive(
    Pid,
    #h_receive{msg = #message{uid = Uid, dst = Pid, val = Value} = Msg, q_pos = QPos} = Entry,
    #system{mail = Mail, pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#h_receive.env,
        expr = Entry#h_receive.expr,
        stack = Entry#h_receive.stack
    },
    LogAction = #log_receive{
        uid = Uid
    },
    T = #x_trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val = Value,
        time = Uid
    },
    Sys#system{
        mail = cauder_mailbox:insert(Msg, QPos, Mail),
        pool = cauder_pool:update(P1, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        x_trace = lists:delete(T, Sys#system.x_trace)
    }.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(Pid, System) -> Option when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Option :: cauder_types:option() | ?NULL_OPT.

process_option(Pid, #system{pool = Pool} = Sys) ->
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
            ProcViewOfNodes = lists:delete(Node, Sys#system.nodes),
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
                    cauder_pool:find_history_nodes(StartNode, Pool) =:= error andalso
                    cauder_pool:find_history_failed_start(StartNode, Pool) =:= error,
            case Bool of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
                false -> ?NULL_OPT
            end;
        {value, #h_start{success = false}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
        {value, #h_send{msg = #message{uid = Uid}}} ->
            case cauder_mailbox:is_element(Uid, Sys#system.mail) of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
                false -> ?NULL_OPT
            end;
        {value, #h_receive{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}
    end.
