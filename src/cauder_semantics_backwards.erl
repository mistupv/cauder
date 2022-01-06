%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").
-include("cauder_log.hrl").
-include("cauder_trace.hrl").
-include("cauder_semantics.hrl").

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
        #hist_tau{} ->
            rule_local(Pid, Entry, Sys);
        #hist_self{} ->
            rule_self(Pid, Entry, Sys);
        #hist_node{} ->
            rule_node(Pid, Entry, Sys);
        #hist_nodes{} ->
            rule_nodes(Pid, Entry, Sys);
        #hist_start{} ->
            rule_start(Pid, Entry, Sys);
        #hist_spawn{} ->
            rule_spawn(Pid, Entry, Sys);
        #hist_send{} ->
            rule_send(Pid, Entry, Sys);
        #hist_receive{} ->
            rule_receive(Pid, Entry, Sys)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> #{Pid => Rule} when
    System :: cauder_system:system(),
    Pid :: cauder_process:id(),
    Rule :: cauder_semantics:rule().

options(#system{pool = Pool} = Sys) ->
    lists:foldl(
        fun(#process{pid = Pid}, Map) ->
            case process_option(Pid, Sys) of
                {'ok', Rule} ->
                    maps:put(Pid, Rule, Map);
                'false' ->
                    Map
            end
        end,
        maps:new(),
        cauder_pool:to_list(Pool)
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec rule_local(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_tau(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_local(Pid, #hist_tau{env = Bs, expr = Es, stack = Stk}, Sys) ->
    rule_simple(Pid, {Bs, Es, Stk}, Sys).

-spec rule_self(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_self(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_self(Pid, #hist_self{env = Bs, expr = Es, stack = Stk}, Sys) ->
    rule_simple(Pid, {Bs, Es, Stk}, Sys).

-spec rule_node(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_node(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_node(Pid, #hist_node{env = Bs, expr = Es, stack = Stk}, Sys) ->
    rule_simple(Pid, {Bs, Es, Stk}, Sys).

-spec rule_simple(Pid, {Bs, Es, Stk}, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Bs :: cauder_bindings:bindings(),
    Es :: [cauder_syntax:abstract_expr()],
    Stk :: cauder_stack:stack(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_simple(
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

-spec rule_nodes(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_nodes(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_nodes(
    Pid,
    #hist_nodes{} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#hist_nodes.env,
        expr = Entry#hist_nodes.expr,
        stack = Entry#hist_nodes.stack
    },
    LogAction = #log_nodes{
        nodes = Entry#hist_nodes.nodes
    },
    TraceAction = #trace_nodes{
        nodes = Entry#hist_nodes.nodes
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        trace = cauder_trace:pop(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_start(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_start(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_start(
    Pid,
    #hist_start{node = Node, success = Success} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Entry#hist_start.env,
        expr = Entry#hist_start.expr,
        stack = Entry#hist_start.stack
    },
    LogAction = #log_start{
        node = Node,
        success = Success
    },
    TraceAction = #trace_start{
        node = Node,
        success = Success
    },
    Sys#system{
        pool = cauder_pool:update(P, Pool),
        nodes = lists:delete(Node, Sys#system.nodes),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        trace = cauder_trace:pop(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_spawn(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_spawn(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_spawn(
    Pid,
    #hist_spawn{pid = Gid} = Entry,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#hist_spawn.env,
        expr = Entry#hist_spawn.expr,
        stack = Entry#hist_spawn.stack
    },
    LogAction = #log_spawn{
        node = Entry#hist_spawn.node,
        pid = Gid,
        success = cauder_pool:is_element(Gid, Pool)
    },
    TraceAction = #trace_spawn{
        node = Entry#hist_spawn.node,
        pid = Gid,
        success = cauder_pool:is_element(Gid, Pool)
    },
    Sys#system{
        pool = cauder_pool:update(P1, cauder_pool:remove(Gid, Pool)),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        trace = cauder_trace:pop(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_send(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_send(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_send(
    Pid,
    #hist_send{msg = Msg} = Entry,
    #system{mail = Mail, pool = Pool} = Sys
) ->
    {_, OldMail} = cauder_mailbox:remove(Msg, Mail),
    P0 = cauder_pool:get(Pid, Pool),
    P = P0#process{
        env = Entry#hist_send.env,
        expr = Entry#hist_send.expr,
        stack = Entry#hist_send.stack
    },
    LogAction = #log_send{
        uid = Msg#message.uid
    },
    TraceAction = #trace_send{
        uid = Msg#message.uid
    },
    Sys#system{
        mail = OldMail,
        pool = cauder_pool:update(P, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        trace = cauder_trace:pop(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_receive(Pid, Entry, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Entry :: cauder_history:entry_receive(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_receive(
    Pid,
    #hist_receive{msg = Msg, q_pos = QPos} = Entry,
    #system{mail = Mail, pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    P1 = P0#process{
        env = Entry#hist_receive.env,
        expr = Entry#hist_receive.expr,
        stack = Entry#hist_receive.stack
    },
    LogAction = #log_receive{
        uid = Msg#message.uid
    },
    TraceAction = #trace_receive{
        uid = Msg#message.uid
    },
    Sys#system{
        mail = cauder_mailbox:insert(Msg, QPos, Mail),
        pool = cauder_pool:update(P1, Pool),
        log = cauder_log:push(Pid, LogAction, Sys#system.log),
        trace = cauder_trace:pop(Pid, TraceAction, Sys#system.trace)
    }.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(Pid, System) -> {ok, Rule} | 'false' when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Rule :: cauder_semantics:rule().

process_option(Pid, Sys) ->
    {#process{node = Node, hist = Hist}, Pool1} = cauder_pool:take(Pid, Sys#system.pool),
    case cauder_history:peek(Hist) of
        empty ->
            false;
        {value, #hist_tau{}} ->
            {ok, ?RULE_LOCAL};
        {value, #hist_self{}} ->
            {ok, ?RULE_SELF};
        {value, #hist_node{}} ->
            {ok, ?RULE_NODE};
        {value, #hist_nodes{nodes = Nodes}} ->
            case lists:delete(Node, Sys#system.nodes) of
                Nodes ->
                    {ok, ?RULE_NODES};
                _ ->
                    false
            end;
        {value, #hist_spawn{pid = SpawnPid}} ->
            case cauder_pool:find(SpawnPid, Pool1) of
                error ->
                    % this case covers the scenario of a failed spawn
                    {ok, ?RULE_SPAWN};
                {ok, P1} ->
                    case cauder_history:is_empty(P1#process.hist) of
                        true ->
                            {ok, ?RULE_SPAWN};
                        false ->
                            false
                    end
            end;
        {value, #hist_start{node = StartNode, success = true}} ->
            Bool =
                cauder_pool:find_on_node(StartNode, Pool1) =:= [] andalso
                    cauder_pool:find_history_nodes(StartNode, Pool1) =:= error andalso
                    cauder_pool:find_history_failed_start(StartNode, Pool1) =:= error,
            case Bool of
                true ->
                    {ok, ?RULE_START};
                false ->
                    false
            end;
        {value, #hist_start{success = false}} ->
            {ok, ?RULE_START};
        {value, #hist_send{msg = #message{uid = Uid}}} ->
            case cauder_mailbox:is_element(Uid, Sys#system.mail) of
                true ->
                    {ok, ?RULE_SEND};
                false ->
                    false
            end;
        {value, #hist_receive{}} ->
            {ok, ?RULE_RECEIVE}
    end.
