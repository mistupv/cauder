%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([step/4, options/2]).

-import(cauder_eval, [is_reducible/2]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").
-include("cauder_eval.hrl").
-include("cauder_log.hrl").
-include("cauder_trace.hrl").
-include("cauder_semantics.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(Pid, System, MessageScheduler, Mode) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    MessageScheduler :: cauder_message:scheduler(),
    Mode :: normal | replay,
    NewSystem :: cauder_system:system().

step(Pid, Sys, Sched, Mode) ->
    #process{stack = Stk0, env = Bs0, expr = Es0} = cauder_pool:get(Pid, Sys#system.pool),
    Result = cauder_eval:exprs(Es0, Bs0, Stk0),
    case Result#result.label of
        #label_tau{} ->
            rule_local(Pid, Result, Sys);
        #label_self{} ->
            rule_self(Pid, Result, Sys);
        #label_node{} ->
            rule_node(Pid, Result, Sys);
        #label_nodes{} ->
            rule_nodes(Pid, Result, Sys);
        #label_start{} ->
            rule_start(Pid, Result, Sys);
        #label_spawn_fun{} ->
            rule_spawn(Pid, Result, Sys);
        #label_spawn_mfa{} ->
            rule_spawn(Pid, Result, Sys);
        #label_send{} ->
            rule_send(Pid, Result, Sys);
        #label_receive{var = Var} when Result#result.expr == [Var] ->
            rule_receive(Pid, Result, Mode, Sched, Sys)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System, Mode) -> #{Pid => Rule} when
    System :: cauder_system:system(),
    Mode :: normal | replay,
    Pid :: cauder_process:id(),
    Rule :: cauder_semantics:rule().

options(#system{pool = Pool} = Sys, Mode) ->
    lists:foldl(
        fun(#process{pid = Pid, expr = E}, Map) ->
            case expression_option(E, Pid, Sys, Mode) of
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

-spec rule_local(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_local(
    Pid,
    #result{label = #label_tau{}} = Result,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    HistEntry = #hist_tau{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = Result#result.expr,
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool)
    }.

-spec rule_self(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_self(
    Pid,
    #result{label = #label_self{var = VarPid}} = Result,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    HistEntry = #hist_self{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = cauder_syntax:replace_variable(Result#result.expr, VarPid, Pid),
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool)
    }.

-spec rule_node(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_node(
    Pid,
    #result{label = #label_node{var = VarNode}} = Result,
    #system{pool = Pool} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),
    HistEntry = #hist_node{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = cauder_syntax:replace_variable(Result#result.expr, VarNode, P0#process.node),
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool)
    }.

-spec rule_nodes(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_nodes(
    Pid,
    #result{label = #label_nodes{var = VarNodes}} = Result,
    #system{pool = Pool, log = Log0} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),

    {LogEntry, Log1} =
        case cauder_log:pop_nodes(Pid, Log0) of
            {#log_nodes{} = Entry, Log} ->
                {Entry, Log};
            error ->
                {Sys#system.nodes, Log0}
        end,
    OtherNodes = lists:delete(P0#process.node, LogEntry#log_nodes.nodes),

    HistEntry = #hist_nodes{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack,
        nodes = OtherNodes
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = cauder_syntax:replace_variable(Result#result.expr, VarNodes, OtherNodes),
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    TraceAction = #trace_nodes{
        nodes = OtherNodes
    },
    Sys#system{
        pool = cauder_pool:update(P1, Pool),
        log = Log1,
        trace = cauder_trace:push(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_start(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_start(
    Pid,
    #result{label = #label_start{var = VarNode, name = Name} = Label} = Result,
    #system{pool = Pool, nodes = Nodes0, log = Log0} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),

    Host =
        case P0#process.node of
            'nonode@nohost' ->
                error(not_alive);
            ProcessNode ->
                case Label#label_start.host of
                    'undefined' ->
                        [_Name, Host0] = string:split(atom_to_list(ProcessNode), <<"$">>, 'leading'),
                        Host0;
                    Host0 ->
                        Host0
                end
        end,

    Node = list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)),

    {LogEntry, Log1} =
        case cauder_log:pop_start(Pid, Log0) of
            {#log_start{node = Node} = Entry, Log} ->
                % Assert node is not started
                false = lists:member(Node, Nodes0),
                {Entry, Log};
            error ->
                Entry = #log_start{
                    node = Node,
                    success = not lists:member(Node, Nodes0)
                },
                {Entry, Log0}
        end,

    Return =
        case LogEntry#log_start.success of
            true -> {ok, Node};
            false -> {error, {already_running, Node}}
        end,
    HistEntry = #hist_start{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack,
        node = LogEntry#log_start.node,
        success = LogEntry#log_start.success
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = cauder_syntax:replace_variable(Result#result.expr, VarNode, Return),
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    TraceAction = #trace_start{
        node = LogEntry#log_start.node,
        success = LogEntry#log_start.success
    },
    Nodes1 =
        case LogEntry#log_start.success of
            true -> [Node | Nodes0];
            false -> Nodes0
        end,
    Sys#system{
        pool = cauder_pool:update(P1, Pool),
        nodes = Nodes1,
        log = Log1,
        trace = cauder_trace:push(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_spawn(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_spawn(
    Pid,
    #result{label = Label} = Result,
    #system{pool = Pool, log = Log0} = Sys
) ->
    P0 = cauder_pool:get(Pid, Pool),

    {VarPid, Node} =
        case Label of
            #label_spawn_fun{var = Var, node = N} -> {Var, N};
            #label_spawn_mfa{var = Var, node = N} -> {Var, N}
        end,
    {LogEntry, Log1} =
        case cauder_log:pop_spawn(Pid, Log0) of
            {#log_spawn{} = Entry, Log} ->
                {Entry, Log};
            error ->
                {SpawnNode, Success} =
                    case Node of
                        undefined -> {P0#process.node, true};
                        _ -> {Node, lists:member(Node, Sys#system.nodes)}
                    end,
                Entry = #log_spawn{
                    node = SpawnNode,
                    pid = cauder_process:new_pid(),
                    success = Success
                },
                {Entry, Log0}
        end,

    HistEntry = #hist_spawn{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack,
        node = LogEntry#log_spawn.node,
        pid = LogEntry#log_spawn.pid
        %success = LogEntry#log_spawn.success
    },

    P1 = P0#process{
        env = Result#result.env,
        expr = cauder_syntax:replace_variable(Result#result.expr, VarPid, LogEntry#log_spawn.pid),
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    P2 =
        case Label of
            #label_spawn_fun{function = {value, Line, Fun} = FunLiteral} ->
                {env, [{{M, F}, _, _}]} = erlang:fun_info(Fun, env),
                {arity, A} = erlang:fun_info(Fun, arity),
                #process{
                    node = LogEntry#log_spawn.node,
                    pid = LogEntry#log_spawn.pid,
                    mfa = {M, F, A},
                    expr = [{apply_fun, Line, FunLiteral, []}]
                };
            #label_spawn_mfa{module = M, function = F, args = As} ->
                #process{
                    node = LogEntry#log_spawn.node,
                    pid = LogEntry#log_spawn.pid,
                    mfa = {M, F, length(As)},
                    expr = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))]
                }
        end,
    TraceAction = #trace_spawn{
        node = LogEntry#log_spawn.node,
        pid = LogEntry#log_spawn.pid,
        success = LogEntry#log_spawn.success
    },
    Pool1 =
        case LogEntry#log_spawn.success of
            true -> cauder_pool:add(P2, Pool);
            false -> Pool
        end,
    Sys#system{
        pool = cauder_pool:update(P1, Pool1),
        log = Log1,
        trace = cauder_trace:push(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_send(Pid, Result, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_send(
    Pid,
    #result{label = #label_send{dst = Dst, val = Val}} = Result,
    #system{pool = Pool, log = Log0} = Sys
) ->
    {Uid, Log1} =
        case cauder_log:pop_send(Pid, Log0) of
            {#log_send{} = Entry, Log} ->
                {Entry#log_send.uid, Log};
            error ->
                {cauder_message:new_uid(), Log0}
        end,

    M = #message{
        uid = Uid,
        src = Pid,
        dst = Dst,
        val = Val
    },

    P0 = cauder_pool:get(Pid, Pool),
    HistEntry = #hist_send{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack,
        msg = M
    },
    P1 = P0#process{
        env = Result#result.env,
        expr = Result#result.expr,
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    TraceAction = #trace_send{
        uid = Uid
    },
    Sys#system{
        mail = cauder_mailbox:add(M, Sys#system.mail),
        pool = cauder_pool:update(P1, Pool),
        log = Log1,
        trace = cauder_trace:push(Pid, TraceAction, Sys#system.trace)
    }.

-spec rule_receive(Pid, Result, Mode, Scheduler, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    Mode :: normal | replay,
    Scheduler :: cauder_message:scheduler(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_receive(
    Pid,
    #result{env = Bs, label = #label_receive{clauses = Cs}} = Result,
    Mode,
    Sched,
    #system{mail = Mail, pool = Pool, log = Log0} = Sys
) ->
    {{Es1, {Msg, QPos}, Mail1, Bs1}, Log1} =
        case Mode of
            normal ->
                Match = cauder_eval:match_receive_pid(Cs, Pid, Mail, Bs, Sched, Sys),
                {_, {#message{uid = Uid}, _}, _, _} = Match,
                %% If the chosen message is the same specified in the log don't invalidate the log
                Log =
                    case cauder_log:pop_receive(Pid, Log0) of
                        {#log_receive{uid = Uid}, NewLog} -> NewLog;
                        _ -> cauder_log:rdep(Pid, Log0)
                    end,
                {Match, Log};
            replay ->
                {#log_receive{uid = Uid}, Log} = cauder_log:pop_receive(Pid, Log0),
                Match = cauder_eval:match_receive_uid(Cs, Uid, Mail, Bs),
                {Match, Log}
        end,

    P0 = cauder_pool:get(Pid, Pool),
    HistEntry = #hist_receive{
        env = P0#process.env,
        expr = P0#process.expr,
        stack = P0#process.stack,
        msg = Msg,
        q_pos = QPos
    },
    P1 = P0#process{
        env = cauder_bindings:merge(Bs, Bs1),
        expr = Es1,
        stack = Result#result.stack,
        hist = cauder_history:push(HistEntry, P0#process.hist)
    },
    TraceAction = #trace_receive{
        uid = Msg#message.uid
    },
    Sys#system{
        mail = Mail1,
        pool = cauder_pool:update(P1, Pool),
        log = Log1,
        trace = cauder_trace:push(Pid, TraceAction, Sys#system.trace)
    }.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.

-spec expression_option(Expr, Pid, System, Mode) -> {ok, Rule} | 'false' when
    Expr :: [cauder_syntax:abstract_expr()],
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Mode :: normal | replay,
    Rule :: cauder_semantics:rule().

expression_option([E0 | Es0], Pid, Sys, Mode) ->
    Proc = cauder_pool:get(Pid, Sys#system.pool),
    case is_reducible(E0, Proc#process.env) of
        false ->
            case {Es0, cauder_stack:is_empty(Proc#process.stack)} of
                {[], true} ->
                    false;
                _ ->
                    {ok, ?RULE_LOCAL}
            end;
        true ->
            case E0 of
                {var, _, _} ->
                    {ok, ?RULE_LOCAL};
                {'if', _, _} ->
                    {ok, ?RULE_LOCAL};
                {'make_fun', _, _, _} ->
                    {ok, ?RULE_LOCAL};
                {self, _} ->
                    {ok, ?RULE_SELF};
                {node, _} ->
                    {ok, ?RULE_NODE};
                {tuple, _, Es} ->
                    expression_option(Es, Pid, Sys, Mode);
                {nodes, _} ->
                    check_reducibility([], Pid, Sys, Mode, nodes);
                {cons, _, H, T} ->
                    check_reducibility([H, T], Pid, Sys, Mode, cons);
                {'case', _, E, _} ->
                    check_reducibility([E], Pid, Sys, Mode, 'case');
                {'receive', _, Cs} ->
                    check_reducibility(Cs, Pid, Sys, Mode, 'receive');
                {bif, _, _, _, As} ->
                    check_reducibility(As, Pid, Sys, Mode, bif);
                {spawn, _, F} ->
                    check_reducibility([F], Pid, Sys, Mode, spawn);
                {spawn, _, N, F} ->
                    check_reducibility([N, F], Pid, Sys, Mode, spawn);
                {spawn, _, M, F, As} ->
                    check_reducibility([M, F, As], Pid, Sys, Mode, spawn);
                {spawn, _, N, M, F, As} ->
                    check_reducibility([N, M, F, As], Pid, Sys, Mode, spawn);
                {start, _, N} ->
                    check_reducibility([N], Pid, Sys, Mode, start);
                {start, _, H, N} ->
                    check_reducibility([H, N], Pid, Sys, Mode, start);
                {send, _, L, R} ->
                    check_reducibility([L, R], Pid, Sys, Mode, send);
                {send_op, _, L, R} ->
                    check_reducibility([L, R], Pid, Sys, Mode, send);
                {local_call, _, _, As} ->
                    check_reducibility(As, Pid, Sys, Mode, local_call);
                {remote_call, _, _, _, As} ->
                    check_reducibility(As, Pid, Sys, Mode, remote_call);
                {apply, _, M, F, As} ->
                    check_reducibility([M, F, As], Pid, Sys, Mode, apply);
                {apply_fun, _, Fun, As} ->
                    check_reducibility([Fun, As], Pid, Sys, Mode, apply_fun);
                {match, _, Pat, E} ->
                    check_reducibility([Pat, E], Pid, Sys, Mode, match);
                {op, _, _, Es} ->
                    check_reducibility(Es, Pid, Sys, Mode, op);
                {'andalso', _, L, R} ->
                    check_reducibility([L, R], Pid, Sys, Mode, 'andalso');
                {'orelse', _, L, R} ->
                    check_reducibility([L, R], Pid, Sys, Mode, 'orelse')
            end
    end.

check_reducibility([], _, _, _, send) ->
    {ok, ?RULE_SEND};
check_reducibility([], _, _, _, send_op) ->
    {ok, ?RULE_SEND};
check_reducibility([], _, _, _, local_call) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, remote_call) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, apply) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, apply_fun) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, match) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, op) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, 'andalso') ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, 'orelse') ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, 'case') ->
    {ok, ?RULE_LOCAL};
check_reducibility([], _, _, _, bif) ->
    {ok, ?RULE_LOCAL};
check_reducibility([], Pid, Sys, _, nodes) ->
    P = cauder_pool:get(Pid, Sys#system.pool),
    OtherNodes = lists:delete(P#process.node, Sys#system.nodes),
    case cauder_log:peek(Pid, Sys#system.log) of
        {value, #log_nodes{nodes = Nodes}} when Nodes =/= OtherNodes ->
            false;
        _ ->
            {ok, ?RULE_NODES}
    end;
check_reducibility(Cs, Pid, #system{mail = Mail, log = Log} = Sys, Mode, 'receive') ->
    #process{env = Bs} = cauder_pool:get(Pid, Sys#system.pool),
    IsMatch =
        case Mode of
            normal ->
                cauder_eval:match_receive_pid(Cs, Pid, Mail, Bs, ?SCHEDULER_Random, Sys) =/= nomatch;
            replay ->
                {value, #log_receive{uid = Uid}} = cauder_log:peek(Pid, Log),
                cauder_eval:match_receive_uid(Cs, Uid, Mail, Bs) =/= nomatch
        end,
    case IsMatch of
        true ->
            {ok, ?RULE_RECEIVE};
        false ->
            false
    end;
check_reducibility([], Pid, #system{log = Log, nodes = Nodes}, _, spawn) ->
    case cauder_log:peek(Pid, Log) of
        {value, #log_spawn{node = Node, success = true}} ->
            case lists:member(Node, Nodes) of
                false ->
                    false;
                true ->
                    {ok, ?RULE_SPAWN}
            end;
        _ ->
            {ok, ?RULE_SPAWN}
    end;
check_reducibility([], Pid, #system{log = Log, nodes = Nodes}, _, start) ->
    case cauder_log:peek(Pid, Log) of
        {value, #log_start{node = Node, success = Success}} ->
            case cauder_log:find_failed_spawns(Node, Log) of
                [_ | _] when Success ->
                    false;
                [] ->
                    case cauder_log:find_nodes(Node, Log) of
                        [_ | _] when Success ->
                            false;
                        [] ->
                            case lists:member(Node, Nodes) of
                                false when not Success ->
                                    false;
                                true ->
                                    {ok, ?RULE_START}
                            end
                    end
            end;
        _ ->
            {ok, ?RULE_START}
    end;
check_reducibility([H | T], Pid, Sys, Mode, ExprType) ->
    #process{env = Bs} = cauder_pool:get(Pid, Sys#system.pool),
    case is_reducible(H, Bs) of
        true ->
            expression_option([H], Pid, Sys, Mode);
        false ->
            check_reducibility(T, Pid, Sys, Mode, ExprType)
    end.
