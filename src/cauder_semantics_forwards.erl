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

-include("cauder.hrl").
-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_history.hrl").
-include("cauder_eval.hrl").
-include("cauder_log.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(Pid, System, MessageScheduler, Mode) -> NewSystem when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    MessageScheduler :: cauder_types:message_scheduler(),
    Mode :: normal | replay,
    NewSystem :: cauder_system:system().

step(Pid, Sys, Sched, Mode) ->
    #process{stack = Stk0, env = Bs0, expr = Es0} = cauder_pool:get(Pid, Sys#system.pool),
    Result = cauder_eval:seq(Bs0, Es0, Stk0),
    case Result#result.label of
        #label_tau{} ->
            rule_local(Pid, Result, Sys);
        #label_send{} ->
            rule_send(Pid, Result, Sys);
        #label_receive{var = Var} when Result#result.expr == [Var] ->
            rule_receive(Pid, Result, Mode, Sched, Sys);
        #label_self{} ->
            rule_self(Pid, Result, Sys);
        #label_node{} ->
            rule_node(Pid, Result, Sys);
        #label_nodes{} ->
            rule_nodes(Pid, Result, Sys);
        #label_spawn_fun{} ->
            rule_spawn(Pid, Result, Sys);
        #label_spawn_mfa{} ->
            rule_spawn(Pid, Result, Sys);
        #label_start{} ->
            rule_start(Pid, Result, Sys)
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System, Mode) -> Options when
    System :: cauder_system:system(),
    Mode :: normal | replay,
    Options :: [cauder_types:option()].

options(#system{pool = Pool} = Sys, Mode) ->
    lists:filtermap(
        fun(#process{expr = E, pid = Pid} = P) ->
            case expression_option(E, P, Mode, Sys) of
                ?NOT_EXP ->
                    false;
                Rule ->
                    Opt = #opt{
                        sem = ?MODULE,
                        pid = Pid,
                        rule = Rule
                    },
                    {true, Opt}
            end
        end,
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
    T = #x_trace{
        type = ?RULE_SEND,
        from = Pid,
        to = Dst,
        val = Val,
        time = Uid
    },
    Sys#system{
        mail = cauder_mailbox:add(M, Sys#system.mail),
        pool = cauder_pool:update(P1, Pool),
        log = Log1,
        x_trace = [T | Sys#system.x_trace]
    }.

-spec rule_receive(Pid, Result, Mode, Scheduler, System) -> NewSystem when
    Pid :: cauder_process:id(),
    Result :: cauder_eval:result(),
    Mode :: normal | replay,
    Scheduler :: cauder_types:message_scheduler(),
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

rule_receive(
    Pid,
    #result{env = Bs, label = #label_receive{clauses = Cs}} = Result,
    Mode,
    Sched,
    #system{mail = Mail, pool = Pool, log = Log0} = Sys
) ->
    {{Bs1, Es1, {Msg, QPos}, Mail1}, Log1} =
        case Mode of
            normal ->
                Match = cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, Sched, Sys),
                {_, _, {#message{uid = Uid}, _}, _} = Match,
                %% If the chosen message is the same specified in the log don't invalidate the log
                Log =
                    case cauder_log:pop_receive(Pid, Log0) of
                        {#log_receive{uid = Uid}, NewLog} -> NewLog;
                        _ -> cauder_log:rdep(Pid, Log0)
                    end,
                {Match, Log};
            replay ->
                {#log_receive{uid = Uid}, Log} = cauder_log:pop_receive(Pid, Log0),
                Match = cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail),
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
    T = #x_trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val = Msg#message.val,
        time = Msg#message.uid
    },
    Sys#system{
        mail = Mail1,
        pool = cauder_pool:update(P1, Pool),
        log = Log1,
        x_trace = [T | Sys#system.x_trace]
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
    Sys#system{
        pool = cauder_pool:update(P1, Pool),
        log = Log1
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
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = LogEntry#log_spawn.pid
    },
    Pool1 =
        case LogEntry#log_spawn.success of
            true -> cauder_pool:add(P2, Pool);
            false -> Pool
        end,
    Sys#system{
        pool = cauder_pool:update(P1, Pool1),
        log = Log1,
        x_trace = [T | Sys#system.x_trace]
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
    Res =
        case LogEntry#log_start.success of
            true -> success;
            false -> failure
        end,
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = Res,
        node = Node
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
        x_trace = [T | Sys#system.x_trace]
    }.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.
%%

-spec expression_option(Expr, Proc, Mode, System) -> Rule when
    Expr :: cauder_syntax:abstract_expr() | [cauder_syntax:abstract_expr()],
    Proc :: cauder_process:proc(),
    System :: cauder_system:system(),
    Mode :: normal | replay,
    Rule :: cauder_types:rule() | ?NOT_EXP.

expression_option(E, P, Mode, Sys) when not is_list(E) ->
    expression_option([E], P, Mode, Sys);
expression_option([E0 | Es0], #process{env = Bs, stack = Stk} = Proc, Mode, Sys) ->
    case is_reducible(E0, Bs) of
        false ->
            case {Es0, cauder_stack:is_empty(Stk)} of
                {[], true} -> ?NOT_EXP;
                _ -> ?RULE_SEQ
            end;
        true ->
            case E0 of
                {var, _, _} -> ?RULE_SEQ;
                {'if', _, _} -> ?RULE_SEQ;
                {'make_fun', _, _, _} -> ?RULE_SEQ;
                {self, _} -> ?RULE_SELF;
                {node, _} -> ?RULE_NODE;
                {tuple, _, Es} -> expression_option(Es, Proc, Mode, Sys);
                {nodes, _} -> check_reducibility([], Proc, Mode, Sys, nodes);
                {cons, _, H, T} -> check_reducibility([H, T], Proc, Mode, Sys, cons);
                {'case', _, E, _} -> check_reducibility([E], Proc, Mode, Sys, 'case');
                {'receive', _, Cs} -> check_reducibility([Cs], Proc, Mode, Sys, {'receive', Mode});
                {bif, _, _, _, As} -> check_reducibility([As], Proc, Mode, Sys, bif);
                {spawn, _, F} -> check_reducibility([F], Proc, Mode, Sys, spawn);
                {spawn, _, N, F} -> check_reducibility([N, F], Proc, Mode, Sys, spawn);
                {spawn, _, M, F, As} -> check_reducibility([M, F, As], Proc, Mode, Sys, spawn);
                {spawn, _, N, M, F, As} -> check_reducibility([N, M, F, As], Proc, Mode, Sys, spawn);
                {start, _, N} -> check_reducibility([N], Proc, Mode, Sys, start);
                {start, _, H, N} -> check_reducibility([H, N], Proc, Mode, Sys, start);
                {send, _, L, R} -> check_reducibility([L, R], Proc, Mode, Sys, send);
                {send_op, _, L, R} -> check_reducibility([L, R], Proc, Mode, Sys, send);
                {local_call, _, _, As} -> check_reducibility([As], Proc, Mode, Sys, local_call);
                {remote_call, _, _, _, As} -> check_reducibility([As], Proc, Mode, Sys, remote_call);
                {apply, _, M, F, As} -> check_reducibility([M, F, As], Proc, Mode, Sys, apply);
                {apply_fun, _, Fun, As} -> check_reducibility([Fun, As], Proc, Mode, Sys, apply_fun);
                {match, _, P, E} -> check_reducibility([P, E], Proc, Mode, Sys, match);
                {op, _, _, Es} -> check_reducibility([Es], Proc, Mode, Sys, op);
                {'andalso', _, L, R} -> check_reducibility([L, R], Proc, Mode, Sys, 'andalso');
                {'orelse', _, L, R} -> check_reducibility([L, R], Proc, Mode, Sys, 'orelse')
            end
    end.

check_reducibility([], _, _, _, send) ->
    ?RULE_SEND;
check_reducibility([], _, _, _, send_op) ->
    ?RULE_SEND;
check_reducibility([], _, _, _, local_call) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, remote_call) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, apply) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, apply_fun) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, match) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, op) ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, 'andalso') ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, 'orelse') ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, 'case') ->
    ?RULE_SEQ;
check_reducibility([], _, _, _, bif) ->
    ?RULE_SEQ;
check_reducibility([], #process{pid = Pid} = P, _, #system{log = Log} = Sys, nodes) ->
    OtherNodes = lists:delete(P#process.node, Sys#system.nodes),
    case cauder_log:peek(Pid, Log) of
        {value, #log_nodes{nodes = Nodes}} when Nodes =/= OtherNodes ->
            ?NOT_EXP;
        _ ->
            ?RULE_NODES
    end;
check_reducibility(
    [Cs | []],
    #process{pid = Pid, env = Bs},
    _,
    #system{mail = Mail, log = Log} = Sys,
    {'receive', Mode}
) ->
    IsMatch =
        case Mode of
            normal ->
                cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, ?SCHEDULER_Random, Sys) =/= nomatch;
            replay ->
                {value, #log_receive{uid = Uid}} = cauder_log:peek(Pid, Log),
                cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
        end,
    case IsMatch of
        true -> ?RULE_RECEIVE;
        false -> ?NOT_EXP
    end;
check_reducibility([], #process{pid = Pid}, _, #system{log = Log, nodes = Nodes}, spawn) ->
    case cauder_log:peek(Pid, Log) of
        {value, #log_spawn{node = Node, success = true}} ->
            case lists:member(Node, Nodes) of
                false -> ?NOT_EXP;
                true -> ?RULE_SPAWN
            end;
        _ ->
            ?RULE_SPAWN
    end;
check_reducibility([], #process{pid = Pid}, _, #system{log = Log, nodes = Nodes}, start) ->
    case cauder_log:peek(Pid, Log) of
        {value, #log_start{node = Node, success = Success}} ->
            case cauder_log:find_failed_spawns(Node, Log) of
                [_ | _] when Success ->
                    ?NOT_EXP;
                [] ->
                    case cauder_log:find_nodes(Node, Log) of
                        [_ | _] when Success ->
                            ?NOT_EXP;
                        [] ->
                            case lists:member(Node, Nodes) of
                                false when not Success ->
                                    ?NOT_EXP;
                                true ->
                                    ?RULE_START
                            end
                    end
            end;
        _ ->
            ?RULE_START
    end;
check_reducibility([H | T], #process{env = Bs} = P, Mode, Sys, ExprType) ->
    case is_reducible(H, Bs) of
        true -> expression_option(H, P, Mode, Sys);
        false -> check_reducibility(T, P, Mode, Sys, ExprType)
    end.
