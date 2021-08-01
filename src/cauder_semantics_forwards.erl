%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([step/2, step/4, options/2]).

%%-ignore_xref([rule_deliver/2]).

%%-ifdef(EUNIT).
%%-export([rdep/2]).
%%-endif.

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

step(Sys, Pid) -> step(Sys, Pid, ?SCHEDULER_Random, normal).

%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid, MessageScheduler, Mode) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    MessageScheduler :: cauder_types:message_scheduler(),
    Mode :: normal | replay,
    NewSystem :: cauder_types:system().

step(Sys0, Pid, Sched, Mode) ->
    #proc{node = Node, pid = Pid, stack = Stk0, env = Bs0, exprs = Es0} = P0 = maps:get(Pid, Sys0#sys.procs),
    Result = cauder_eval:seq(Bs0, Es0, Stk0),
    case Result#result.label of
        tau ->
            rule_local(Sys0, P0, Result);
        {send, Dest, _Val} ->
            Sys1 = rule_send(Sys0, P0, Result),
            % Instant-deliver
            rule_deliver(Sys1, maps:get(Dest, Sys1#sys.procs));
        {rec, VarBody, _Cs} when Result#result.exprs =:= [VarBody] ->
            rule_receive(Sys0, P0, Result, Sched, Mode);
        {self, _TmpVar} ->
            rule_self(Sys0, P0, Result);
        {node, _TmpVar} ->
            rule_node(Sys0, P0, Result);
        {nodes, _TmpVar} ->
            rule_nodes(Sys0, P0, Result);
        {spawn, TmpVar, AbsFun} ->
            rule_spawn(Sys0, P0, Result#result{label = {spawn, TmpVar, Node, AbsFun}});
        {spawn, _TmpVar, _N, _AbsFun} ->
            rule_spawn(Sys0, P0, Result);
        {spawn, TmpVar, M, F, As} ->
            rule_spawn(Sys0, P0, Result#result{label = {spawn, TmpVar, Node, M, F, As}});
        {spawn, _TmpVar, _N, _M, _F, _As} ->
            rule_spawn(Sys0, P0, Result);
        % FIXME Shouldn't this be the host instead of the name??
        {start, TmpVar, NewName} ->
            case Node of
                'nonode@nohost' ->
                    error(no_alive);
                _ ->
                    [$@ | Host] = lists:dropwhile(fun(C) -> C =/= $@ end, atom_to_list(Node)),
                    rule_start(Sys0, P0, Result#result{label = {start, TmpVar, list_to_atom(Host), NewName}})
            end;
        {start, _TmpVar, _Host, _Name} ->
            case Node of
                'nonode@nohost' -> error(no_alive);
                _ -> rule_start(Sys0, P0, Result)
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System, Mode) -> Options when
    System :: cauder_types:system(),
    Mode :: normal | replay,
    Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys, Mode) ->
    lists:foldl(
        fun(#proc{exprs = E, pid = Pid} = P, Opts) ->
            case expression_option(E, P, Mode, Sys) of
                ?NOT_EXP ->
                    Opts;
                Rule ->
                    Opt = #opt{
                        sem = ?MODULE,
                        pid = Pid,
                        rule = Rule
                    },
                    [Opt | Opts]
            end
        end,
        [],
        maps:values(PMap)
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.
%%

-spec expression_option(Expr, Proc, Mode, Sys) -> Rule when
    Expr :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
    Proc :: cauder_types:proc(),
    Sys :: cauder_types:system(),
    Mode :: normal | replay,
    Rule :: cauder_types:rule() | ?NOT_EXP.

expression_option(E, P, Mode, Sys) when not is_list(E) ->
    expression_option([E], P, Mode, Sys);
expression_option([E0 | Es0], #proc{env = Bs, stack = Stk} = Proc, Mode, Sys) ->
    case is_reducible(E0, Bs) of
        false ->
            case {Es0, Stk} of
                {[], []} -> ?NOT_EXP;
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
check_reducibility([], #proc{node = Node, pid = Pid}, _, #sys{log = Log, nodes = Nodes}, nodes) ->
    SNodes = Nodes -- [Node],
    case log_consume_nodes(Pid, Log) of
        {'_', Log} -> ?RULE_NODES;
        {SNodes, _} -> ?RULE_NODES;
        _ -> ?NOT_EXP
    end;
check_reducibility([Cs], #proc{pid = Pid, env = Bs, mail = LocalMail}, _, #sys{log = Log}, {'receive', Mode}) ->
    IsMatch =
        case Mode of
            normal ->
                case log_consume_receive(Pid, Log) of
                    {'_', Log} ->
                        cauder_eval:matchrec(Cs, Bs, LocalMail) =/= nomatch;
                    {Uid, _} ->
                        cauder_eval:matchrec_uid(Cs, Bs, Uid, LocalMail) =/= nomatch
                end;
            replay ->
                case log_consume_receive(Pid, Log) of
                    {'_', Log} ->
                        false;
                    {Uid, _} ->
                        case cauder_eval:matchrec(Cs, Bs, LocalMail) of
                            nomatch -> false;
                            {_, _, #message{uid = Uid}, _, _} -> true
                        end
                end
        end,
    case IsMatch of
        true -> ?RULE_RECEIVE;
        false -> ?NOT_EXP
    end;
check_reducibility([], #proc{pid = Pid}, _, #sys{log = Log, nodes = Nodes}, spawn) ->
    case log_consume_spawn(Pid, Log) of
        {{{'_', _}, _}, Log} ->
            ?RULE_START;
        {{{SpawnNode, _SpawnPid}, Result}, _} ->
            case {Result, lists:member(SpawnNode, Nodes)} of
                {success, false} -> ?NOT_EXP;
                {_, _} -> ?RULE_START
            end
    end;
check_reducibility([], #proc{pid = Pid}, _, #sys{log = Log, nodes = Nodes}, start) ->
    case log_consume_start(Pid, Log) of
        {{'_', _}, Log} ->
            ?RULE_START;
        {{Node, Result}, _} ->
            FailedSpawnsExist = cauder_utils:find_process_with_failed_spawn(Log, Node),
            FutureReads = cauder_utils:find_process_with_future_reads(Log, Node),
            NodeAlreadyExists = lists:member(Node, Nodes),
            case {Result, FailedSpawnsExist, NodeAlreadyExists, FutureReads} of
                %at least one spawn has still to fail
                {success, {value, _}, _, _} -> ?NOT_EXP;
                {failure, _, false, _} -> ?NOT_EXP;
                {success, _, _, {value, _}} -> ?NOT_EXP;
                _ -> ?RULE_START
            end
    end;
check_reducibility([H | T], #proc{env = Bs} = P, Mode, Sys, ExprType) ->
    case is_reducible(H, Bs) of
        true -> expression_option(H, P, Mode, Sys);
        false -> check_reducibility(T, P, Mode, Sys, ExprType)
    end.

% TODO Rename to log_take_xxx and return 'error' instead on {'_', Log}

-spec log_consume_send(Pid, Log) -> {Uid, NewLog} when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    Uid :: cauder_mailbox:uid(),
    NewLog :: cauder_types:log().

log_consume_send(Pid, Log) ->
    case maps:get(Pid, Log, []) of
        [{send, Uid} | RestLog] ->
            {Uid, Log#{Pid => RestLog}};
        [] ->
            {cauder_mailbox:uid(), Log}
    end.

-spec log_consume_receive(Pid, Log) -> {Uid, NewLog} | {'_', Log} when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    Uid :: cauder_mailbox:uid(),
    NewLog :: cauder_types:log().

log_consume_receive(Pid, Log) ->
    case maps:get(Pid, Log, []) of
        [{'receive', Uid} | RestLog] ->
            {Uid, Log#{Pid => RestLog}};
        [] ->
            {'_', Log}
    end.

-spec log_consume_nodes(Pid, Log) -> {[Node], NewLog} | {'_', Log} when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    Node :: node(),
    NewLog :: cauder_types:log().

log_consume_nodes(Pid, Log) ->
    case maps:get(Pid, Log, []) of
        [{'nodes', Nodes} | RestLog] ->
            {Nodes, Log#{Pid => RestLog}};
        [] ->
            {'_', Log}
    end.

-spec log_consume_start(Pid, Log) -> {{Node, Result}, NewLog} | {{'_', success}, Log} when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    Node :: node(),
    Result :: success | failure,
    NewLog :: cauder_types:log().

log_consume_start(Pid, Log) ->
    case maps:get(Pid, Log, []) of
        [{start, Node, Result} | RestLog] ->
            {{Node, Result}, Log#{Pid => RestLog}};
        [] ->
            {{'_', success}, Log}
    end.

-spec log_consume_spawn(Pid, Log) -> {{{SpawnNode, SpawnPid}, Result}, NewLog} | {{{'_', SpawnPid}, success}, Log} when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    SpawnNode :: node(),
    SpawnPid :: cauder_types:proc_id(),
    Result :: success | failure,
    NewLog :: cauder_types:log().

log_consume_spawn(Pid, Log) ->
    case maps:get(Pid, Log, []) of
        [{spawn, {SpawnNode, SpawnPid}, Result} | RestLog] ->
            {{{SpawnNode, SpawnPid}, Result}, Log#{Pid => RestLog}};
        [] ->
            {{{'_', cauder_utils:fresh_pid()}, success}, Log}
    end.

-spec admissible(LogActions, LocalMail) -> Uid | false when
    LogActions :: [cauder_types:log_action()],
    LocalMail :: queue:queue(cauder_mailbox:message()),
    Uid :: cauder_mailbox:uid().

admissible(Actions, LocalMail) ->
    DeliveredUids = lists:foldl(
        fun(#message{uid = Uid}, Set) -> sets:add_element(Uid, Set) end,
        sets:new(),
        queue:to_list(LocalMail)
    ),
    case lists:search(fun({'receive', Uid}) -> not sets:is_element(Uid, DeliveredUids) end, Actions) of
        {value, {'receive', NextReceiveUid}} -> NextReceiveUid;
        _ -> false
    end.

-spec rule_local(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_local(
    #sys{procs = PMap} = Sys,
    #proc{pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1}
) ->
    P1 = P0#proc{
        hist = [{tau, Bs0, Es0, Stk0} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = Es1
    },
    Sys#sys{
        procs = PMap#{Pid := P1}
    }.

-spec rule_send(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_send(
    #sys{mail = Mail0, log = Log0, procs = PMap, x_trace = XTrace} = Sys,
    #proc{pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = {send, Dest, Val}}
) ->
    {Uid, Log1} = log_consume_send(Pid, Log0),

    M = #message{
        uid = Uid,
        src = Pid,
        dest = Dest,
        value = Val
    },
    P1 = P0#proc{
        hist = [{send, Bs0, Es0, Stk0, M} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = Es1
    },
    T = #x_trace{
        type = ?RULE_SEND,
        from = Pid,
        to = Dest,
        val = Val,
        time = M#message.uid
    },
    Sys#sys{
        mail = cauder_mailbox:add(M, Mail0),
        procs = PMap#{Pid := P1},
        log = Log1,
        x_trace = [T | XTrace]
    }.

-spec rule_deliver(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_deliver(
    #sys{mail = Mail0, log = Log, procs = PMap} = Sys,
    #proc{pid = Pid, mail = LocalMail} = P0
) ->
    Delivered =
        case maps:get(Pid, Log) of
            [] ->
                cauder_mailbox:pid_take(Pid, Mail0);
            Actions ->
                case admissible(Actions, LocalMail) of
                    false -> false;
                    Uid -> cauder_mailbox:uid_take(Uid, Mail0)
                end
        end,
    case Delivered of
        false ->
            Sys;
        {Message, Mail1} ->
            P1 = P0#proc{
                mail = queue:in(Message, LocalMail)
            },
            Sys#sys{
                mail = Mail1,
                procs = PMap#{Pid := P1}
            }
    end.

-spec rule_receive(System, Process, Result, Scheduler, Mode) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Scheduler :: cauder_types:message_scheduler(),
    Mode :: normal | replay,
    NewSystem :: cauder_types:system().

rule_receive(
    #sys{log = Log0} = Sys0,
    #proc{pid = Pid, mail = LocalMail0},
    #result{env = Bs1, stack = Stk1, exprs = [TmpVar], label = {rec, TmpVar, Cs}},
    Sched,
    Mode
) ->
    {{Es2, Bs2, Msg, LocalMail1, QPos}, #sys{procs = PMap, x_trace = XTrace} = Sys1} =
        case Mode of
            normal ->
                case log_consume_receive(Pid, Log0) of
                    {'_', Log0} ->
                        case cauder_eval:matchrec(Cs, Bs1, LocalMail0) of
                            nomatch -> throw(nomatch);
                            Match -> {Match, Sys0}
                        end;
                    {Uid, NewLog} ->
                        case cauder_eval:matchrec_race(Cs, Bs1, Uid, Pid, Sys0#sys{log = NewLog}) of
                            nomatch -> throw(nomatch);
                            {Match, NewSys} -> {Match, NewSys}
                        end
                end;
            replay ->
                case log_consume_receive(Pid, Log0) of
                    % Should not happen
                    {'_', Log0} ->
                        error(badlog);
                    % Deliver until message in log is available to be received
                    {Uid, NewLog} ->
                        case cauder_eval:matchrec(Cs, Bs1, LocalMail0) of
                            nomatch -> throw(nomatch);
                            {_, _, #message{uid = Uid}, _, _} = Match -> {Match, Sys0#sys{log = NewLog}}
                        end
                end
        end,

    #proc{hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0 = maps:get(Pid, Sys1#sys.procs),

    P1 = P0#proc{
        hist = [{rec, Bs0, Es0, Stk0, Msg, QPos} | Hist0],
        stack = Stk1,
        env = cauder_utils:merge_bindings(Bs1, Bs2),
        exprs = Es2,
        mail = LocalMail1
    },
    T = #x_trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val = Msg#message.value,
        time = Msg#message.uid
    },
    Sys1#sys{
        procs = PMap#{Pid := P1},
        x_trace = [T | XTrace]
    }.

-spec rule_self(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_self(
    #sys{procs = PMap} = Sys,
    #proc{pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = {self, VarPid}}
) ->
    P1 = P0#proc{
        hist = [{self, Bs0, Es0, Stk0} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = cauder_syntax:replace_variable(Es1, VarPid, Pid)
    },
    Sys#sys{
        procs = PMap#{Pid := P1}
    }.

-spec rule_node(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_node(
    #sys{procs = PMap} = Sys,
    #proc{node = Node, pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = {node, VarNode}}
) ->
    P0 = P0#proc{
        hist = [{node, Bs0, Es0, Stk0} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = cauder_syntax:replace_variable(Es1, VarNode, Node)
    },
    Sys#sys{
        procs = PMap#{Pid => P0}
    }.

-spec rule_nodes(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_nodes(
    #sys{nodes = Nodes, log = Log0, procs = PMap} = Sys,
    #proc{node = Node, pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = {nodes, VarNodes}}
) ->
    {Nodes, Log1} =
        case log_consume_nodes(Pid, Log0) of
            {'_', Log0} -> {Nodes, Log0};
            {Nodes, NewLog} -> {Nodes, NewLog}
        end,

    OtherNodes = Nodes -- [Node],

    P1 = P0#proc{
        hist = [{nodes, Bs0, Es0, Stk0, OtherNodes} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = cauder_syntax:replace_variable(Es1, VarNodes, OtherNodes)
    },
    Sys#sys{
        procs = PMap#{Pid := P1},
        log = Log1
    }.

-spec rule_spawn(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_spawn(
    #sys{procs = PMap0, log = Log0, nodes = Nodes, x_trace = XTrace} = Sys,
    #proc{pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = Label}
) ->
    VarPid = element(2, Label),
    Node = element(3, Label),

    {{{SpawnNode, SpawnPid}, SpawnResult}, Log1} =
        case log_consume_spawn(Pid, Log0) of
            {{{'_', SPid}, success}, Log0} ->
                SResult =
                    case lists:member(Node, Nodes) of
                        true -> success;
                        false -> failure
                    end,
                {{{Node, SPid}, SResult}, Log0};
            % TODO SNode =:= Node ??
            {{{SNode, SPid}, SResult}, NewLog} ->
                % Assert node is started
                true = lists:member(SNode, Nodes),
                {{{SNode, SPid}, SResult}, NewLog}
        end,

    P1 = P0#proc{
        hist = [{spawn, Bs0, Es0, Stk0, SpawnNode, SpawnPid} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = cauder_syntax:replace_variable(Es1, VarPid, SpawnPid)
    },
    P2 =
        case Label of
            {spawn, VarPid, Node, {value, Line, Fun} = FunLiteral} ->
                {env, [{{M, F}, _, _}]} = erlang:fun_info(Fun, env),
                {arity, A} = erlang:fun_info(Fun, arity),
                #proc{
                    node = SpawnNode,
                    pid = SpawnPid,
                    exprs = [{apply_fun, Line, FunLiteral, []}],
                    entry_point = {M, F, A}
                };
            {spawn, VarPid, Node, M, F, As} ->
                #proc{
                    node = SpawnNode,
                    pid = SpawnPid,
                    exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
                    entry_point = {M, F, length(As)}
                }
        end,
    PMap1 =
        case SpawnResult of
            success -> PMap0#{Pid := P1, SpawnPid => P2};
            failure -> PMap0#{Pid := P1}
        end,
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = SpawnPid
    },
    Sys#sys{
        procs = PMap1,
        log = Log1,
        x_trace = [T | XTrace]
    }.

-spec rule_start(System, Process, Result) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Result :: cauder_types:result(),
    NewSystem :: cauder_types:system().

rule_start(
    #sys{nodes = Nodes, log = Log0, procs = PMap, x_trace = XTrace} = Sys,
    #proc{pid = Pid, hist = Hist0, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs1, exprs = Es1, stack = Stk1, label = {start, VarNode, Host, Name}}
) ->
    Node = list_to_atom(atom_to_list(Name) ++ "@" ++ atom_to_list(Host)),

    {{StartNode, StartResult}, Log1} =
        case log_consume_start(Pid, Log0) of
            {{'_', success}, Log0} ->
                Result =
                    case lists:member(Node, Nodes) of
                        true -> failure;
                        false -> success
                    end,
                {{Node, Result}, Log0};
            {{Node, Result}, NewLog} ->
                % Assert node is not started
                false = lists:member(Node, Nodes),
                {{Node, Result}, NewLog}
        end,
    Return =
        case StartResult of
            success -> {ok, StartNode};
            failure -> {error, {already_running, StartNode}}
        end,
    P1 = P0#proc{
        hist = [{start, StartResult, Bs0, Es0, Stk0, StartNode} | Hist0],
        stack = Stk1,
        env = Bs1,
        exprs = cauder_syntax:replace_variable(Es1, VarNode, Return)
    },
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = StartResult,
        node = StartNode
    },
    Sys#sys{
        procs = PMap#{Pid := P1},
        nodes = [StartNode | Nodes],
        log = Log1,
        x_trace = [T | XTrace]
    }.
