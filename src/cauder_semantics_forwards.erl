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

-ifdef(EUNIT).
-export([rdep/2]).
-endif.

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid, MessageScheduler, Mode) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    MessageScheduler :: cauder_types:message_scheduler(),
    Mode :: normal | replay,
    NewSystem :: cauder_types:system().

step(Sys, Pid, Sched, Mode) ->
    {#proc{node = Node, pid = Pid, stack = Stk0, env = Bs0, exprs = Es0} = P0, _} = maps:take(Pid, Sys#sys.procs),
    #result{label = Label, exprs = Es} = Result = cauder_eval:seq(Bs0, Es0, Stk0),
    #sys{nodes = Nodes, traces = LMap} = Sys,
    case Label of
        tau ->
            fwd_tau(P0, Result, Sys);
        {self, _VarPid} ->
            fwd_self(P0, Result, Sys);
        {node, _VarNode} ->
            fwd_node(P0, Result, Sys);
        {nodes, _VarNodes} ->
            case extract_log(LMap, Pid, nodes) of
                {found, {nodes, _}, NewLog} -> fwd_nodes(P0, Result, Sys, #{new_log => NewLog});
                not_found -> fwd_nodes(P0, Result, Sys, #{})
            end;
        {spawn, VarPid, {value, _Line, Fun} = FunLiteral} ->
            {env, [{{M, F}, _, _}]} = erlang:fun_info(Fun, env),
            CLabel = {spawn, VarPid, Node, M, F, []},
            case extract_log(LMap, Pid, spawn) of
                {found, {spawn, _N, success, NewPid}, NewLog} ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{
                        pid => NewPid,
                        new_log => NewLog,
                        inline => true,
                        fun_literal => FunLiteral
                    });
                not_found ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{inline => true, fun_literal => FunLiteral})
            end;
        {spawn, VarPid, N, {value, _Line, Fun} = FunLiteral} ->
            {env, [{{M, F}, _, _}]} = erlang:fun_info(Fun, env),
            CLabel = {spawn, VarPid, N, M, F, []},
            case {extract_log(LMap, Pid, spawn), Node, lists:member(N, Nodes)} of
                {{found, {spawn, _N, success, NewPid}, NewLog}, _, _} ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{
                        pid => NewPid,
                        new_log => NewLog,
                        inline => true,
                        fun_literal => FunLiteral
                    });
                {{found, {spawn, _N, failure, NewPid}, NewLog}, _, _} ->
                    fwd_spawn_f(P0, Result#result{label = CLabel}, Sys, #{
                        pid => NewPid,
                        new_log => NewLog,
                        inline => true,
                        fun_literal => FunLiteral
                    });
                {_, 'nonode@nohost', _} ->
                    fwd_spawn_f(P0, Result#result{label = CLabel}, Sys, #{});
                {_, _, true} ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{inline => true, fun_literal => FunLiteral});
                {_, _, false} ->
                    fwd_spawn_f(P0, Result#result{label = CLabel}, Sys, #{})
            end;
        {spawn, VarPid, M, F, As} ->
            CLabel = {spawn, VarPid, Node, M, F, As},
            case extract_log(LMap, Pid, spawn) of
                {found, {spawn, _N, success, NewPid}, NewLog} ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{pid => NewPid, new_log => NewLog});
                {found, {spawn, _N, failure, NewPid}, NewLog} ->
                    fwd_spawn_f(P0, Result#result{label = CLabel}, Sys, #{pid => NewPid, new_log => NewLog});
                not_found ->
                    fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{})
            end;
        {spawn, _VarPid, N, _M, _F, _As} ->
            case {extract_log(LMap, Pid, spawn), Node, lists:member(N, Nodes)} of
                {{found, {spawn, _N, failure, NewPid}, NewLog}, _, _} ->
                    fwd_spawn_f(P0, Result, Sys, #{pid => NewPid, new_log => NewLog});
                {{found, {spawn, _N, success, NewPid}, NewLog}, _, _} ->
                    fwd_spawn_s(P0, Result, Sys, #{pid => NewPid, new_log => NewLog});
                {_, 'nonode@nohost', _} ->
                    fwd_spawn_f(P0, Result, Sys, #{});
                {_, _, false} ->
                    fwd_spawn_f(P0, Result, Sys, #{});
                {_, _, true} ->
                    fwd_spawn_s(P0, Result, Sys, #{})
            end;
        {start, VarNode, NewName} ->
            [_Name, Host] = string:split(atom_to_list(Node), "@"),
            N = list_to_atom(atom_to_list(NewName) ++ "@" ++ Host),
            CLabel = {start, VarNode, list_to_atom(Host), NewName},
            case {extract_log(LMap, Pid, start), Node, lists:member(N, Nodes)} of
                {{found, {start, success, N}, NewLog}, _, _} ->
                    fwd_start_s(P0, Result#result{label = CLabel}, Sys, #{node => N, new_log => NewLog});
                {{found, {start, failure, N}, NewLog}, _, _} ->
                    fwd_start_f(P0, Result#result{label = CLabel}, Sys, #{node => N, new_log => NewLog});
                {_, 'nonode@nohost', _} ->
                    error(no_alive);
                {_, _, false} ->
                    fwd_start_s(P0, Result#result{label = CLabel}, Sys, #{node => N});
                {_, _, true} ->
                    fwd_start_f(P0, Result#result{label = CLabel}, Sys, #{node => N})
            end;
        {start, _, Host, NewName} ->
            N = list_to_atom(atom_to_list(NewName) ++ "@" ++ atom_to_list(Host)),
            case {extract_log(LMap, Pid, start), Node, lists:member(N, Nodes)} of
                {{found, {start, success, N}, NewLog}, _, _} ->
                    fwd_start_s(P0, Result, Sys, #{node => N, new_log => NewLog});
                {{found, {start, failure, N}, NewLog}, _, _} ->
                    fwd_start_f(P0, Result, Sys, #{node => N, new_log => NewLog});
                {_, 'nonode@nohost', _} ->
                    error(no_alive);
                {_, _, false} ->
                    fwd_start_s(P0, Result, Sys, #{node => N});
                {_, _, true} ->
                    fwd_start_f(P0, Result, Sys, #{node => N})
            end;
        {send, _Dest, _Val} ->
            case extract_log(LMap, Pid, send) of
                {found, Uid, NewLog} -> fwd_send(P0, Result, Sys, #{uid => Uid, new_log => NewLog});
                not_found -> fwd_send(P0, Result, Sys, #{})
            end;
        {rec, VarBody, _Cs} when Es == [VarBody] ->
            fwd_rec(P0, Result, Sys, #{mode => Mode, sched => Sched})
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

rdep(Pid, LMap0) ->
    LMap = remove_dependents_spawn(Pid, LMap0),
    DropEmpty =
        fun
            (_, []) -> false;
            (_, _) -> true
        end,
    maps:filter(DropEmpty, LMap).

remove_dependents_spawn(Pid0, LMap0) when is_map_key(Pid0, LMap0) ->
    lists:foldl(
        fun entry_dependents/2,
        maps:remove(Pid0, LMap0),
        maps:get(Pid0, LMap0)
    );
remove_dependents_spawn(_Pid, LMap) ->
    LMap.

remove_dependents_receive(Uid0, LMap0) ->
    RemoveAfterReceive =
        fun(Pid0, LMap1) ->
            {Independent, Dependent} = lists:splitwith(
                fun(Entry) -> Entry =/= {'receive', Uid0} end,
                maps:get(Pid0, LMap1)
            ),
            case Dependent of
                [] ->
                    LMap1;
                _ ->
                    LMap = lists:foldl(
                        fun entry_dependents/2,
                        maps:put(Pid0, Independent, LMap1),
                        Dependent
                    ),
                    throw(LMap)
            end
        end,
    try
        lists:foldl(
            RemoveAfterReceive,
            LMap0,
            maps:keys(LMap0)
        )
    catch
        throw:LMap -> LMap
    end.

entry_dependents({spawn, Pid}, LMap) -> remove_dependents_spawn(Pid, LMap);
entry_dependents({send, Uid}, LMap) -> remove_dependents_receive(Uid, LMap);
entry_dependents({'receive', _Uid}, LMap) -> LMap.

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.
%%

-spec expression_option(Expr, Proc, Mode, Sys) -> Rule when
    Expr :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
    Proc :: cauder_types:proc(),
    Sys :: cauder_types:sys(),
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
check_reducibility([], #proc{node = Node, pid = Pid}, _, #sys{traces = LMap, nodes = Nodes}, nodes) ->
    SNodes = Nodes -- [Node],
    Log = extract_log(LMap, Pid, nodes),
    case Log of
        not_found -> ?RULE_NODES;
        {found, {nodes, {LogNodes}}, _} when LogNodes =:= SNodes -> ?RULE_NODES;
        _ -> ?NOT_EXP
    end;
check_reducibility([Cs | []], #proc{pid = Pid, env = Bs}, _, #sys{traces = LMap, mail = Mail} = Sys, {'receive', Mode}) ->
    IsMatch =
        case Mode of
            normal ->
                cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, ?SCHEDULER_Random, Sys) =/= nomatch;
            replay ->
                Log = maps:get(Pid, LMap, []),
                [{'receive', Uid} | _] = Log,
                cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
        end,
    case IsMatch of
        true -> ?RULE_RECEIVE;
        false -> ?NOT_EXP
    end;
check_reducibility([], #proc{pid = Pid}, _, #sys{traces = LMap, nodes = Nodes}, spawn) ->
    Log = extract_log(LMap, Pid, spawn),
    case Log of
        not_found ->
            ?RULE_START;
        {found, {spawn, Node, Result, _}, _} ->
            case {Result, lists:member(Node, Nodes)} of
                {success, false} -> ?NOT_EXP;
                {_, _} -> ?RULE_START
            end
    end;
check_reducibility([], #proc{pid = Pid}, _, #sys{traces = LMap, nodes = Nodes}, start) ->
    Log = extract_log(LMap, Pid, start),
    case Log of
        not_found ->
            ?RULE_START;
        {found, {start, Result, Node}, _} ->
            FailedSpawnsExist = cauder_utils:find_process_with_failed_spawn(LMap, Node),
            FutureReads = cauder_utils:find_process_with_future_reads(LMap, Node),
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

extract_log(LMap, Pid, nodes) ->
    case LMap of
        #{Pid := [{nodes, Nodes} | RestLog]} ->
            {found, {nodes, Nodes}, RestLog};
        _ ->
            not_found
    end;
extract_log(LMap, Pid, start) ->
    case LMap of
        #{Pid := [{start, {Result, Node}} | RestLog]} ->
            {found, {start, Result, Node}, RestLog};
        _ ->
            not_found
    end;
extract_log(LMap, Pid, spawn) ->
    case LMap of
        #{Pid := [{spawn, {Node, Result, LogPid}} | RestLog]} ->
            {found, {spawn, Node, Result, LogPid}, RestLog};
        _ ->
            not_found
    end;
extract_log(LMap, Pid, send) ->
    case LMap of
        #{Pid := [{send, LogUid} | RestLog]} ->
            {found, LogUid, RestLog};
        _ ->
            not_found
    end.

-spec fwd_tau(Proc, Result, Sys) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_tau(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, exprs = Es, stack = Stk},
    #sys{procs = PMap} = Sys
) ->
    P = P0#proc{
        hist = [{tau, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env = Bs,
        exprs = Es
    },
    Sys#sys{
        procs = PMap#{Pid => P}
    }.

-spec fwd_self(Proc, Result, Sys) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_self(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, exprs = Es, stack = Stk, label = {self, VarPid}},
    #sys{procs = PMap} = Sys
) ->
    P = P0#proc{
        hist = [{self, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, Pid)
    },
    Sys#sys{
        procs = PMap#{Pid => P}
    }.

-spec fwd_node(Proc, Result, Sys) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_node(
    #proc{node = Node, hist = Hist, pid = Pid, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, exprs = Es, stack = Stk, label = {node, VarNode}},
    #sys{procs = PMap} = Sys
) ->
    P = P0#proc{
        hist = [{node, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, Node)
    },
    Sys#sys{
        procs = PMap#{Pid => P}
    }.

-spec fwd_nodes(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:sys(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_nodes(
    #proc{node = Node, pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, exprs = Es, stack = Stk, label = {nodes, VarNodes}},
    #sys{nodes = Nodes, traces = LMap, procs = PMap} = Sys,
    Opts
) ->
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NLog -> NLog
        end,
    P = P0#proc{
        hist = [{nodes, Bs0, Es0, Stk0, Nodes -- [Node]} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNodes, Nodes -- [Node])
    },
    Sys#sys{
        procs = PMap#{Pid => P},
        traces = LMap#{Pid => NewLog}
    }.

-spec fwd_spawn_s(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_spawn_s(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{label = {spawn, VarPid, N, M, F, As}, env = Bs, exprs = Es, stack = Stk},
    #sys{traces = LMap, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    Inline = maps:get(inline, Opts, false),
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NLog -> NLog
        end,
    SpawnPid =
        case maps:get(pid, Opts, not_found) of
            not_found -> cauder_utils:fresh_pid();
            LogPid -> LogPid
        end,
    P1 = P0#proc{
        hist = [{spawn, Bs0, Es0, Stk0, N, SpawnPid} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
    },
    P2 =
        case Inline of
            false ->
                #proc{
                    node = N,
                    pid = SpawnPid,
                    exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
                    spf = {M, F, length(As)}
                };
            true ->
                {_, Line, Fun} = FunLiteral = maps:get(fun_literal, Opts, error_fun_inline),
                {arity, A} = erlang:fun_info(Fun, arity),
                #proc{
                    node = N,
                    pid = SpawnPid,
                    exprs = [{apply_fun, Line, FunLiteral, []}],
                    spf = {M, F, A}
                }
        end,
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = SpawnPid
    },
    Sys#sys{
        procs = PMap#{Pid => P1, SpawnPid => P2},
        traces = LMap#{Pid => NewLog},
        x_trace = [T | Trace]
    }.

-spec fwd_spawn_f(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_spawn_f(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{label = {spawn, VarPid, N, _M, _F, _As}, env = Bs, exprs = Es, stack = Stk},
    #sys{traces = LMap, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NLog -> NLog
        end,
    SpawnPid =
        case maps:get(pid, Opts, not_found) of
            not_found -> cauder_utils:fresh_pid();
            LogPid -> LogPid
        end,
    P1 = P0#proc{
        hist = [{spawn, Bs0, Es0, Stk0, N, SpawnPid} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
    },
    T = #x_trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to = SpawnPid
    },
    Sys#sys{
        procs = PMap#{Pid => P1},
        traces = LMap#{Pid => NewLog},
        x_trace = [T | Trace]
    }.

-spec fwd_start_s(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_start_s(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{label = {start, VarNode, _Host, _Name}, env = Bs, exprs = Es, stack = Stk},
    #sys{nodes = Nodes, traces = LMap, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    NewNode =
        case maps:get(node, Opts, not_found) of
            not_found -> error;
            RemoteNode -> RemoteNode
        end,
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NLog -> NLog
        end,
    P = P0#proc{
        hist = [{start, success, Bs0, Es0, Stk0, NewNode} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, {ok, NewNode})
    },
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = success,
        node = NewNode
    },
    Sys#sys{
        procs = PMap#{Pid => P},
        x_trace = [T | Trace],
        nodes = [NewNode] ++ Nodes,
        traces = LMap#{Pid => NewLog}
    }.

-spec fwd_start_f(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_start_f(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{label = {start, VarNode, _Host, _Name}, env = Bs, exprs = Es, stack = Stk},
    #sys{traces = LMap, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    NewNode =
        case maps:get(node, Opts, not_found) of
            not_found -> error;
            RemoteNode -> RemoteNode
        end,
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NewLog0 -> NewLog0
        end,
    Err = {error, {already_running, NewNode}},
    P = P0#proc{
        hist = [{start, failure, Bs0, Es0, Stk0, NewNode} | Hist],
        stack = Stk,
        env = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, Err)
    },
    T = #x_trace{
        type = ?RULE_START,
        from = Pid,
        res = failure,
        node = NewNode
    },
    Sys#sys{
        procs = PMap#{Pid => P},
        x_trace = [T | Trace],
        traces = LMap#{Pid => NewLog}
    }.

-spec fwd_send(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_send(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, exprs = Es, stack = Stk, label = {send, Dest, Val}},
    #sys{mail = Mail, traces = LMap, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    NewLog =
        case maps:get(new_log, Opts, not_found) of
            not_found -> [];
            NLog -> NLog
        end,
    M = #message{
        src = Pid,
        dest = Dest,
        value = Val
    },
    UM =
        case maps:get(uid, Opts, not_found) of
            not_found -> M;
            _Uid -> M#message{uid = _Uid}
        end,
    P = P0#proc{
        hist = [{send, Bs0, Es0, Stk0, UM} | Hist],
        stack = Stk,
        env = Bs,
        exprs = Es
    },
    T = #x_trace{
        type = ?RULE_SEND,
        from = Pid,
        to = Dest,
        val = Val,
        time = UM#message.uid
    },
    Sys#sys{
        mail = cauder_mailbox:add(UM, Mail),
        procs = PMap#{Pid => P},
        traces = LMap#{Pid => NewLog},
        x_trace = [T | Trace]
    }.

-spec fwd_rec(Proc, Result, Sys, Opts) -> NewSystem when
    Proc :: cauder_types:proc(),
    Result :: cauder_types:result(),
    Sys :: cauder_types:system(),
    Opts :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_rec(
    #proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
    #result{env = Bs, stack = Stk, label = {rec, _, Cs}},
    #sys{mail = Mail, traces = LMap0, procs = PMap, x_trace = Trace} = Sys,
    Opts
) ->
    Mode =
        case maps:get(mode, Opts, not_found) of
            not_found -> error(mode_not_found);
            Mode0 -> Mode0
        end,
    Sched =
        case maps:get(sched, Opts, not_found) of
            not_found -> error(sched_not_found);
            Sched0 -> Sched0
        end,
    {{Bs1, Es1, {Msg, QPos}, Mail1}, LMap1} =
        case Mode of
            normal ->
                {_, _, {#message{uid = Uid}, _}, _} = Match = cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, Sched, Sys),
                LMap =
                    case maps:get(Pid, LMap0, []) of
                        %% If the chosen message is the same specified in the log don't invalidate the log
                        [{'receive', Uid} | RestLog] -> maps:put(Pid, RestLog, LMap0);
                        _ -> rdep(Pid, LMap0)
                    end,
                {Match, LMap};
            replay ->
                #{Pid := [{'receive', LogUid} | RestLog]} = LMap0,
                Match = cauder_eval:match_rec_uid(Cs, Bs, LogUid, Mail),
                LMap = LMap0#{Pid => RestLog},
                {Match, LMap}
        end,
    P = P0#proc{
        hist = [{rec, Bs0, Es0, Stk0, Msg, QPos} | Hist],
        stack = Stk,
        env = cauder_utils:merge_bindings(Bs, Bs1),
        exprs = Es1
    },
    T = #x_trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val = Msg#message.value,
        time = Msg#message.uid
    },
    Sys#sys{
        mail = Mail1,
        procs = PMap#{Pid => P},
        traces = LMap1,
        x_trace = [T | Trace]
    }.
