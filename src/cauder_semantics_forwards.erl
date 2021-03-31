%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([step/2, options/1]).

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

step(Sys, Pid) ->
  {#proc{node = Node, pid = Pid, stack = Stk0, env = Bs0, exprs = Es0} = P0, _} = maps:take(Pid, Sys#sys.procs),
  #result{label = Label, exprs = Es} = Result = cauder_eval:seq(Bs0, Es0, Stk0),
  #sys{nodes = Nodes, logs = LMap} = Sys,
  case Label of
    tau -> fwd_tau(P0, Result, Sys);
    {self, _VarPid} -> fwd_self(P0, Result, Sys);
    {node, _VarNode} -> fwd_node(P0, Result, Sys);
    {nodes, _VarNodes} ->
      case extract_log(LMap, Pid, nodes) of
        {found, {nodes, _}, NewLog} -> fwd_nodes(P0, Result, Sys, #{new_log => NewLog});
        not_found                   -> fwd_nodes(P0, Result, Sys, #{})
      end;
    {spawn, VarPid, M, F, As} ->
      CLabel = {spawn, VarPid, Node, M, F, As},
      case extract_log(LMap, Pid, spawn) of
        {found, {spawn, _N, succ, NewPid}, NewLog} -> fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{pid => NewPid, new_log => NewLog});
        {found, {spawn, _N, fail, NewPid}, NewLog} -> fwd_spawn_f(P0, Result#result{label = CLabel}, Sys, #{pid => NewPid, new_log => NewLog});
        not_found                                  -> fwd_spawn_s(P0, Result#result{label = CLabel}, Sys, #{})
      end;
    {spawn, _VarPid, N, _M, _F, _As} ->
      case {extract_log(LMap, Pid, spawn) ,lists:member(N, Nodes)} of
        {{found, {spawn, _N, fail, NewPid}, NewLog}, _} -> fwd_spawn_f(P0, Result, Sys, #{pid => NewPid, new_log => NewLog});
        {{found, {spawn, _N, succ, NewPid}, NewLog}, _} -> fwd_spawn_s(P0, Result, Sys, #{pid => NewPid, new_log => NewLog});
        {_, false}                                      -> fwd_spawn_f(P0, Result, Sys, #{});
        {_, true}                                       -> fwd_spawn_s(P0, Result, Sys, #{})
      end;
    {start, VarNode, NewName} ->
      [_Name, Host] = string:split(atom_to_list(Node), "@"),
      N = list_to_atom(atom_to_list(NewName) ++ "@" ++ Host),
      CLabel = {start, VarNode, list_to_atom(Host), NewName},
      case {extract_log(LMap, Pid, start), lists:member(N, Nodes)} of
        {{found, {start, succ, N}, NewLog}, _} -> fwd_start_s(P0, Result#result{label = CLabel}, Sys, #{node => N, new_log => NewLog});
        {{found, {start, fail, N}, NewLog}, _} -> fwd_start_f(P0, Result#result{label = CLabel}, Sys, #{node => N, new_log => NewLog});
        {_, false}                             -> fwd_start_s(P0, Result#result{label = CLabel}, Sys, #{node => N});
        {_, true}                              -> fwd_start_f(P0, Result#result{label = CLabel}, Sys, #{node => N})
      end;
    {start, VarNode, Host, NewName} ->
      N = list_to_atom(atom_to_list(NewName) ++ "@" ++ atom_to_list(Host)),
      case {extract_log(LMap, Pid, start), lists:member(N, Nodes)} of
        {{found, {start, succ, N}, NewLog}, _} -> fwd_start_s(P0, Result, Sys, #{node => N, new_log => NewLog});
        {{found, {start, fail, N}, NewLog}, _} -> fwd_start_f(P0, Result, Sys, #{node => N, new_log => NewLog});
        {_, false}                             -> fwd_start_s(P0, Result, Sys, #{node => N});
        {_, true}                              -> fwd_start_f(P0, Result, Sys, #{node => N})
      end;
    {send, _Dest, _Val} ->
      case extract_log(LMap, Pid, send) of
        {found, Uid, NewLog} -> fwd_send(P0, Result, Sys, #{uid => Uid, new_log => NewLog});
        not_found            -> fwd_send(P0, Result, Sys, #{})
      end;
    {rec, VarBody, _Cs} when Es == [VarBody] ->
      case extract_log(LMap, Pid, 'receive') of
        {found, {'receive', Uid}, NewLog} -> fwd_rec(P0, Result, Sys, #{new_log => NewLog, uid => Uid});
        not_found                         -> fwd_rec(P0, Result, Sys, #{})
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System) -> Options when
    System :: cauder_types:system(),
    Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys) ->
  lists:foldl(
    fun
      (#proc{exprs = E, pid = Pid} = P, Opts) ->
                 case expression_option(E, P, Sys) of
                   ?NOT_EXP -> Opts;
                   Rule ->
                     Opt = #opt{
                              sem  = ?MODULE,
                              pid  = Pid,
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

-spec expression_option(Expr, Proc, Sys) -> Rule when
    Expr :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
    Proc :: cauder_types:proc(),
    Sys  :: cauder_types:sys(),
    Rule :: cauder_types:rule() | ?NOT_EXP.

expression_option(E, P, Sys) when not is_list(E) ->
  expression_option([E], P, Sys);
expression_option([E0 | Es0], #proc{env = Bs, stack = Stk} = Proc, Sys) ->
  case is_reducible(E0, Bs) of
    false ->
      case {Es0, Stk} of
        {[], []} -> ?NOT_EXP;
        _ -> ?RULE_SEQ
      end;
    true ->
      case E0 of
        {var, _, _}                -> ?RULE_SEQ;
        {'if', _, _}               -> ?RULE_SEQ;
        {'make_fun', _, _, _}      -> ?RULE_SEQ;
        {self, _}                  -> ?RULE_SELF;
        {node, _}                  -> ?RULE_NODE;
        {tuple, _, Es}             -> expression_option(Es, Proc, Sys);
        {nodes, _}                 -> check_reducibility([], Proc, Sys, nodes);
        {cons, _, H, T}            -> check_reducibility([H] ++ T, Proc, Sys, cons);
        {'case', _, E, _}          -> check_reducibility([E], Proc, Sys, 'case');
        {'receive', _, Cs}         -> check_reducibility([Cs], Proc, Sys, 'receive');
        {bif, _, _, _, As}         -> check_reducibility([As], Proc, Sys, bif);
        {spawn, _, F}              -> check_reducibility([F], Proc, Sys, spawn);
        {spawn, _, M, F, As}       -> check_reducibility([M,F,As], Proc, Sys, spawn);
        {spawn, _, N, M, F, As}    -> check_reducibility([N,M,F,As], Proc, Sys, spawn);
        {start, _, N}              -> check_reducibility([N], Proc, Sys, start);
        {start, _, H, N}           -> check_reducibility([H, N], Proc, Sys, start);
        {send, _, L, R}            -> check_reducibility([L, R], Proc, Sys, send);
        {send_op, _, L, R}         -> check_reducibility([L, R], Proc, Sys, send);
        {local_call, _, _, As}     -> check_reducibility([As], Proc, Sys, local_call);
        {remote_call, _, _, _, As} -> check_reducibility([As], Proc, Sys, remote_call);
        {apply, _, M, F, As}       -> check_reducibility([M, F, As], Proc, Sys, apply);
        {apply_fun, _, Fun, As}    -> check_reducibility([Fun, As], Proc, Sys, apply_fun);
        {match, _, P, E}           -> check_reducibility([P, E], Proc, Sys, match);
        {op, _, _, Es}             -> check_reducibility([Es], Proc, Sys, op);
        {'andalso', _, L, R}       -> check_reducibility([L,R], Proc, Sys, 'andalso');
        {'orelse', _, L, R}        -> check_reducibility([L,R], Proc, Sys, 'orelse')
      end
  end.

check_reducibility([], _, _, send)                                   -> ?RULE_SEND;
check_reducibility([], _, _, send_op)                                -> ?RULE_SEND;
check_reducibility([], _, _, local_call)                             -> ?RULE_SEQ;
check_reducibility([], _, _, remote_call)                            -> ?RULE_SEQ;
check_reducibility([], _, _, apply)                                  -> ?RULE_SEQ;
check_reducibility([], _, _, apply_fun)                              -> ?RULE_SEQ;
check_reducibility([], _, _, match)                                  -> ?RULE_SEQ;
check_reducibility([], _, _, op)                                     -> ?RULE_SEQ;
check_reducibility([], _, _, 'andalso')                              -> ?RULE_SEQ;
check_reducibility([], _, _, 'orelse')                               -> ?RULE_SEQ;
check_reducibility([], _, _, 'case')                                 -> ?RULE_SEQ;
check_reducibility([], _, _, bif)                                    -> ?RULE_SEQ;
check_reducibility([], #proc{node = Node, pid = Pid}, #sys{logs = LMap, nodes = Nodes}, nodes) ->
  SNodes = Nodes -- [Node],
  Log = extract_log(LMap, Pid, nodes),
  case Log of
    not_found -> ?RULE_NODES;
    {found, {nodes, {LogNodes}}, _} when LogNodes =:= SNodes -> ?RULE_NODES;
    _ -> ?NOT_EXP
  end;
check_reducibility([Cs | []], #proc{pid = Pid, env = Bs}, #sys{logs = LMap, mail = Mail}, 'receive') ->
  IsMatch =
    case extract_log(LMap, Pid, 'receive') of
      not_found -> cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail) =/= nomatch;
      {found, {'receive', Uid}, _} -> cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
    end,
  case IsMatch of
    true -> ?RULE_RECEIVE;
    false -> ?NOT_EXP
  end;
check_reducibility([], #proc{pid = Pid}, #sys{logs = LMap, nodes = Nodes}, spawn) ->
  Log = extract_log(LMap, Pid, spawn),
  case Log of
    not_found -> ?RULE_START;
    {found, {spawn, Node, Result, _}, _} ->
      case {Result, lists:member(Node, Nodes)} of
        {succ, false} -> ?NOT_EXP;
        {_, _}        -> ?RULE_START
      end
  end;
check_reducibility([], #proc{pid = Pid}, #sys{logs = LMap, nodes = Nodes}, start) ->
  Log = extract_log(LMap, Pid, start),
  case Log of
    not_found -> ?RULE_START;
    {found, {start, Result, Node}, _} ->
      FailedSpawnsExist = cauder_utils:find_process_with_failed_spawn(LMap, Node),
      FutureReads = cauder_utils:find_process_with_future_reads(LMap, Node),
      NodeAlreadyExists = lists:member(Node, Nodes),
      case {Result, FailedSpawnsExist, NodeAlreadyExists, FutureReads} of
        {succ, {value, _}, _, _} -> ?NOT_EXP; %at least one spawn has still to fail
        {fail, _, false, _}      -> ?NOT_EXP;
        {succ, _, _, {value, _}} -> ?NOT_EXP;
        _                     -> ?RULE_START
      end
  end;
check_reducibility([H|T], #proc{env = Bs} = P, Sys, ExprType) ->
  case is_reducible(H,Bs) of
    true  -> expression_option(H, P, Sys);
    false -> check_reducibility(T, P, Sys, ExprType)
  end.

extract_log(LMap, Pid, nodes) ->
  case LMap of
    #{Pid := [{nodes, Nodes} | RestLog]} ->
      {found, {nodes, Nodes}, RestLog};
    _ -> not_found
  end;
extract_log(LMap, Pid, start) ->
  case LMap of
    #{Pid := [{start, {Result, Node}} | RestLog]} ->
      {found, {start, Result, Node}, RestLog};
    _ -> not_found
  end;
extract_log(LMap, Pid, spawn) ->
  case LMap of
    #{Pid := [{spawn, {Node, Result, LogPid}} | RestLog]} ->
      {found, {spawn, Node, Result, LogPid}, RestLog};
    _ -> not_found
  end;
extract_log(LMap, Pid, send) ->
  case LMap of
    #{Pid := [{send, LogUid} | RestLog]} ->
      {found, LogUid, RestLog};
    _ -> not_found
  end;
extract_log(LMap, Pid, 'receive') ->
  case LMap of
    #{Pid := [{'receive', LogUid} | RestLog]} ->
      {found, {'receive', LogUid} , RestLog};
    _ -> not_found
  end.

-spec fwd_tau(Proc, Result, Sys) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_tau(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
        #result{env = Bs, exprs = Es, stack = Stk},
        #sys{procs = PMap} = Sys) ->
  P = P0#proc{
        hist  = [{tau, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
       },
  Sys#sys{
    procs = PMap#{Pid => P}
   }.

-spec fwd_self(Proc, Result, Sys) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_self(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
         #result{env = Bs, exprs = Es, stack = Stk, label = {self, VarPid}},
         #sys{procs = PMap} = Sys) ->
  P = P0#proc{
        hist  = [{self, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, Pid)
       },
  Sys#sys{
    procs = PMap#{Pid => P}
   }.

-spec fwd_node(Proc, Result, Sys) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:sys(),
    NewSystem :: cauder_types:system().

fwd_node(#proc{node = Node, hist = Hist, pid = Pid, stack = Stk0, env = Bs0, exprs = Es0} = P0,
         #result{env = Bs, exprs = Es, stack = Stk, label = {node, VarNode}},
         #sys{procs = PMap} = Sys) ->
  P = P0#proc{
        hist  = [{node, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, Node)
       },
  Sys#sys{
    procs = PMap#{Pid => P}
   }.

-spec fwd_nodes(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:sys(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_nodes(#proc{node = Node, pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
          #result{env = Bs, exprs = Es, stack = Stk, label = {nodes, VarNodes}},
          #sys{nodes = Nodes, logs = LMap, procs = PMap} = Sys, Opts) ->
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog  -> NLog
           end,
  P = P0#proc{
        hist  = [{nodes, Bs0, Es0, Stk0, Nodes -- [Node]} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNodes, Nodes -- [Node])
       },
  Sys#sys{
    procs = PMap#{Pid => P},
    logs  = LMap#{Pid => NewLog}
   }.

-spec fwd_spawn_s(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_spawn_s(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
            #result{label = {spawn, VarPid, N, M, F, As}, env = Bs, exprs = Es, stack = Stk},
            #sys{logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog  -> NLog
           end,
  SpawnPid = case maps:get(pid, Opts, not_found) of
               not_found -> cauder_utils:fresh_pid();
               LogPid    -> LogPid
             end,
  P1 = P0#proc{
         hist  = [{spawn, Bs0, Es0, Stk0, N, SpawnPid} | Hist],
         stack = Stk,
         env   = Bs,
         exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
        },
  P2 = #proc{
          node  = N,
          pid   = SpawnPid,
          exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
          spf   = {M, F, length(As)}
         },
  T = #trace{
         type = ?RULE_SPAWN,
         from = Pid,
         to   = SpawnPid
        },
  Sys#sys{
    procs = PMap#{Pid => P1, SpawnPid => P2},
    logs  = LMap#{Pid => NewLog},
    trace = [T | Trace]
   }.

-spec fwd_spawn_f(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_spawn_f(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
            #result{label = {spawn, VarPid, N, _M, _F, _As}, env = Bs, exprs = Es, stack = Stk},
            #sys{logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog  -> NLog
           end,
  SpawnPid = case maps:get(pid, Opts, not_found) of
               not_found -> cauder_utils:fresh_pid();
               LogPid  -> LogPid
             end,
  P1 = P0#proc{
         hist  = [{spawn, Bs0, Es0, Stk0, N, SpawnPid} | Hist],
         stack = Stk,
         env   = Bs,
         exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
        },
  T = #trace{
         type = ?RULE_SPAWN,
         from = Pid,
         to   = SpawnPid
        },
  Sys#sys{
    procs = PMap#{Pid => P1},
    logs  = LMap#{Pid => NewLog},
    trace = [T | Trace]
   }.

-spec fwd_start_s(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_start_s(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
            #result{label = {start, VarNode, _Host, _Name}, env = Bs, exprs = Es, stack = Stk},
            #sys{nodes = Nodes, logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewNode = case maps:get(node, Opts, not_found) of
              not_found  -> error;
              RemoteNode -> RemoteNode
            end,
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog      -> NLog
           end,
  P = P0#proc{
        hist  = [{start, success, Bs0, Es0, Stk0, NewNode} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, {ok, NewNode})
       },
  T = #trace{
         type = ?RULE_START,
         from = Pid,
         res  = succ,
         node = NewNode
        },
  Sys#sys{
    procs = PMap#{Pid => P},
    trace = [T | Trace],
    nodes = [NewNode] ++ Nodes,
    logs  = LMap#{Pid => NewLog}
   }.

-spec fwd_start_f(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_start_f(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
            #result{label = {start, VarNode, _Host, _Name}, env = Bs, exprs = Es, stack = Stk},
            #sys{logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewNode = case maps:get(node, Opts, not_found) of
              not_found  -> error;
              RemoteNode -> RemoteNode
            end,
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             _NLog     -> _NLog
           end,
  Err = {error, {already_running, NewNode}},
  P = P0#proc{
        hist  = [{start, fail, Bs0, Es0, Stk0, NewNode} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarNode, Err)
       },
  T = #trace{
         type = ?RULE_START,
         from = Pid,
         res  = fail,
         node = NewNode
        },
  Sys#sys{
    procs = PMap#{Pid => P},
    trace = [T | Trace],
    logs  = LMap#{Pid => NewLog}
   }.

-spec fwd_send(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_send(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
         #result{env = Bs, exprs = Es, stack = Stk, label = {send, Dest, Val}},
         #sys{mail = Ms, logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog      -> NLog
           end,
  Uid = case maps:get(uid, Opts, not_found) of
          not_found -> cauder_utils:fresh_uid();
          _Uid      -> _Uid
        end,
  M = #msg{
         dest = Dest,
         val  = Val,
         uid  = Uid
        },
  P = P0#proc{
        hist  = [{send, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
       },
  T = #trace{
         type = ?RULE_SEND,
         from = Pid,
         to   = Dest,
         val  = Val,
         time = Uid
        },
  Sys#sys{
    mail  = [M | Ms],
    procs = PMap#{Pid => P},
    logs  = LMap#{Pid => NewLog},
    trace = [T | Trace]
   }.

-spec fwd_rec(Proc, Result, Sys, Opts) -> NewSystem when
    Proc      :: cauder_types:proc(),
    Result    :: cauder_types:result(),
    Sys       :: cauder_types:system(),
    Opts      :: cauder_types:fwd_opts(),
    NewSystem :: cauder_types:system().

fwd_rec(#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0,
        #result{env = Bs, stack = Stk, label = {rec, _, Cs}},
        #sys{mail = Ms, logs = LMap, procs = PMap, trace = Trace} = Sys, Opts) ->
  NewLog = case maps:get(new_log, Opts, not_found) of
             not_found -> [];
             NLog      -> NLog
           end,
  Uid = case maps:get(uid, Opts, not_found) of
          not_found -> cauder_utils:fresh_uid();
          _Uid      -> _Uid
        end,
  {{Bs1, Es1, M = #msg{dest = Pid, val = Val, uid = Uid}, Ms1}, NewLog} = {cauder_eval:match_rec_uid(Cs, Bs, Uid, Ms), NewLog},
  P = P0#proc{
        hist  = [{rec, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = cauder_utils:merge_bindings(Bs, Bs1),
        exprs = Es1
       },
  T = #trace{
         type = ?RULE_RECEIVE,
         from = Pid,
         val  = Val,
         time = Uid
        },
  Sys#sys{
    mail  = Ms1,
    procs = PMap#{Pid => P},
    logs  = LMap#{Pid => NewLog},
    trace = [T | Trace]
   }.
