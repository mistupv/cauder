%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-include("cauder.hrl").
-include("cauder_history.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> NewSystem when
    System :: cauder_system:system(),
    Pid :: cauder_process:proc_id(),
    NewSystem :: cauder_system:system().

step(#sys{nodes = Nodes, mail = Mail, traces = LMap, x_trace = Trace} = Sys, Pid) ->
    {#proc{pid = Pid, hist = Hist} = P0, PMap} = maps:take(Pid, Sys#sys.procs),
    {Entry, RestHist} = cauder_history:pop(Hist),
    case Entry of
        {value, #h_tau{
            env = Bs,
            expr = Es,
            stack = Stk
        }} ->
            P = P0#proc{
                hist = RestHist,
                env = Bs,
                exprs = Es,
                stack = Stk
            },
            Sys#sys{
                mail = Mail,
                procs = PMap#{Pid => P}
            };
        {value, #h_self{
            env = Bs,
            expr = Es,
            stack = Stk
        }} ->
            P = P0#proc{
                hist = RestHist,
                env = Bs,
                exprs = Es,
                stack = Stk
            },
            Sys#sys{
                mail = Mail,
                procs = PMap#{Pid => P}
            };
        {value, #h_node{
            env = Bs,
            expr = Es,
            stack = Stk
        }} ->
            P = P0#proc{
                hist = RestHist,
                env = Bs,
                exprs = Es,
                stack = Stk
            },
            Sys#sys{
                mail = Mail,
                procs = PMap#{Pid => P}
            };
        {value, #h_nodes{
            env = Bs,
            expr = Es,
            stack = Stk,
            nodes = HistNodes
        }} ->
            P = P0#proc{
                hist = RestHist,
                env = Bs,
                exprs = Es,
                stack = Stk
            },
            Sys#sys{
                mail = Mail,
                procs = PMap#{Pid => P},
                traces = maps:update_with(Pid, fun(Log) -> [{nodes, {HistNodes}} | Log] end, [], LMap)
            };
        {value, #h_spawn{
            env = Bs,
            expr = Es,
            stack = Stk,
            node = Node,
            pid = Gid
        }} ->
            P = P0#proc{
                hist = RestHist,
                stack = Stk,
                env = Bs,
                exprs = Es
            },
            T = #x_trace{
                type = ?RULE_SPAWN,
                from = Pid,
                to = Gid
            },
            Result =
                case cauder_utils:process_node(PMap, Gid) of
                    false -> failure;
                    _ -> success
                end,
            NewLog = {spawn, {Node, Gid}, Result},
            Sys#sys{
                mail = Mail,
                procs = maps:remove(Gid, PMap#{Pid => P}),
                traces = maps:update_with(Pid, fun(Log) -> [NewLog | Log] end, [NewLog], LMap),
                x_trace = lists:delete(T, Trace)
            };
        {value, #h_start{
            env = Bs,
            expr = Es,
            stack = Stk,
            node = Node,
            success = true
        }} ->
            P = P0#proc{
                hist = RestHist,
                stack = Stk,
                env = Bs,
                exprs = Es
            },
            T = #x_trace{
                type = ?RULE_START,
                from = Pid,
                res = success,
                node = Node
            },
            Sys#sys{
                nodes = Nodes -- [Node],
                mail = Mail,
                procs = PMap#{Pid => P},
                x_trace = lists:delete(T, Trace),
                traces = maps:update_with(Pid, fun(Log) -> [{start, Node, success} | Log] end, [], LMap)
            };
        {value, #h_start{
            env = Bs,
            expr = Es,
            stack = Stk,
            node = Node,
            success = false
        }} ->
            P = P0#proc{
                hist = RestHist,
                stack = Stk,
                env = Bs,
                exprs = Es
            },
            T = #x_trace{
                type = ?RULE_START,
                from = Pid,
                res = failure,
                node = Node
            },
            Sys#sys{
                mail = Mail,
                procs = PMap#{Pid => P},
                x_trace = lists:delete(T, Trace),
                traces = maps:update_with(Pid, fun(Log) -> [{start, Node, failure} | Log] end, [], LMap)
            };
        {value, #h_send{
            env = Bs,
            expr = Es,
            stack = Stk,
            msg = #message{dest = Dest, value = Val, uid = Uid} = Msg
        }} ->
            {_Msg, OldMsgs} = cauder_mailbox:delete(Msg, Mail),
            P = P0#proc{
                hist = RestHist,
                stack = Stk,
                env = Bs,
                exprs = Es
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
                procs = PMap#{Pid => P},
                traces = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
                x_trace = lists:delete(T, Trace)
            };
        {value, #h_receive{
            env = Bs,
            expr = Es,
            stack = Stk,
            msg = M = #message{uid = Uid, value = Value, dest = Pid},
            q_pos = QPos
        }} ->
            P = P0#proc{
                hist = RestHist,
                stack = Stk,
                env = Bs,
                exprs = Es
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
                procs = PMap#{Pid => P},
                traces = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
                x_trace = lists:delete(T, Trace)
            }
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
    System :: cauder_system:system(),
    Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys) ->
    maps:fold(
        fun(Pid, Proc, Opts) ->
            case process_option(Sys#sys{procs = maps:without([Pid], PMap)}, Proc) of
                ?NULL_OPT -> Opts;
                Opt -> [Opt | Opts]
            end
        end,
        [],
        PMap
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(System, Process) -> Option when
    System :: cauder_system:system(),
    Process :: cauder_process:process(),
    Option :: cauder_types:option() | ?NULL_OPT.

process_option(#sys{procs = PMap} = Sys, #proc{pid = Pid} = P0) ->
    case cauder_history:peek(P0#proc.hist) of
        empty ->
            ?NULL_OPT;
        {value, #h_tau{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEQ};
        {value, #h_self{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SELF};
        {value, #h_node{}} ->
            #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODE};
        {value, #h_nodes{nodes = Nodes}} ->
            ProcViewOfNodes = Sys#sys.nodes -- [P0#proc.node],
            case ProcViewOfNodes =:= Nodes of
                true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODES};
                false -> ?NULL_OPT
            end;
        {value, #h_spawn{pid = SpawnPid}} ->
            try maps:get(SpawnPid, PMap) of
                P1 ->
                    case cauder_history:is_empty(P1#proc.hist) of
                        true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
                        false -> ?NULL_OPT
                    end
            catch
                % this case covers the scenario of a failed spawn
                error:{badkey, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN}
            end;
        {value, #h_start{node = StartNode, success = true}} ->
            ProcWithFailedStart = cauder_utils:find_process_with_failed_start(PMap, StartNode),
            ProcOnNode = cauder_utils:find_process_on_node(PMap, StartNode),
            ProcWithRead = cauder_utils:find_process_with_read(PMap, StartNode),
            case {ProcWithFailedStart, ProcOnNode, ProcWithRead} of
                {false, false, false} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
                _ -> ?NULL_OPT
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
