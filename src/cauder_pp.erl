%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for CauDEr systems.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_pp).

-export([process/2, log_entry/1, history_entry/1, stack_entry/1, expression/1, trace_action/1]).
-export([pid/1, pp_node/1, to_string/1]).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-spec process(Process, Options) -> String when
    Process :: cauder_types:process(),
    Options :: [Option],
    Option :: {icon, boolean()} | {node, boolean() | auto} | {pid, boolean()} | {mfa, boolean()},
    String :: string().

process(#process{node = Node, pid = Pid, mfa = {M, F, A}} = Proc, Options) ->
    IconStr =
        case proplists:get_value('icon', Options) of
            false ->
                "";
            true ->
                case cauder_utils:is_dead(Proc) of
                    true -> ?ICON_DEAD;
                    false -> ?ICON_ALIVE
                end
        end,
    NodeStr =
        case proplists:get_value('node', Options) of
            false -> "";
            auto when Node =:= 'nonode@nohost' -> "";
            auto -> io_lib:format("~p", [Node]);
            true -> io_lib:format("~p", [Node])
        end,
    PidStr =
        case proplists:get_value('pid', Options) of
            false -> "";
            true -> io_lib:format("~p", [Pid])
        end,
    MFAStr =
        case proplists:get_value('mfa', Options) of
            false -> "";
            true -> io_lib:format("~s:~s/~B", [M, F, A])
        end,

    NotEmpty = fun(Str) -> Str =/= [] end,
    ProcInfo = lists:join(", ", lists:filter(NotEmpty, [NodeStr, PidStr, MFAStr])),
    lists:flatten(lists:join(" - ", lists:filter(NotEmpty, [IconStr, ProcInfo]))).

%%%=============================================================================

-spec log_entry(LogEntry) -> String when
    LogEntry :: cauder_types:log_action(),
    String :: string().

log_entry({nodes, Nodes}) -> "nodes(" ++ to_string(Nodes) ++ ")";
log_entry({spawn, {Node, Pid}, success}) -> "spawn(" ++ green(to_string(Node) ++ ", " ++ to_string(Pid)) ++ ")";
log_entry({spawn, {Node, Pid}, failure}) -> "spawn(" ++ red(to_string(Node) ++ ", " ++ to_string(Pid)) ++ ")";
log_entry({send, Uid}) -> "send(" ++ red(to_string(Uid)) ++ ")";
log_entry({'receive', Uid}) -> "rec(" ++ blue(to_string(Uid)) ++ ")";
log_entry({start, NodeName, success}) -> "start(" ++ green(to_string(NodeName)) ++ ")";
log_entry({start, NodeName, failure}) -> "start(" ++ red(to_string(NodeName)) ++ ")".

%%%=============================================================================

-spec history_entry(HistoryEntry) -> String when
    HistoryEntry :: cauder_types:history_entry(),
    String :: string().

history_entry({tau, _Bs, _Es, _Stk}) ->
    "seq";
history_entry({self, _Bs, _Es, _Stk}) ->
    "self";
history_entry({nodes, _Bs, _Es, _Stk, Nodes}) ->
    "nodes(" ++ pp_nodes(Nodes) ++ ")";
history_entry({spawn, _Bs, _Es, _Stk, Node, Pid}) ->
    "spawn(" ++ to_string(Node) ++ ", " ++ to_string(Pid) ++ ")";
history_entry({start, success, _Bs, _Es, _Stk, Node}) ->
    "start(" ++ green(to_string(Node)) ++ ")";
history_entry({start, failure, _Bs, _Es, _Stk, Node}) ->
    "start(" ++ red(to_string(Node)) ++ ")";
history_entry({send, _Bs, _Es, _Stk, #message{val = Val, uid = Uid}}) ->
    "send(" ++ to_string(Val) ++ "," ++ red(to_string(Uid)) ++ ")";
history_entry({rec, _Bs, _Es, _Stk, #message{val = Val, uid = Uid}, _QPos}) ->
    "rec(" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")".

%%%=============================================================================

-spec stack_entry(StackEntry) -> String when
    StackEntry :: cauder_types:stack_entry(),
    String :: string().

stack_entry({{M, F, A}, _Bs, _Es, _Var}) -> io_lib:format("~s:~s/~b", [M, F, A]);
stack_entry({Type, _Es, _Var}) -> atom_to_list(Type).

%%%=============================================================================

-spec expression(Expression) -> String when
    Expression :: cauder_types:abstract_expr(),
    String :: string().

expression(Expr) ->
    lists:flatten(erl_prettypr:format(cauder_syntax:to_abstract_expr(Expr), [{paper, 120}, {ribbon, 120}])).

%%%=============================================================================

-spec trace_action(Trace) -> String when
    Trace :: cauder_types:trace_action(),
    String :: string().

trace_action({'send', Uid, Dest, _Value}) ->
    io_lib:format("{~p,~p,~p,...}", ['send', Uid, Dest]);
trace_action({'receive', Uid}) ->
    io_lib:format("{~p,~p,...}", ['receive', Uid]);
trace_action(Action) ->
    io_lib:format("~p", [Action]).

%%%=============================================================================

-spec pid(Pid) -> String when
    Pid :: cauder_types:proc_id(),
    String :: string().

pid(Pid) -> "Proc. " ++ to_string(Pid).

-spec pp_node(Node) -> String when
    Node :: node(),
    String :: string().

pp_node(Node) -> "Node " ++ to_string(Node).

-spec to_string(Term) -> String when
    Term :: term(),
    String :: string().

to_string(Term) -> io_lib:format("~p", [Term]).

-spec pp_nodes(Nodes) -> String when
    Nodes :: [node()],
    String :: string().

pp_nodes([]) ->
    "[]";
pp_nodes(Nodes) ->
    [FirstNode | RemNodes] = lists:map(fun(Node) -> to_string(Node) end, Nodes),
    "[" ++ lists:foldl(fun(Node, AccIn) -> Node ++ ", " ++ AccIn end, FirstNode, RemNodes) ++ "]".

red(Text) -> [{?wxRED, Text}].
green(Text) -> [{?CAUDER_GREEN, Text}].
blue(Text) -> [{?wxBLUE, Text}].
