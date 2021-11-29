%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for CauDEr systems.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_pp).

-export([process/1, log_action/1, history_entry/1, stack_entry/1, expression/1, trace_entry/1]).
-export([pid/1, pp_node/1, to_string/1]).

-include("cauder.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_stack.hrl").
-include("cauder_history.hrl").
-include("cauder_log.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-spec process(Process) -> nonempty_string() when
    Process :: cauder_process:process().

process(#process{node = Node, pid = Pid, mfa = {M, F, A}} = Proc) ->
    Icon =
        case cauder_utils:is_dead(Proc) of
            true -> ?ICON_DEAD;
            false -> ?ICON_ALIVE
        end,
    lists:flatten(Icon ++ io_lib:format("Node: ~p  PID: ~p - ~s:~s/~p", [Node, Pid, M, F, A])).

%%%=============================================================================

-spec log_action(Action) -> nonempty_string() when
    Action :: cauder_log:action().

log_action(#log_spawn{node = Node, pid = Pid, success = 'true'}) ->
    "spawn(" ++ green(to_string(Node) ++ ", " ++ to_string(Pid)) ++ ")";
log_action(#log_spawn{node = Node, pid = Pid, success = 'false'}) ->
    "spawn(" ++ red(to_string(Node) ++ ", " ++ to_string(Pid)) ++ ")";
log_action(#log_send{uid = Uid}) ->
    "send(" ++ red(to_string(Uid)) ++ ")";
log_action(#log_receive{uid = Uid}) ->
    "rec(" ++ blue(to_string(Uid)) ++ ")";
log_action(#log_nodes{nodes = Nodes}) ->
    "nodes(" ++ to_string(Nodes) ++ ")";
log_action(#log_start{node = Node, success = 'true'}) ->
    "start(" ++ green(to_string(Node)) ++ ")";
log_action(#log_start{node = Node, success = 'false'}) ->
    "start(" ++ red(to_string(Node)) ++ ")".

%%%=============================================================================

-spec history_entry(Entry) -> nonempty_string() when
    Entry :: cauder_history:entry().

history_entry(#hist_tau{}) ->
    "seq";
history_entry(#hist_self{}) ->
    "self";
history_entry(#hist_nodes{nodes = Nodes}) ->
    "nodes(" ++ pp_nodes(Nodes) ++ ")";
history_entry(#hist_spawn{node = Node, pid = Pid}) ->
    "spawn(" ++ to_string(Node) ++ ", " ++ to_string(Pid) ++ ")";
history_entry(#hist_start{node = Node, success = true}) ->
    "start(" ++ green(to_string(Node)) ++ ")";
history_entry(#hist_start{node = Node, success = false}) ->
    "start(" ++ red(to_string(Node)) ++ ")";
history_entry(#hist_send{msg = #message{uid = Uid, val = Val}}) ->
    "send(" ++ to_string(Val) ++ "," ++ red(to_string(Uid)) ++ ")";
history_entry(#hist_receive{msg = #message{uid = Uid, val = Val}}) ->
    "rec(" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")".

%%%=============================================================================

-spec stack_entry(Entry) -> nonempty_string() when
    Entry :: cauder_stack:entry().

stack_entry(#s_function{mfa = {M, F, A}}) ->
    io_lib:format("~s:~s/~b", [M, F, A]);
stack_entry(#s_block{type = Type}) ->
    atom_to_list(Type).

%%%=============================================================================

-spec expression(Expression) -> nonempty_string() when
    Expression :: cauder_syntax:abstract_expr().

expression(Expr) ->
    lists:flatten(erl_prettypr:format(cauder_syntax:to_abstract_expr(Expr), [{paper, 120}, {ribbon, 120}])).

%%%=============================================================================

-spec trace_entry(Trace) -> nonempty_string() when
    Trace :: cauder_types:x_trace().

trace_entry(#x_trace{type = ?RULE_SEND, from = From, to = To, val = Val, time = Uid}) ->
    io_lib:format("~s send ~p to ~s (~p)", [pid(From), Val, pid(To), Uid]);
trace_entry(#x_trace{type = ?RULE_SPAWN, from = From, to = To}) ->
    io_lib:format("~s spawns ~s", [pid(From), pid(To)]);
trace_entry(#x_trace{type = ?RULE_START, from = From, res = success, node = Node}) ->
    io_lib:format("~s starts ~s", [pid(From), Node]);
trace_entry(#x_trace{type = ?RULE_START, from = From, res = failure, node = Node}) ->
    io_lib:format("Warning: ~s tried to start ~s and failed", [pid(From), Node]);
trace_entry(#x_trace{type = ?RULE_RECEIVE, from = From, val = Val, time = Uid}) ->
    io_lib:format("~s receives ~p (~p)", [pid(From), Val, Uid]).

%%%=============================================================================

-spec pid(Pid) -> nonempty_string() when
    Pid :: cauder_process:id().

pid(Pid) -> "Proc. " ++ to_string(Pid).

-spec pp_node(Node) -> nonempty_string() when
    Node :: node().

pp_node(Node) -> "Node " ++ to_string(Node).

-spec pp_nodes(Nodes) -> nonempty_string() when
    Nodes :: [node()].

pp_nodes([]) ->
    "[]";
pp_nodes(Nodes) ->
    [FirstNode | RemNodes] = lists:map(fun(Node) -> to_string(Node) end, Nodes),
    "[" ++ lists:foldl(fun(Node, AccIn) -> Node ++ ", " ++ AccIn end, FirstNode, RemNodes) ++ "]".

%%%=============================================================================

-spec to_string(term()) -> nonempty_string().

to_string(Term) -> io_lib:format("~p", [Term]).

%%%=============================================================================

red(Text) -> [{?wxRED, Text}].
green(Text) -> [{?CAUDER_GREEN, Text}].
blue(Text) -> [{?wxBLUE, Text}].
