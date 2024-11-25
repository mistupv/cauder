%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for CauDEr systems.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_pp).

-export([process/1, log_action/1, history_entry/1, stack_entry/1, expression/1, trace_action/1]).
-export([pid/1, pp_node/1, to_string/1]).

-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_stack.hrl").
-include("cauder_history.hrl").
-include("cauder_log.hrl").
-include("cauder_trace.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-spec process(Process) -> nonempty_string() when
    Process :: cauder_process:process().

process(#process{node = Node, pid = Pid, mfa = {M, F, A}} = Proc) ->
    Icon =
        case cauder_process:is_alive(Proc) of
            true -> ?ICON_ALIVE;
            false -> ?ICON_DEAD
        end,
    lists:flatten(Icon ++ io_lib:format("Node: ~p  PID: ~p - ~s:~s/~p", [Node, Pid, M, F, A])).

%%%=============================================================================

-spec log_action(Action) -> [char() | {{integer(), integer(), integer()}, [char()]}] when
    Action :: cauder_log:action().

log_action(#log_nodes{nodes = Nodes}) ->
    "nodes(" ++ to_string(Nodes) ++ ")";
log_action(#log_start{node = Node, success = 'true'}) ->
    "start(" ++ green(to_string(Node)) ++ ")";
log_action(#log_start{node = Node, success = 'false'}) ->
    "start(" ++ red(to_string(Node)) ++ ")";
log_action(#log_spawn{node = Node, pid = Pid, success = 'true'}) ->
    "spawn(" ++ to_string(Node) ++ ", " ++ green(to_string(Pid)) ++ ")";
log_action(#log_spawn{node = Node, pid = Pid, success = 'false'}) ->
    "spawn(" ++ to_string(Node) ++ ", " ++ red(to_string(Pid)) ++ ")";
log_action(#log_send{uid = Uid}) ->
    "send(" ++ blue(to_string(Uid)) ++ ")";
log_action(#log_receive{uid = Uid}) ->
    "receive(" ++ blue(to_string(Uid)) ++ ")";
log_action(#log_reg{key = K}) ->
    "register {" ++ blue(to_string(K)) ++ "}";
log_action(#log_del{key = K}) ->
    "delete {" ++ blue(to_string(K)) ++ "}";
log_action(#log_read{}) ->
    "read";
log_action(#log_sendA{uid = Uid, el = {A, _, _, _}}) ->
    "send(" ++ blue(to_string(Uid)) ++ ") with atom " ++ blue(to_string(A)).

%%%=============================================================================

-spec trace_action(Action) -> nonempty_string() when
    Action :: cauder_trace:action().

trace_action(#trace_nodes{nodes = Nodes}) ->
    io_lib:format("nodes(~p)", [Nodes]);
trace_action(#trace_start{node = Node, success = _}) ->
    io_lib:format("start(~p)", [Node]);
trace_action(#trace_spawn{node = Node, pid = Pid, success = _}) ->
    io_lib:format("spawn(~p, ~p)", [Node, Pid]);
trace_action(#trace_send{uid = Uid}) ->
    io_lib:format("send(~p)", [Uid]);
trace_action(#trace_receive{uid = Uid}) ->
    io_lib:format("receive(~p)", [Uid]).

%%%=============================================================================

-spec history_entry(Entry) -> nonempty_string() when
    Entry :: cauder_history:entry().

history_entry(#hist_tau{}) ->
    "local";
history_entry(#hist_self{}) ->
    "self";
history_entry(#hist_registered{}) ->
    "registered";
history_entry(#hist_readS{mapEl = Map}) ->
    "read of " ++ to_string(Map);
history_entry(#hist_readF{atom = Atom}) ->
    "try to read " ++ to_string(Atom);
history_entry(#hist_regS{mapEl = {A, P, K, _}}) ->
    "register {" ++ to_string(A) ++ ", " ++ to_string(P) ++ ", " ++ to_string(K) ++ "}";
history_entry(#hist_del{mapEl = El}) ->
    "delete " ++ to_string(El);
history_entry(#hist_sendA{mapEl = {A, _, _, _}, msg = #message{uid = Uid, val = Val}}) ->
    "send with atom " ++ to_string(A) ++ " (" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")";
history_entry(#hist_nodes{nodes = Nodes}) ->
    "nodes(" ++ pp_nodes(Nodes) ++ ")";
history_entry(#hist_start{node = Node, success = true}) ->
    "start(" ++ green(to_string(Node)) ++ ")";
history_entry(#hist_start{node = Node, success = false}) ->
    "start(" ++ red(to_string(Node)) ++ ")";
history_entry(#hist_spawn{node = Node, pid = Pid}) ->
    "spawn(" ++ to_string(Node) ++ ", " ++ green(to_string(Pid)) ++ ")";
history_entry(#hist_send{msg = #message{uid = Uid, val = Val}}) ->
    "send(" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")";
history_entry(#hist_receive{msg = #message{uid = Uid, val = Val}}) ->
    "receive(" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")".

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
