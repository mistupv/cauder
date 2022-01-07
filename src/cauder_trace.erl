-module(cauder_trace).

%% API
-export([
    new/0,
    push/3,
    pop/3,
    get/2,
    peek/2
]).
-export([
    reverse/1,
    map_pids/1,
    to_log/1
]).

-ignore_xref([peek/2]).

-include("cauder_trace.hrl").
-include("cauder_log.hrl").

-export_type([trace/0]).

-opaque trace() :: #{cauder_process:id() => [cauder_trace:action(), ...]}.
-type action() ::
    action_nodes()
    | action_start()
    | action_spawn()
    | action_send()
    | action_receive().

-type action_start() :: #trace_start{}.
-type action_nodes() :: #trace_nodes{}.
-type action_spawn() :: #trace_spawn{}.
-type action_send() :: #trace_send{}.
-type action_receive() :: #trace_receive{}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new() -> cauder_trace:trace().

new() -> maps:new().

-spec push(Pid, Action, Trace) -> NewTrace when
    Pid :: cauder_process:id(),
    Action :: cauder_trace:action(),
    Trace :: cauder_trace:trace(),
    NewTrace :: cauder_trace:trace().

push(Pid, Action, Trace) ->
    maps:update_with(Pid, fun(Actions) -> [Action | Actions] end, [Action], Trace).

-spec pop(Pid, Action, Trace) -> NewTrace when
    Pid :: cauder_process:id(),
    Action :: cauder_trace:action(),
    Trace :: cauder_trace:trace(),
    NewTrace :: cauder_trace:trace().

pop(Pid, Action, Trace) ->
    case maps:get(Pid, Trace) of
        [Action] ->
            maps:remove(Pid, Trace);
        [Action | Actions] ->
            maps:update(Pid, Actions, Trace)
    end.

-spec get(Pid, Trace) -> [Action] when
    Pid :: cauder_process:id(),
    Trace :: cauder_trace:trace(),
    Action :: cauder_trace:action().

get(Pid, Trace) ->
    maps:get(Pid, Trace, []).

-spec peek(Pid, Trace) -> {value, Action} | empty | error when
    Pid :: cauder_process:id(),
    Trace :: cauder_trace:trace(),
    Action :: cauder_trace:action().

peek(Pid, Trace) ->
    case maps:find(Pid, Trace) of
        {ok, [Action | _]} ->
            {value, Action};
        {ok, []} ->
            empty;
        error ->
            error
    end.

%%%=============================================================================

-spec reverse(Trace) -> NewTrace when
    Trace :: cauder_trace:trace(),
    NewTrace :: cauder_trace:trace().

reverse(Trace) ->
    maps:map(fun(_, Actions) -> lists:reverse(map_pids(Actions)) end, Trace).

-spec map_pids(term()) -> term().

map_pids(List) when is_list(List) ->
    lists:map(fun map_pids/1, List);
map_pids(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(lists:map(fun map_pids/1, tuple_to_list(Tuple)));
map_pids(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = map_pids(K),
            V1 = map_pids(V),
            false = maps:is_key(K1, Acc),
            Acc#{K1 => V1}
        end,
        #{},
        Map
    );
map_pids(Pid) when is_pid(Pid) ->
    cauder_process:from_pid(Pid);
map_pids(Term) ->
    Term.

-spec to_log(Trace) -> Log when
    Trace :: cauder_trace:trace(),
    Log :: cauder_log:log().

to_log(Trace) ->
    FilteredTrace = maps:map(fun(_, As) -> lists:filtermap(fun filtermap_action/1, As) end, Trace),
    List = maps:to_list(FilteredTrace),
    cauder_log:from_list(List).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec filtermap_action(TraceAction) -> {true, LogAction} | false when
    TraceAction :: cauder_trace:action(),
    LogAction :: cauder_log:action().

filtermap_action(#trace_nodes{nodes = Nodes}) ->
    {true, #log_nodes{nodes = Nodes}};
filtermap_action(#trace_start{node = Node, success = Success}) ->
    {true, #log_start{node = Node, success = Success}};
filtermap_action(#trace_spawn{node = Node, pid = Pid, success = Success}) ->
    {true, #log_spawn{node = Node, pid = Pid, success = Success}};
filtermap_action(#trace_send{uid = Uid}) ->
    {true, #log_send{uid = Uid}};
filtermap_action(#trace_receive{uid = Uid}) ->
    {true, #log_receive{uid = Uid}}.
