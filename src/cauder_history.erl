-module(cauder_history).

%% API
-export([
    new/0,
    push/2,
    peek/1,
    pop/1,
    is_empty/1,
    to_list/1
]).
-export([
    has_nodes/2,
    has_start/2,
    has_failed_start/2,
    has_spawn/2,
    has_send/2,
    has_receive/2
]).
-export([
    group_actions/1,
    is_concurrent/1
]).

-include("cauder_message.hrl").
-include("cauder_history.hrl").

-export_type([history/0]).

-opaque history() :: [entry()].
-type entry() ::
    entry_tau()
    | entry_self()
    | entry_node()
    | entry_nodes()
    | entry_start()
    | entry_spawn()
    | entry_send()
    | entry_receive().

-type entry_tau() :: #hist_tau{}.
-type entry_self() :: #hist_self{}.
-type entry_node() :: #hist_node{}.
-type entry_nodes() :: #hist_nodes{}.
-type entry_start() :: #hist_start{}.
-type entry_spawn() :: #hist_spawn{}.
-type entry_send() :: #hist_send{}.
-type entry_receive() :: #hist_receive{}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new() -> cauder_history:history().

new() -> [].

-spec peek(History) -> {value, Entry} | empty when
    History :: cauder_history:history(),
    Entry :: cauder_history:entry().

peek([]) -> empty;
peek([Entry | _]) -> {value, Entry}.

-spec pop(History1) -> {{value, Entry}, History2} | {empty, History1} when
    History1 :: cauder_history:history(),
    Entry :: cauder_history:entry(),
    History2 :: cauder_history:history().

pop([] = History) -> {empty, History};
pop([Entry | History]) -> {{value, Entry}, History}.

-spec push(Entry, History1) -> History2 when
    Entry :: cauder_history:entry(),
    History1 :: cauder_history:history(),
    History2 :: cauder_history:history().

push(Entry, History1) -> [Entry | History1].

-spec is_empty(History) -> boolean() when
    History :: cauder_history:history().

is_empty([]) -> true;
is_empty(_) -> false.

-spec to_list(History) -> [Entry] when
    History :: cauder_history:history(),
    Entry :: cauder_history:entry().

to_list(History) -> History.

%%%=============================================================================
%%% Utils
%%%=============================================================================

-spec has_nodes(Node, History) -> boolean() when
    Node :: node(),
    History :: cauder_history:history().

has_nodes(_, []) -> false;
has_nodes(Node, [#hist_nodes{nodes = Nodes} | H]) -> lists:member(Node, Nodes) orelse has_nodes(Node, H);
has_nodes(Node, [_ | H]) -> has_nodes(Node, H).

-spec has_start(Node, History) -> boolean() when
    Node :: node(),
    History :: cauder_history:history().

has_start(_, []) -> false;
has_start(Node, [#hist_start{node = Node, success = true} | _]) -> true;
has_start(Node, [_ | H]) -> has_start(Node, H).

-spec has_failed_start(Node, History) -> boolean() when
    Node :: node(),
    History :: cauder_history:history().

has_failed_start(_, []) -> false;
has_failed_start(Node, [#hist_start{node = Node, success = false} | _]) -> true;
has_failed_start(Node, [_ | H]) -> has_failed_start(Node, H).

-spec has_spawn(Pid, History) -> boolean() when
    Pid :: cauder_process:id(),
    History :: cauder_history:history().

has_spawn(_, []) -> false;
has_spawn(Pid, [#hist_spawn{pid = Pid} | _]) -> true;
has_spawn(Pid, [_ | H]) -> has_spawn(Pid, H).

-spec has_send(Uid, History) -> boolean() when
    Uid :: cauder_message:uid(),
    History :: cauder_history:history().

has_send(_, []) -> false;
has_send(Uid, [#hist_send{msg = #message{uid = Uid}} | _]) -> true;
has_send(Uid, [_ | H]) -> has_send(Uid, H).

-spec has_receive(Uid, History) -> boolean() when
    Uid :: cauder_message:uid(),
    History :: cauder_history:history().

has_receive(_, []) -> false;
has_receive(Uid, [#hist_receive{msg = #message{uid = Uid}} | _]) -> true;
has_receive(Uid, [_ | H]) -> has_receive(Uid, H).

%%%=============================================================================

-spec group_actions(History) -> Map when
    History :: cauder_history:history(),
    Map :: #{
        'send' := ordsets:ordset(cauder_message:uid()),
        'receive' := ordsets:ordset(cauder_message:uid()),
        'spawn' := ordsets:ordset(cauder_process:id()),
        'start' := ordsets:ordset(node())
    }.

group_actions(History) ->
    lists:foldl(
        fun
            (#hist_send{msg = #message{uid = Uid}}, Map) ->
                maps:update_with('send', fun(Uids) -> ordsets:add_element(Uid, Uids) end, Map);
            (#hist_receive{msg = #message{uid = Uid}}, Map) ->
                maps:update_with('receive', fun(Uids) -> ordsets:add_element(Uid, Uids) end, Map);
            % TODO CHeck success
            (#hist_spawn{pid = Pid}, Map) ->
                maps:update_with('spawn', fun(Pids) -> ordsets:add_element(Pid, Pids) end, Map);
            (#hist_start{node = Node, success = 'true'}, Map) ->
                maps:update_with('start', fun(Nodes) -> ordsets:add_element(Node, Nodes) end, Map);
            (_, Map1) ->
                Map1
        end,
        #{
            'send' => ordsets:new(),
            'receive' => ordsets:new(),
            'spawn' => ordsets:new(),
            'start' => ordsets:new()
        },
        History
    ).

% TODO Review
-spec is_concurrent(Entry) -> boolean() when
    Entry :: cauder_history:entry().

is_concurrent(#hist_tau{}) -> false;
is_concurrent(#hist_self{}) -> false;
is_concurrent(#hist_node{}) -> false;
is_concurrent(#hist_nodes{}) -> true;
is_concurrent(#hist_start{}) -> true;
is_concurrent(#hist_spawn{}) -> true;
is_concurrent(#hist_send{}) -> true;
is_concurrent(#hist_receive{}) -> true.
