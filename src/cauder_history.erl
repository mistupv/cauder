-module(cauder_history).

%% API
-export([new/0, peek/1, pop/1, push/2, is_empty/1, to_list/1]).
-export([has_nodes/2, has_spawn/2, has_start/2, has_failed_start/2, has_send/2, has_receive/2]).

-include("cauder.hrl").
-include("cauder_message.hrl").
-include("cauder_history.hrl").

-export_type([history/0]).

-opaque history() :: [entry()].
-type entry() ::
    entry_tau()
    | entry_self()
    | entry_node()
    | entry_nodes()
    | entry_spawn()
    | entry_start()
    | entry_send()
    | entry_rec().

-type entry_tau() :: #h_tau{}.
-type entry_self() :: #h_self{}.
-type entry_node() :: #h_node{}.
-type entry_nodes() :: #h_nodes{}.
-type entry_spawn() :: #h_spawn{}.
-type entry_start() :: #h_start{}.
-type entry_send() :: #h_send{}.
-type entry_rec() :: #h_receive{}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new() -> cauder_history:history().

new() -> [].

-spec peek(History) -> {value, Entry} | empty when
    History :: cauder_history:history(),
    Entry :: cauder_history:entry().

peek([Entry | _]) -> Entry.

-spec pop(History1) -> {{value, Entry}, History2} | {empty, History1} when
    History1 :: cauder_history:history(),
    Entry :: cauder_history:entry(),
    History2 :: cauder_history:history().

pop([Entry | History]) -> {Entry, History}.

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
has_nodes(Node, [#h_nodes{nodes = Nodes} | H]) -> lists:member(Node, Nodes) orelse has_nodes(Node, H);
has_nodes(Node, [_ | H]) -> has_nodes(Node, H).

-spec has_spawn(Pid, History) -> boolean() when
    Pid :: cauder_process:id(),
    History :: cauder_history:history().

has_spawn(_, []) -> false;
has_spawn(Pid, [#h_spawn{pid = Pid} | _]) -> true;
has_spawn(Pid, [_ | H]) -> has_spawn(Pid, H).

-spec has_start(Node, History) -> boolean() when
    Node :: node(),
    History :: cauder_history:history().

has_start(_, []) -> false;
has_start(Node, [#h_start{node = Node, success = true} | _]) -> true;
has_start(Node, [_ | H]) -> has_start(Node, H).

-spec has_failed_start(Node, History) -> boolean() when
    Node :: node(),
    History :: cauder_history:history().

has_failed_start(_, []) -> false;
has_failed_start(Node, [#h_start{node = Node, success = false} | _]) -> true;
has_failed_start(Node, [_ | H]) -> has_failed_start(Node, H).

-spec has_send(Uid, History) -> boolean() when
    Uid :: cauder_message:uid(),
    History :: cauder_history:history().

has_send(_, []) -> false;
has_send(Uid, [#h_send{msg = #message{uid = Uid}} | _]) -> true;
has_send(Uid, [_ | H]) -> has_send(Uid, H).

-spec has_receive(Uid, History) -> boolean() when
    Uid :: cauder_message:uid(),
    History :: cauder_history:history().

has_receive(_, []) -> false;
has_receive(Uid, [#h_receive{msg = #message{uid = Uid}} | _]) -> true;
has_receive(Uid, [_ | H]) -> has_receive(Uid, H).
