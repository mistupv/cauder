-module(cauder_history).

%% API
-export([new/0, peek/1, pop/1, push/2, is_empty/1, to_list/1]).

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
    History1 :: cauder_history:history(),
    Entry :: cauder_history:entry(),
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
