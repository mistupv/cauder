-module(cauder_pool).

%% API
-export([
    new/1,
    add/2,
    remove/2,
    get/2,
    find/2,
    first/1,
    is_element/2,
    update/2,
    update_with/3,
    to_list/1
]).
-export([
    find_history_spawn/2,
    find_history_start/2,
    find_on_node/2,
    find_history_failed_start/2,
    find_history_nodes/2,
    find_history_send/2,
    find_history_receive/2,
    find_variable/2
]).

-include("cauder_process.hrl").

-export_type([pool/0]).

-opaque pool() :: #{cauder_process:id() := cauder_process:process()}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new(Process) -> Pool when
    Process :: cauder_process:process(),
    Pool :: cauder_pool:pool().

new(Process) ->
    add(Process, #{}).

-spec add(Process, Pool1) -> Pool2 when
    Process :: cauder_process:process(),
    Pool1 :: cauder_pool:pool(),
    Pool2 :: cauder_pool:pool().

add(Process, Pool) ->
    maps:put(Process#process.pid, Process, Pool).

-spec remove(Id, Pool1) -> Pool2 when
    Id :: cauder_process:id(),
    Pool1 :: cauder_pool:pool(),
    Pool2 :: cauder_pool:pool().

remove(Id, Pool) ->
    maps:remove(Id, Pool).

-spec get(Id, Pool) -> Process when
    Id :: cauder_process:id(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

get(Id, Pool) ->
    maps:get(Id, Pool).

-spec find(Id, Pool) -> {ok, Process} | error when
    Id :: cauder_process:id(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find(Id, Pool) ->
    maps:find(Id, Pool).

-spec first(Pool) -> Process when
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

first(Pool) ->
    maps:get(lists:min(maps:keys(Pool)), Pool).

-spec is_element(Id, Pool) -> boolean() when
    Id :: cauder_process:id(),
    Pool :: cauder_pool:pool().

is_element(Id, Pool) ->
    maps:is_key(Id, Pool).

-spec update(Process, Pool1) -> Pool2 when
    Process :: cauder_process:process(),
    Pool1 :: cauder_pool:pool(),
    Pool2 :: cauder_pool:pool().

update(Process, Pool) ->
    maps:update(Process#process.pid, Process, Pool).

-spec update_with(Id, Fun, Pool1) -> Pool2 when
    Id :: cauder_process:id(),
    Fun :: fun((cauder_process:process()) -> cauder_process:process()),
    Pool1 :: cauder_pool:pool(),
    Pool2 :: cauder_pool:pool().

update_with(Id, Fun, Pool) ->
    maps:update_with(Id, Fun, Pool).

-spec to_list(Pool) -> [Process] when
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

to_list(Pool) -> maps:values(Pool).

%%%=============================================================================
%%% Utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks the history of each process, until it finds the process that
%% spawned the process with the given `Pid'.

-spec find_history_spawn(Pid, Pool) -> {ok, Process} | error when
    Pid :: cauder_process:id(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_spawn(Pid, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_spawn(Pid, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the history of each process, until it finds the process that
%% started the given `Node' node.

-spec find_history_start(Node, Pool) -> {ok, Process} | error when
    Node :: node(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_start(Node, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_start(Node, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Searches for the process(es) that tried to start `Node' and failed because
%% this was already part of the network, by looking at its history.

-spec find_history_failed_start(Node, Pool) -> {ok, Process} | error when
    Node :: node(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_failed_start(Node, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_failed_start(Node, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the history of each process, until it finds the process that
%% sent the message with the given `Uid'.

-spec find_history_send(Uid, Pool) -> {ok, Process} | error when
    Uid :: cauder_message:uid(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_send(Uid, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_send(Uid, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the history of each process, until it finds the process that
%% sent the message with the given `Uid'.

-spec find_history_receive(Uid, Pool) -> {ok, Process} | error when
    Uid :: cauder_message:uid(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_receive(Uid, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_receive(Uid, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Returns a list of all the processes running on the given `Node'.

-spec find_on_node(Node, Pool) -> [Process] when
    Node :: node(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_on_node(Node, Pool) ->
    lists:filter(
        fun(P) -> P#process.node =:= Node end,
        maps:values(Pool)
    ).

%%------------------------------------------------------------------------------
%% @doc Searches for process(es) that have performed a read of `Node' by means
%% of the function 'nodes()'

-spec find_history_nodes(Node, Pool) -> {ok, Process} | error when
    Node :: node(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_history_nodes(Node, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_history:has_nodes(Node, P#process.hist) end,
            maps:values(Pool)
        )
    ).

%%------------------------------------------------------------------------------
%% @doc Searches for the process that defined the variable with the given name,
%% by looking at its history.

-spec find_variable(Name, Pool) -> {ok, Process} | error when
    Name :: atom(),
    Pool :: cauder_pool:pool(),
    Process :: cauder_process:process().

find_variable(Name, Pool) ->
    value_to_ok(
        lists:search(
            fun(P) -> cauder_bindings:is_bound(Name, P#process.env) end,
            maps:values(Pool)
        )
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec value_to_ok
    ({value, Value}) -> {ok, Value};
    (false) -> error.

value_to_ok({value, Value}) -> {ok, Value};
value_to_ok(false) -> error.
