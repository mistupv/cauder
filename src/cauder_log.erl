-module(cauder_log).

%% API
-export([
    new/0,
    get/2,
    peek/2,
    pop_nodes/2,
    pop_start/2,
    pop_spawn/2,
    pop_send/2,
    pop_receive/2,
    pop_reg/2,
    pop_del/2,
    pop_read/2,
    push/3,
    is_element/2,
    is_empty/1,
    pids/1,
    from_list/1
]).
-export([
    rdep/2,
    group_actions/1
]).
-export([
    %has_nodes/3,
    %has_start/3,
    has_spawn/3,
    has_send/3,
    has_receive/3,
    has_register/3,
    has_delete/3,
    has_registered/3,
    has_fail_read/4,
    has_success_read/3
]).
-export([
    find_nodes/2,
    find_start/2,
    find_spawn/2,
    find_spawn_action/2,
    find_failed_spawns/2,
    find_send/2,
    find_receive/2,
    find_register/2,
    find_delete/2,
    find_registered/2,
    find_fail_read/3,
    find_success_read/2
]).

-include("cauder_log.hrl").

-export_type([log/0]).

-opaque log() :: #{cauder_process:id() => [cauder_log:action(), ...]}.
-type action() ::
    action_spawn()
    | action_send()
    | action_receive()
    | action_nodes()
    | action_start()
    | action_reg()
    | action_del()
    | action_read().

-type action_spawn() :: #log_spawn{}.
-type action_send() :: #log_send{} | #log_sendA{}.
-type action_receive() :: #log_receive{}.
-type action_nodes() :: #log_nodes{}.
-type action_start() :: #log_start{}.
-type action_reg() :: #log_reg{}.
-type action_del() :: #log_del{}.
-type action_read() :: #log_read{}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new() -> cauder_log:log().

new() -> maps:new().

-spec get(Pid, Log) -> [Action] when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action().

get(Pid, Log) ->
    maps:get(Pid, Log, []).

-spec peek(Pid, Log) -> {value, Action} | empty | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action().

peek(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [Action | _]} ->
            {value, Action};
        {ok, []} ->
            empty;
        error ->
            error
    end.

-spec pop_spawn(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_spawn(),
    NewLog :: cauder_log:log().

pop_spawn(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_spawn{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_send(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_send(),
    NewLog :: cauder_log:log().

pop_send(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_send{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        {ok, [#log_sendA{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_receive(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_receive(),
    NewLog :: cauder_log:log().

pop_receive(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_receive{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_nodes(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_nodes(),
    NewLog :: cauder_log:log().

pop_nodes(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_nodes{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_start(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_start(),
    NewLog :: cauder_log:log().

pop_start(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_start{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_reg(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_reg(),
    NewLog :: cauder_log:log().

pop_reg(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_reg{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_del(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_del(),
    NewLog :: cauder_log:log().

pop_del(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_del{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec pop_read(Pid, Log) -> {Action, NewLog} | error when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Action :: cauder_log:action_read(),
    NewLog :: cauder_log:log().

pop_read(Pid, Log) ->
    case maps:find(Pid, Log) of
        {ok, [#log_read{} = Action | Actions]} ->
            {Action, update_or_remove(Pid, Actions, Log)};
        _ ->
            error
    end.

-spec push(Pid, Action, Log) -> NewLog when
    Pid :: cauder_process:id(),
    Action :: cauder_log:action(),
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

push(Pid, Action, Log) ->
    maps:update_with(Pid, fun(Actions) -> [Action | Actions] end, [Action], Log).

-spec is_element(Pid, Log) -> boolean() when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log().

is_element(Pid, Log) ->
    maps:is_key(Pid, Log).

-spec is_empty(Log) -> boolean() when
    Log :: cauder_log:log().

is_empty(Log) ->
    maps:size(Log) =:= 0.

-spec pids(Log) -> [Pid] when
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

pids(Log) ->
    maps:keys(Log).

-spec from_list(List) -> Log when
    List :: [{Pid, [Action]}],
    Pid :: cauder_process:id(),
    Action :: cauder_log:action(),
    Log :: cauder_log:log().

from_list(List) ->
    maps:from_list(List).

%%%=============================================================================
%%% Utils
%%%=============================================================================

-spec rdep(Pid, Log) -> NewLog when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

rdep(Pid, Log) -> remove_dependents_spawn(Pid, Log).

-spec group_actions(Log) -> Map when
    Log :: cauder_log:log(),
    Map :: #{
        'send' := ordsets:ordset(cauder_message:uid()),
        'receive' := ordsets:ordset(cauder_message:uid()),
        'spawn' := ordsets:ordset(cauder_process:id()),
        'start' := ordsets:ordset(node()),
        'register' := ordsets:ordset(cauder_map:map_element()),
        'delete' := ordsets:ordset(cauder_map:map_element())
    }.

group_actions(Log) ->
    maps:fold(
        fun(_, Actions, Map0) ->
            lists:foldl(
                fun
                    (#log_send{uid = Uid}, Map1) ->
                        maps:update_with('send', fun(Uids) -> ordsets:add_element(Uid, Uids) end, Map1);
                    (#log_sendA{uid = Uid}, Map1) ->
                        maps:update_with('send', fun(Uids) -> ordsets:add_element(Uid, Uids) end, Map1);
                    (#log_receive{uid = Uid}, Map1) ->
                        maps:update_with('receive', fun(Uids) -> ordsets:add_element(Uid, Uids) end, Map1);
                    (#log_spawn{pid = Pid, success = 'true'}, Map1) ->
                        maps:update_with('spawn', fun(Pids) -> ordsets:add_element(Pid, Pids) end, Map1);
                    (#log_start{node = Node, success = 'true'}, Map1) ->
                        maps:update_with('start', fun(Nodes) -> ordsets:add_element(Node, Nodes) end, Map1);
                    (#log_reg{key = K, atom = A, pid = P}, Map1) ->
                        maps:update_with('register', fun(Els) -> ordsets:add_element({A, P, K, top}, Els) end, Map1);
                    (#log_del{key = K, atom = A, pid = P}, Map1) ->
                        maps:update_with('delete', fun(Els) -> ordsets:add_element({A, P, K, bot}, Els) end, Map1);
                    (_, Map1) ->
                        Map1
                end,
                Map0,
                Actions
            )
        end,
        #{
            'send' => ordsets:new(),
            'receive' => ordsets:new(),
            'spawn' => ordsets:new(),
            'start' => ordsets:new(),
            'register' => ordsets:new(),
            'delete' => ordsets:new()
        },
        Log
    ).

%%%=============================================================================

-spec has_spawn(Pid, ChildPid, Log) -> boolean() when
    Pid :: cauder_process:id(),
    ChildPid :: cauder_process:id(),
    Log :: cauder_log:log().

has_spawn(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_spawn(Pid, ChildPid, Log) ->
    lists:any(
        fun
            (#log_spawn{pid = ChildPid1}) -> ChildPid1 =:= ChildPid;
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_send(Pid, Uid, Log) -> boolean() when
    Pid :: cauder_process:id(),
    Uid :: cauder_message:uid(),
    Log :: cauder_log:log().

has_send(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_send(Pid, Uid, Log) ->
    lists:any(
        fun
            (#log_send{uid = Uid1}) -> Uid1 =:= Uid;
            (#log_sendA{uid = Uid1}) -> Uid1 =:= Uid;
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_receive(Pid, Uid, Log) -> boolean() when
    Pid :: cauder_process:id(),
    Uid :: cauder_message:uid(),
    Log :: cauder_log:log().

has_receive(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_receive(Pid, Uid, Log) ->
    lists:any(
        fun
            (#log_receive{uid = Uid1}) -> Uid1 =:= Uid;
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_register(Pid, El, Log) -> boolean() when
    Pid :: cauder_process:id(),
    El :: cauder_map:map_element(),
    Log :: cauder_log:log().

has_register(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_register(Pid, {_, _, Key, _}, Log) ->
    lists:any(
        fun
            (#log_reg{key = Key1}) -> Key1 =:= Key;
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_delete(Pid, El, Log) -> boolean() when
    Pid :: cauder_process:id(),
    El :: cauder_map:map_element(),
    Log :: cauder_log:log().

has_delete(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_delete(Pid, {_, _, Key, _}, Log) ->
    lists:any(
        fun
            (#log_del{key = Key1}) -> Key1 =:= Key;
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_registered(Pid, Map, Log) -> boolean() when
    Pid :: cauder_process:id(),
    Map :: [cauder_map:map_element()],
    Log :: cauder_log:log().

has_registered(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_registered(Pid, Map, Log) ->
    lists:any(
        fun
            (#log_read{atom = undefined, pid = undefined, map = LogMap}) ->
                ((LogMap -- Map == []) and (Map -- LogMap == []));
            (_) ->
                'false'
        end,
        maps:get(Pid, Log)
    ).

-spec has_fail_read(Pid, Map, El, Log) -> boolean() when
    Pid :: cauder_process:id(),
    Map :: [cauder_map:map_element()],
    El :: cauder_map:map_element(),
    Log :: cauder_log:log().

has_fail_read(Pid, _, _, Log) when not is_map_key(Pid, Log) -> false;
has_fail_read(Pid, Map, {A1, P1, _, _}, Log) ->
    lists:any(
        fun
            (#log_read{atom = A, pid = P, map = LogMap}) -> (((A =:= A1) or (P =:= P1)) and (LogMap -- Map == []));
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

-spec has_success_read(Pid, El, Log) -> boolean() when
    Pid :: cauder_process:id(),
    El :: cauder_map:map_element(),
    Log :: cauder_log:log().

has_success_read(Pid, _, Log) when not is_map_key(Pid, Log) -> false;
has_success_read(Pid, El, Log) ->
    lists:any(
        fun
            (#log_read{map = Map}) -> cauder_map:is_in_map(Map, El);
            (_) -> false
        end,
        maps:get(Pid, Log)
    ).

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds all the processes that
%% called `erlang:nodes()' while the given `Node' was alive.

-spec find_nodes(Node, Log) -> [Pid] when
    Node :: node(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_nodes(Node, Log) ->
    Log1 = maps:filter(
        fun(_Pid, Actions) ->
            lists:any(
                fun
                    (#log_nodes{nodes = Nodes}) -> not lists:member(Node, Nodes);
                    (_) -> 'false'
                end,
                Actions
            )
        end,
        Log
    ),
    maps:keys(Log1).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that started
%% the given `Node'.

-spec find_start(Node, Log) -> {ok, Pid} | error when
    Node :: node(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_start(Node, Log) ->
    find_pid(
        fun
            (#log_start{node = Node1, success = 'true'}) -> Node =:= Node1;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that spawned
%% the process with the given `Pid'.

-spec find_spawn(ChildPid, Log) -> {ok, Pid} | error when
    ChildPid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_spawn(ChildPid, Log) ->
    find_pid(
        fun
            (#log_spawn{pid = Pid}) -> Pid =:= ChildPid;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that spawned
%% the process with the given `Pid'.

-spec find_spawn_action(ChildPid, Log) -> {ok, {Pid, ChildNode}} | error when
    ChildPid :: cauder_process:id(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id(),
    ChildNode :: node().

find_spawn_action(ChildPid, Log) ->
    I0 = maps:iterator(Log),
    FindAction = fun
        (#log_spawn{pid = Pid, success = 'true'}) -> Pid =:= ChildPid;
        (_) -> 'false'
    end,
    Fun = fun Search(I) ->
        case maps:next(I) of
            'none' ->
                error;
            {Pid, Actions, I1} ->
                case lists:search(FindAction, Actions) of
                    {value, #log_spawn{node = Node}} -> {ok, {Pid, Node}};
                    false -> Search(I1)
                end
        end
    end,
    Fun(I0).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that failed
%% to spawn the process with the given `Pid'.

-spec find_failed_spawns(Node, Log) -> [Pid] when
    Node :: node(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_failed_spawns(Node, Log) ->
    Log1 = maps:filter(
        fun(_Pid, Actions) ->
            lists:any(
                fun
                    (#log_spawn{node = Node1, success = 'false'}) -> Node =:= Node1;
                    (_) -> 'false'
                end,
                Actions
            )
        end,
        Log
    ),
    maps:keys(Log1).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that sent the
%% message with the given `Uid'.

-spec find_send(Uid, Log) -> {ok, Pid} | error when
    Uid :: cauder_message:uid(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_send(Uid, Log) ->
    find_pid(
        fun
            (#log_send{uid = Uid1}) -> Uid =:= Uid1;
            (#log_sendA{uid = Uid1}) -> Uid =:= Uid1;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that received
%% the message with the given `Uid'.

-spec find_receive(Uid, Log) -> {ok, Pid} | error when
    Uid :: cauder_message:uid(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_receive(Uid, Log) ->
    find_pid(
        fun
            (#log_receive{uid = Uid1}) -> Uid =:= Uid1;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that delete
%% the message with the given `Uid'.

-spec find_delete(El, Log) -> {ok, Pid} | error when
    El :: cauder_map:map_element(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_delete({_, _, Key, _}, Log) ->
    find_pid(
        fun
            (#log_del{key = Key1}) -> Key =:= Key1;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that registered
%% the message with the given `Uid'.

-spec find_register(El, Log) -> {ok, Pid} | error when
    El :: cauder_map:map_element(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_register({_, _, Key, _}, Log) ->
    find_pid(
        fun
            (#log_reg{key = Key1}) -> Key =:= Key1;
            (_) -> 'false'
        end,
        Log
    ).

%%------------------------------------------------------------------------------
%% @doc Checks the log of each process, until it finds the process that registered
%% the message with the given `Uid'.

-spec find_registered(Map, Log) -> {ok, Pid} | error when
    Map :: [cauder_map:map_element()],
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_registered(Map, Log) ->
    find_pid(
        fun
            (#log_read{atom = undefined, pid = undefined, map = LogMap}) ->
                ((LogMap -- Map == []) and (Map -- LogMap == []));
            (_) ->
                'false'
        end,
        Log
    ).

-spec find_fail_read(El, Map, Log) -> {ok, Pid} | error when
    El :: cauder_map:map_element(),
    Map :: [cauder_map:map_element()],
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_fail_read({A1, P1, _, _}, Map, Log) ->
    find_pid(
        fun
            (#log_read{atom = A, pid = P, map = LogMap}) -> (((A =:= A1) or (P =:= P1)) and (LogMap -- Map == []));
            (_) -> 'false'
        end,
        Log
    ).

-spec find_success_read(El, Log) -> {ok, Pid} | error when
    El :: cauder_map:map_element(),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id().

find_success_read(El, Log) ->
    find_pid(
        fun
            (#log_read{map = Map}) -> cauder_map:is_in_map(Map, El);
            (#log_sendA{el = El1}) -> El1 == El;
            (_) -> 'false'
        end,
        Log
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec update_or_remove(Pid, Actions, Log) -> NewLog when
    Pid :: cauder_process:id(),
    Actions :: [cauder_log:action()],
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

update_or_remove(Pid, [], Log) ->
    maps:remove(Pid, Log);
update_or_remove(Pid, Actions, Log) ->
    maps:update(Pid, Actions, Log).

-spec remove_dependents_spawn(Pid, Log) -> NewLog when
    Pid :: cauder_process:id(),
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

remove_dependents_spawn(Pid, Log) when is_map_key(Pid, Log) ->
    lists:foldl(
        fun remove_dependents/2,
        maps:remove(Pid, Log),
        maps:get(Pid, Log)
    );
remove_dependents_spawn(_Pid, Log) ->
    Log.

-spec remove_dependents_receive(Uid, Log) -> NewLog when
    Uid :: cauder_message:uid(),
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

remove_dependents_receive(Uid, Log) ->
    Fun = fun Next(I0) ->
        case maps:next(I0) of
            {Pid, Actions, I1} ->
                Split = lists:splitwith(
                    fun(Action) ->
                        case Action of
                            #log_receive{uid = Uid} -> false;
                            _ -> true
                        end
                    end,
                    Actions
                ),
                case Split of
                    {_, []} ->
                        Next(I1);
                    {[], Dependent} ->
                        lists:foldl(
                            fun remove_dependents/2,
                            maps:remove(Pid, Log),
                            Dependent
                        );
                    {Independent, Dependent} ->
                        lists:foldl(
                            fun remove_dependents/2,
                            maps:put(Pid, Independent, Log),
                            Dependent
                        )
                end;
            'none' ->
                Log
        end
    end,
    I = maps:iterator(Log),
    Fun(I).

-spec remove_dependents(Action, Log) -> NewLog when
    Action :: cauder_log:action(),
    Log :: cauder_log:log(),
    NewLog :: cauder_log:log().

remove_dependents(#log_spawn{pid = Pid}, Log) -> remove_dependents_spawn(Pid, Log);
remove_dependents(#log_send{uid = Uid}, Log) -> remove_dependents_receive(Uid, Log);
remove_dependents(#log_sendA{uid = Uid}, Log) -> remove_dependents_receive(Uid, Log);
remove_dependents(_, Log) -> Log.

%%%=============================================================================

-spec find_pid(Pred, Log) -> {ok, Pid} | error when
    Pred :: fun((Action) -> boolean()),
    Log :: cauder_log:log(),
    Pid :: cauder_process:id(),
    Action :: cauder_log:action().

find_pid(Pred, Log) ->
    I0 = maps:iterator(Log),
    Fun = fun Search(I) ->
        case maps:next(I) of
            'none' ->
                error;
            {Pid, Actions, I1} ->
                case lists:any(Pred, Actions) of
                    true -> {ok, Pid};
                    false -> Search(I1)
                end
        end
    end,
    Fun(I0).
