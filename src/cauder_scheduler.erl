-module(cauder_scheduler).

%% API
-export([get/1]).

-ifdef(EUNIT).
-export([scheduler_round_robin/2, scheduler_fcfs/2]).
-endif.

-include("cauder_process.hrl").
-include("cauder_message.hrl").

-type change(Item) ::
    none
    | {init, [Item, ...]}
    | {add, Item}
    | {remove, Item}
    % {update, AddedItem, RemovedItem}
    | {update, Item, Item}.

-type scheduler_fun(Item) :: fun(
    (Queue :: queue:queue(Item), Change :: change(Item)) -> {Item, NewQueue :: queue:queue(Item)}
).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the scheduler function `SchedulerFun' associated with the given
%% `Key' if it matches an available scheduler.

-spec get(Key) -> SchedulerFun when
    Key :: atom(),
    SchedulerFun :: scheduler_fun(_).

get(Key) ->
    maps:get(Key, schedulers()).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @private
%% @doc Returns a map containing all the available schedulers.
%% The kay of this map is an atom and the value is the scheduler function, that
%% will select the next process to execute.

-spec schedulers() -> Map when
    Map :: #{Key := SchedulerFun},
    Key :: atom(),
    SchedulerFun :: scheduler_fun(_).

schedulers() ->
    #{
        ?SCHEDULER_RoundRobin => fun scheduler_round_robin/2,
        ?SCHEDULER_FCFS => fun scheduler_fcfs/2
    }.

%%%=============================================================================
%%% Scheduling implementations
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Round-robin scheduling implementation.

-spec scheduler_round_robin(Queue, Change) -> {Item, NewQueue} when
    Queue :: queue:queue(Item),
    Change :: change(Item),
    Item :: term(),
    NewQueue :: queue:queue(Item).

scheduler_round_robin(Q0, {init, [Item | Items]}) ->
    case queue:is_empty(Q0) of
        false ->
            error(not_empty);
        true ->
            Q1 = lists:foldl(fun queue:in/2, Q0, Items),
            Q2 = queue:in(Item, Q1),
            {Item, Q2}
    end;
scheduler_round_robin(Q0, none) ->
    case queue:out(Q0) of
        {empty, _} ->
            error(empty);
        {{value, Item}, Q1} ->
            Q2 = queue:in(Item, Q1),
            {Item, Q2}
    end;
scheduler_round_robin(Q0, {add, AddedItem}) ->
    case queue:out(Q0) of
        {empty, _} ->
            error(empty);
        {{value, Item}, Q1} ->
            Q2 = queue:in(AddedItem, Q1),
            Q3 = queue:in(Item, Q2),
            {Item, Q3}
    end;
scheduler_round_robin(Q0, {remove, RemovedItem}) ->
    % The removed Item must be the one at the end of the queue, the last one that was given "cpu time"
    case queue:out_r(Q0) of
        {empty, _} ->
            error(empty);
        {{value, RemovedItem}, Q1} ->
            {{value, Item}, Q2} = queue:out(Q1),
            Q3 = queue:in(Item, Q2),
            {Item, Q3};
        {{value, _}, _} ->
            error(not_tail)
    end;
scheduler_round_robin(Q0, {update, AddedItem, RemovedItem}) ->
    % The removed Item must be the one at the end of the queue, the last one that was given "cpu time"
    case queue:out_r(Q0) of
        {empty, _} ->
            error(empty);
        {{value, RemovedItem}, Q1} ->
            Q2 = queue:in(AddedItem, Q1),
            {{value, Item}, Q3} = queue:out(Q2),
            Q4 = queue:in(Item, Q3),
            {Item, Q4};
        {{value, _}, _} ->
            error(not_tail)
    end.

%%------------------------------------------------------------------------------
%% @doc First come, first served scheduling implementation.

-spec scheduler_fcfs(Queue, Change) -> {Item, NewQueue} when
    Queue :: queue:queue(Item),
    Change :: change(Item),
    Item :: term(),
    NewQueue :: queue:queue(Item).

scheduler_fcfs(Q0, {init, Ps}) ->
    case queue:is_empty(Q0) of
        false ->
            error(not_empty);
        true ->
            Q1 = lists:foldl(fun queue:in/2, Q0, Ps),
            {value, Item} = queue:peek(Q1),
            {Item, Q1}
    end;
scheduler_fcfs(Q0, none) ->
    case queue:peek(Q0) of
        empty -> error(empty);
        {value, Item} -> {Item, Q0}
    end;
scheduler_fcfs(Q0, {add, AddedItem}) ->
    case queue:peek(Q0) of
        empty ->
            error(empty);
        {value, Item} ->
            Q1 = queue:in(AddedItem, Q0),
            {Item, Q1}
    end;
scheduler_fcfs(Q0, {remove, RemovedItem}) ->
    % The removed Item must be the first one in the queue, the last one that was given "cpu time"
    case queue:out(Q0) of
        {empty, _} ->
            error(empty);
        {{value, RemovedItem}, Q1} ->
            {value, Item} = queue:peek(Q1),
            {Item, Q1};
        {{value, _}, _} ->
            error(not_head)
    end;
scheduler_fcfs(Q0, {update, AddedItem, RemovedItem}) ->
    % The removed Item must be the first one in the queue, the last one that was given "cpu time"
    case queue:out(Q0) of
        {empty, _} ->
            error(empty);
        {{value, RemovedItem}, Q1} ->
            Q2 = queue:in(AddedItem, Q1),
            {value, Item} = queue:peek(Q2),
            {Item, Q2};
        {{value, _}, _} ->
            error(not_head)
    end.
