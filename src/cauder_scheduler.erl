-module(cauder_scheduler).

%% API
-export([get/1]).

-include("cauder.hrl").

-type change() :: none |
                  {init, [cauder_types:proc_id(), ...]} |
                  {add, cauder_types:proc_id()} |
                  {remove, cauder_types:proc_id()}.

-type proc_queue() :: queue:queue(cauder_types:proc_id()).

-type scheduler_fun() :: fun((proc_queue(), change()) -> {cauder_types:proc_id(), proc_queue()}).


%%------------------------------------------------------------------------------
%% @private
%% @doc Returns a map containing all the available schedulers.
%% The kay of this map is an atom and the value is the scheduler function, that
%% will select the next process to execute.

-spec schedulers() -> Map when
  Map :: #{Key := SchedulerFun},
  Key :: atom(),
  SchedulerFun :: scheduler_fun().

schedulers() ->
  #{
    ?SCHEDULER_ROUND_ROBIN => fun scheduler_round_robin/2
  }.


%%------------------------------------------------------------------------------
%% @doc Returns the scheduler function `SchedulerFun' associated with the given
%% `Key' if it matches an available scheduler.

-spec get(Key) -> SchedulerFun when
  Key :: atom(),
  SchedulerFun :: scheduler_fun().

get(Key) ->
  maps:get(Key, schedulers()).


%%------------------------------------------------------------------------------
%% @doc Round-robin scheduling implementation.

-spec scheduler_round_robin(Queue, Change) -> {Pid, NewQueue} when
  Queue :: proc_queue(),
  Change :: change(),
  Pid :: cauder_types:proc_id(),
  NewQueue :: proc_queue().

scheduler_round_robin(Q0, none) ->
  {{value, Pid}, Q1} = queue:out(Q0),
  Q2 = queue:in(Pid, Q1),
  {Pid, Q2};
scheduler_round_robin(Q0, {init, [Pid | Ps]}) ->
  Q1 = lists:foldl(fun queue:in/2, Q0, Ps),
  Q2 = queue:in(Pid, Q1),
  {Pid, Q2};
scheduler_round_robin(Q0, {add, NewPid}) ->
  {{value, Pid}, Q1} = queue:out(Q0),
  Q2 = queue:in(NewPid, Q1),
  Q3 = queue:in(Pid, Q2),
  {Pid, Q3};
scheduler_round_robin(Q0, {remove, OldPid}) ->
  % The removed Pid must be the one at the end of the queue, the last one that was given "cpu time"
  {{value, OldPid}, Q1} = queue:out_r(Q0),
  {{value, Pid}, Q2} = queue:out(Q1),
  Q3 = queue:in(Pid, Q2),
  {Pid, Q3}.
