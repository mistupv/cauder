-module(cauder_scheduler).

%% API
-export([get/1]).

-include("cauder.hrl").

-type list_pid() :: [cauder_types:proc_id()].
-type nonempty_list_pid() :: [cauder_types:proc_id(), ...].
-type step_fun() :: fun((cauder_types:proc_id()) -> cauder_types:system()).
-type scheduler_fun() :: fun((nonempty_list_pid(), list_pid(), step_fun()) -> {cauder_types:system(), nonempty_list_pid()}).


-spec schedulers() -> Map when
  Map :: #{Key := SchedFun},
  Key :: atom(),
  SchedFun :: scheduler_fun().

schedulers() ->
  #{
    ?SCHEDULER_ROUND_ROBIN => fun sched_round_robin/3
  }.


-spec get(Key) -> {ok, SchedFun} | error when
  Key :: atom(),
  SchedFun :: scheduler_fun().

get(Key) ->
  maps:get(Key, schedulers()).


-spec sched_round_robin(PrevPids, NewPids, StepFun) -> {Sys, NextPids} when
  PrevPids :: [cauder_types:proc_id()],
  NewPids :: [cauder_types:proc_id(), ...],
  StepFun :: step_fun(),
  Sys :: cauder_types:system(),
  NextPids :: [cauder_types:proc_id(), ...].

sched_round_robin(PrevPids, NewPids, StepFun) ->
  UpdatedPids =
    lists:reverse(
      lists:foldl(
        fun
          (P, Ps) ->
            case lists:member(P, Ps) of
              true -> Ps;
              false -> [P | Ps]
            end
        end,
        lists:reverse(PrevPids),
        NewPids
      )
    ),
  NewSet = sets:from_list(NewPids),
  [Pid | Pids] = lists:filter(fun(P) -> sets:is_element(P, NewSet) end, UpdatedPids),
  Sys = StepFun(Pid),
  NextPids = lists:reverse([Pid | lists:reverse(Pids)]),
  {Sys, NextPids}.
