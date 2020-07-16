%%%-------------------------------------------------------------------
%%% @doc The main module for the rev-erlang project.
%%% This module includes functions for starting the application
%%% and interact with the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(cauder).
-export([start/0, load_file/1]).
-export([eval_opts/1, eval_step/2, eval_mult/3, eval_norm/1]).
-export([eval_roll/3, eval_roll_send/2, eval_roll_spawn/2, eval_roll_rec/2, eval_roll_var/2, eval_replay/0]).
-export([start_refs/0, stop_refs/0, reset_fresh_refs/1, ref_add/2, ref_lookup/1, ref_match_object/1]).

-include("cauder.hrl").


%% =====================================================================
%% @doc Starts the GUI

-spec start() -> ok.

start() ->
  cauder_gui:setup_gui(),
  ok.


-spec load_file(File) -> [MFA] when
  File :: file:filename(),
  MFA :: string().

load_file(File) ->
  Mod = list_to_atom(filename:basename(File, ".erl")),
  cauder_load:load_module(Mod, File),
  % TODO Only allow to start system from an exported function?
  Defs0 = ref_match_object({{Mod, '_', '_', '_'}, '_'}),
  Defs1 = lists:sort(fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end, Defs0),
  lists:map(fun({{M, F, A, _}, _}) -> io_lib:format("~s:~s/~p", [M, F, A]) end, Defs1).


%% =====================================================================
%% @doc Returns all the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(Sys) -> fwd_sem:eval_opts(Sys) ++ bwd_sem:eval_opts(Sys).


%% =====================================================================
%% @doc Performs a single evaluation step in the given System

-spec eval_step(cauder_types:system(), cauder_types:option()) -> cauder_types:system().

eval_step(Sys, #opt{sem = Sem, type = ?TYPE_MSG, id = Id})  -> Sem:eval_sched(Sys, Id);
eval_step(Sys, #opt{sem = Sem, type = ?TYPE_PROC, id = Id}) -> Sem:eval_step(Sys, Id).


%% =====================================================================
%% @doc Performs Steps evaluation steps in the given System in the
%% specified Direction

-spec eval_mult(cauder_types:system(), ?MULT_FWD | ?MULT_BWD, pos_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_mult(Sys, Dir, Steps) -> eval_mult_1(Sys, Dir, Steps, 0).


-spec eval_mult_1(cauder_types:system(), ?MULT_FWD | ?MULT_BWD, pos_integer(), non_neg_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_mult_1(Sys, _Dir, Steps, Steps) -> {Sys, Steps};
eval_mult_1(Sys, Dir, Steps, StepsDone) ->
  Sem =
  case Dir of
    ?MULT_FWD -> fwd_sem;
    ?MULT_BWD -> bwd_sem
  end,
  case sched:select_opt(Sem, Sys) of
    none -> {Sys, StepsDone};
    Opt ->
      Sys1 = eval_step(Sys, Opt),
      eval_mult_1(Sys1, Dir, Steps, StepsDone + 1)
  end.


%% =====================================================================
%% @doc Performs evaluation steps (except for sched steps) in System
%% until the system becomes "normalized" (more info on the paper)

-spec eval_norm(cauder_types:system()) -> {cauder_types:system(), non_neg_integer()}.

eval_norm(Sys) -> eval_norm_1(Sys, 0).


-spec eval_norm_1(cauder_types:system(), non_neg_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_norm_1(Sys, Steps) ->
  case fwd_sem:eval_procs_opts(Sys) of
    [] -> {Sys, Steps};
    Opts ->
      Idx = rand:uniform(length(Opts)),
      Opt = lists:nth(Idx, Opts),
      Sys1 = eval_step(Sys, Opt),
      eval_norm_1(Sys1, Steps + 1)
  end.


-spec eval_roll(System, Pid, Steps) -> {FocusLog, NewSystem, StepsDone} when
  System :: cauder_types:system(),
  Pid :: pos_integer(),
  Steps :: pos_integer(),
  FocusLog :: boolean(),
  NewSystem :: cauder_types:system(),
  StepsDone :: non_neg_integer().

eval_roll(Sys, Pid, Steps) ->
  Sys1 = utils:clear_log(Sys),
  {Sys2, StepsDone} = eval_roll_1(Sys1, Pid, Steps, 0),
  FocusLog = utils:must_focus_log(Sys2),
  {FocusLog, Sys2, StepsDone}.


-spec eval_roll_1(cauder_types:system(), pos_integer(), pos_integer(), non_neg_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_roll_1(Sys0, _Pid, Steps, Steps) -> {Sys0, Steps};
eval_roll_1(Sys0, Pid, Steps, StepsDone) ->
  case roll:can_roll(Sys0, Pid) of
    true ->
      Sys1 = roll:eval_step(Sys0, Pid),
      eval_roll_1(Sys1, Pid, Steps, StepsDone + 1);
    false ->
      {Sys0, StepsDone}
  end.


%% =====================================================================


-spec eval_roll_spawn(System, SpawnPid) -> {CanRoll, FocusLog, NewSystem} when
  System :: cauder_types:system(),
  SpawnPid :: pos_integer(),
  CanRoll :: boolean(),
  FocusLog :: boolean(),
  NewSystem :: cauder_types:system().

eval_roll_spawn(Sys0, SpawnPid) ->
  case roll:can_roll_spawn(Sys0, SpawnPid) of
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:eval_roll_spawn(Sys1, SpawnPid),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog, Sys2};
    false ->
      {false, false, Sys0}
  end.


-spec eval_roll_send(System, Time) -> {CanRoll, FocusLog, NewSystem} when
  System :: cauder_types:system(),
  Time :: pos_integer(),
  CanRoll :: boolean(),
  FocusLog :: boolean(),
  NewSystem :: cauder_types:system().

eval_roll_send(Sys0, Time) ->
  case roll:can_roll_send(Sys0, Time) of
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:eval_roll_send(Sys1, Time),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog, Sys2};
    false ->
      {false, false, Sys0}
  end.


-spec eval_roll_rec(System, Time) -> {CanRoll, FocusLog, NewSystem} when
  System :: cauder_types:system(),
  Time :: pos_integer(),
  CanRoll :: boolean(),
  FocusLog :: boolean(),
  NewSystem :: cauder_types:system().

eval_roll_rec(Sys0, Time) ->
  case roll:can_roll_rec(Sys0, Time) of
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:eval_roll_rec(Sys1, Time),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog, Sys2};
    false ->
      {false, false, Sys0}
  end.


-spec eval_roll_var(System, Name) -> {CanRoll, FocusLog, NewSystem} when
  System :: cauder_types:system(),
  Name :: atom(),
  CanRoll :: boolean(),
  FocusLog :: boolean(),
  NewSystem :: cauder_types:system().

eval_roll_var(Sys0, Name) ->
  case roll:can_roll_var(Sys0, Name) of
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:eval_roll_var(Sys1, Name),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog, Sys2};
    false ->
      {false, false, Sys0}
  end.


%% =====================================================================


eval_replay() -> ok.


%% =====================================================================
%% @doc Starts the ETS servers

-spec start_refs() -> ok.

start_refs() ->
  ?LOG("Starting refs"),
  ref_start().


%% =====================================================================
%% @doc Resets the fresh values for: Pid, Time and Var

-spec reset_fresh_refs(pos_integer()) -> ok.

reset_fresh_refs(FirstPid) ->
  ?LOG("Resetting fresh refs"),
  ref_add(?FRESH_PID, FirstPid + 1),
  ref_add(?FRESH_TIME, 1),
  ref_add(?FRESH_VAR, 1).


%% =====================================================================
%% @doc Stops the ETS servers

-spec stop_refs() -> ok.

stop_refs() ->
  ?LOG("Stopping refs"),
  ref_stop().


ref_start() ->
  ?APP_REF = ets:new(?APP_REF, [set, public, named_table]),
  ok.

ref_stop() ->
  true = ets:delete(?APP_REF),
  ok.

ref_add(Id, Ref) ->
  true = ets:insert(?APP_REF, {Id, Ref}),
  ok.

ref_lookup(Id) -> ets:lookup_element(?APP_REF, Id, 2).

ref_match_object(Id) -> ets:match_object(?APP_REF, Id).
