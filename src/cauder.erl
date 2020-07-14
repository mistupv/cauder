%%%-------------------------------------------------------------------
%%% @doc The main module for the rev-erlang project.
%%% This module includes functions for starting the application
%%% and interact with the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(cauder).
-export([start/0, load_file/1]).
-export([start_refs/0, stop_refs/0, reset_fresh_refs/1]).
-export([eval_opts/1, eval_step/2, eval_mult/3, eval_norm/1,
         eval_roll/3, eval_roll_send/2, eval_roll_spawn/2,
         eval_roll_rec/2, eval_roll_var/2, eval_replay/0]).
-export([ref_add/2, ref_lookup/1, ref_match_object/1]).

-include("cauder.hrl").

%%--------------------------------------------------------------------
%% @doc Starts the GUI
%% @end
%%--------------------------------------------------------------------
start() ->
  cauder_gui:setup_gui(),
  ok.

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


%%--------------------------------------------------------------------
%% @doc Returns all the evaluation options for a given System
%% @end
%%--------------------------------------------------------------------
eval_opts(System) ->
  FwdOpts = fwd_sem:eval_opts(System),
  BwdOpts = bwd_sem:eval_opts(System),
  FwdOpts ++ BwdOpts.

eval_step(System, Option) ->
  #opt{sem = Semantics, type = Type, id = Id} = Option,
  NewSystem =
  case Type of
    ?TYPE_MSG -> Semantics:eval_sched(System, Id);
    ?TYPE_PROC -> Semantics:eval_step(System, Id)
  end,
  NewSystem.

%%--------------------------------------------------------------------
%% @doc Performs Steps evaluation steps in System in
%% the Option direction
%% @end
%%--------------------------------------------------------------------
eval_mult(System, Option, Steps) ->
  eval_mult_1(System, Option, Steps, 0).

eval_mult_1(System, _Option, Steps, Steps) ->
  {System, Steps};
eval_mult_1(System, Option, Steps, StepsDone) ->
  Sem =
  case Option of
    ?MULT_FWD -> fwd_sem;
    ?MULT_BWD -> bwd_sem
  end,
  SelOpt = sched:select_opt(Sem, System),
  case SelOpt of
    none ->
      {System, StepsDone};
    _ ->
      NewSystem = eval_step(System, SelOpt),
      eval_mult_1(NewSystem, Option, Steps, StepsDone + 1)
  end.

%%--------------------------------------------------------------------
%% @doc Performs evaluation steps (except for sched steps) in System
%% until the system becomes "normalized" (more info on the paper)
%% @end
%%--------------------------------------------------------------------
eval_norm(System) ->
  eval_norm_1(System, 0).

eval_norm_1(System, Steps) ->
  Opts = fwd_sem:eval_opts(System),
  ProcsOpts = utils:filter_procs_opts(Opts),
  case ProcsOpts of
    [] ->
      {System, Steps};
    _Other ->
      RandIdx = rand:uniform(length(ProcsOpts)),
      RandOpt = lists:nth(RandIdx, ProcsOpts),
      NewSystem = eval_step(System, RandOpt),
      eval_norm_1(NewSystem, Steps + 1)
  end.

eval_roll(System, Pid, Steps) ->
  EmptyLogSystem = utils:empty_log(System),
  {RolledSystem, StepsDone} = eval_roll_1(EmptyLogSystem, Pid, Steps, 0),
  FocusLog = utils:must_focus_log(RolledSystem),
  {FocusLog, RolledSystem, StepsDone}.

eval_roll_1(System, _Pid, Steps, Steps) ->
  {System, Steps};
eval_roll_1(System, Pid, Steps, StepsDone) ->
  case roll:can_roll(System, Pid) of
    false ->
      {System, StepsDone};
    true ->
      NewSystem = roll:eval_step(System, Pid),
      eval_roll_1(NewSystem, Pid, Steps, StepsDone + 1)
  end.

eval_roll_send(System, Id) ->
  case roll:can_roll_send(System, Id) of
    false ->
      {false, false, System};
    true ->
      EmptyLogSystem = utils:empty_log(System),
      RolledSystem = roll:eval_roll_send(EmptyLogSystem, Id),
      FocusLog = utils:must_focus_log(RolledSystem),
      {true, FocusLog, RolledSystem}
  end.

eval_roll_spawn(System, Id) ->
  case roll:can_roll_spawn(System, Id) of
    false ->
      {false, false, System};
    true ->
      EmptyLogSystem = utils:empty_log(System),
      RolledSystem = roll:eval_roll_spawn(EmptyLogSystem, Id),
      FocusLog = utils:must_focus_log(RolledSystem),
      {true, FocusLog, RolledSystem}
  end.

eval_roll_rec(System, Id) ->
  case roll:can_roll_rec(System, Id) of
    false ->
      {false, false, System};
    true ->
      EmptyLogSystem = utils:empty_log(System),
      RolledSystem = roll:eval_roll_rec(EmptyLogSystem, Id),
      FocusLog = utils:must_focus_log(RolledSystem),
      {true, FocusLog, RolledSystem}
  end.

eval_roll_var(System, Id) ->
  case roll:can_roll_var(System, Id) of
    false ->
      {false, false, System};
    true ->
      EmptyLogSystem = utils:empty_log(System),
      RolledSystem = roll:eval_roll_var(EmptyLogSystem, Id),
      FocusLog = utils:must_focus_log(RolledSystem),
      {true, FocusLog, RolledSystem}
  end.

eval_replay() ->
  ok.


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
