%%%-------------------------------------------------------------------
%%% @doc The main module for the rev-erlang project.
%%% This module includes functions for starting the application
%%% and interact with the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(cauder).

%% API
-export([start/0]).

-export([load_file/1, init_system/5, entry_points/1, system/0, status/0]).
%% Manual evaluation functions
-export([eval_opts/1, eval_reduce/2, eval_step/2]).
%% Automatic evaluation functions
-export([eval_mult/2]).
%% Replay evaluation functions
-export([eval_replay/2, eval_replay_send/1, eval_replay_spawn/1, eval_replay_rec/1]).
%% Rollback evaluation functions
-export([eval_roll/2, eval_roll_send/1, eval_roll_spawn/1, eval_roll_rec/1, eval_roll_var/1]).
%% ETS functions
-export([db_get/1, db_match/1]).

-export([edit_binding/2]).

-include("cauder.hrl").


%% ===================================================================
%% @doc Starts the GUI

-spec start() -> ok.

start() ->
  cauder_wx:start(),
  ok.


-spec load_file(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: atom().

load_file(File) ->
  Status = get(status),
  put(status, Status#status{loaded = true}),
  put(path, filename:absname(File)),
  cauder_load:file(File).


-spec init_system(Module, Function, Args, Pid, Log) -> 'ok' when
  Module :: atom(),
  Function :: atom(),
  Args :: [cauder_types:abstract_expr()],
  Pid :: cauder_types:proc_id(),
  Log :: cauder_types:log().

init_system(Mod, Fun, Args, Pid, Log) ->
  Proc = #proc{
    pid   = Pid,
    log   = Log,
    exprs = [{remote_call, 0, Mod, Fun, Args}],
    spf   = {Mod, Fun, length(Args)}
  },
  System = #sys{
    procs  = orddict:from_list([{Pid, Proc}]),
    ghosts = load_ghosts(Pid)
  },

  Status = get(status),
  put(status, Status#status{running = true}),
  put(system, System),
  put(last_pid, Pid),
  put(line, 0),

  ok.


-spec entry_points(Module :: atom()) -> list({Module :: atom(), Function :: atom(), Arity :: arity()}).

entry_points(Module) ->
  Defs = db_match({{Module, '_', '_', '_'}, '_'}),
  SortedDefs = lists:sort(fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end, Defs),
  % TODO Only allow to start system from an exported function?
  lists:map(fun({{M, F, A, _}, _}) -> {M, F, A} end, SortedDefs).


-spec system() -> cauder_types:system().

system() -> get(system). % TODO Remove
status() -> get(status). % TODO Remove


%% -------------------------------------------------------------------
%% @doc Loads the replay data for all the processes in the current replay
%% data, except for the one with the MainPid, which has already been loaded.

-spec load_ghosts(MainPid :: cauder_types:proc_id()) -> cauder_types:process_dict().

load_ghosts(MainPid) ->
  #replay{log_path = Path} = get(replay_data),
  {ok, Filenames} = file:list_dir(Path),
  Ghosts =
    lists:filtermap(
      fun(Filename) ->
        case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
          {match, [StrPid]} ->
            case list_to_integer(StrPid) of
              MainPid -> false;
              Pid ->
                Proc = #proc{
                  pid = Pid,
                  log = utils:get_log_data(Path, Pid)
                },
                {true, {Pid, Proc}}
            end;
          nomatch -> false
        end
      end, Filenames),
  orddict:from_list(Ghosts).


%% ===================================================================


%% ---------------------------------------------------------------------
%% @doc Returns all the evaluation options for a given System

-spec eval_opts(cauder_types:system()) -> [cauder_types:option()].

eval_opts(Sys) -> fwd_sem:eval_opts(Sys) ++ bwd_sem:eval_opts(Sys).


%% ==================== Manual evaluation ==================== %%


%% ---------------------------------------------------------------------
%% @doc Performs a single reduction step using the given semantics on the given process

-spec eval_reduce(Semantics, Pid) -> Rule when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  Rule :: cauder_types:rule().

eval_reduce(Sem, Pid) ->
  Sys0 = get(system),
  Opts = utils:filter_options(cauder:eval_opts(Sys0), Pid),
  {value, #opt{pid = Pid, sem = Sem, rule = Rule}} = lists:search(fun(Opt) -> Opt#opt.sem =:= Sem end, Opts),
  Sys1 = Sem:eval_step(Sys0, Pid),
  put(system, Sys1),
  Rule.


-spec eval_step(Semantics, Pid) -> nomatch | ReductionSteps when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  ReductionSteps :: pos_integer().

eval_step(Sem, Pid) ->
  Sys0 = get(system),
  {#proc{exprs = Es}, _} = utils:take_process(Sys0#sys.procs, Pid),
  case catch eval_step_1(Sem, Sys0, Pid, Es, 0) of
    {Sys1, Steps} ->
      put(system, Sys1),
      Steps;
    nomatch -> nomatch
  end.

eval_step_1(Sem, Sys0, Pid, Es0, Steps) ->
  Sys1 =
    try
      Sem:eval_step(Sys0, Pid)
    catch
      error:{badmatch, nomatch} -> throw(nomatch)
    end,
  {#proc{exprs = Es1}, _} = utils:take_process(Sys1#sys.procs, Pid),
  case Sem of
    ?FWD_SEM when Es1 =:= tl(Es0) -> throw({Sys1, Steps});
    ?BWD_SEM when tl(tl(Es1)) =:= Es0 -> throw({Sys0, Steps});
    _ -> continue
  end,
  eval_step_1(Sem, Sys1, Pid, Es0, Steps + 1).


%% ==================== Automatic evaluation ==================== %%


%% ---------------------------------------------------------------------
%% @doc Performs Steps evaluation steps in the given System in the
%% specified Direction

-spec eval_mult(Semantics, Steps) -> StepsDone when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer().

eval_mult(Sem, Steps) ->
  Sys0 = get(system),
  {Sys1, StepsDone} = eval_mult_1(Sys0, Sem, Steps, 0),
  put(system, Sys1),
  StepsDone.


-spec eval_mult_1(System, Semantics, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  System :: cauder_types:system(),
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

eval_mult_1(Sys, _Sem, Steps, Steps) -> {Sys, Steps};
eval_mult_1(Sys, Sem, Steps, StepsDone) ->
  case Sem:eval_opts(Sys) of
    [] -> {Sys, StepsDone};
    [#opt{pid = Pid, sem = Sem} | _] ->
      Sys1 = Sem:eval_step(Sys, Pid),
      eval_mult_1(Sys1, Sem, Steps, StepsDone + 1)
  end.


%% ==================== Replay evaluation ==================== %%


-spec eval_replay(Pid, Steps) -> StepsDone when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer().

eval_replay(Pid, Steps) ->
  Sys0 = get(system),
  {Sys1, StepsDone} = eval_replay_1(Sys0, Pid, Steps, 0),
  put(system, Sys1),
  StepsDone.


-spec eval_replay_1(cauder_types:system(), pos_integer(), pos_integer(), non_neg_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_replay_1(Sys0, _Pid, Steps, Steps) -> {Sys0, Steps};
eval_replay_1(Sys0, Pid, Steps, StepsDone) ->
  case replay:can_replay(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = replay:replay_step(Sys0, Pid),
      eval_replay_1(Sys1, Pid, Steps, StepsDone + 1)
  end.


-spec eval_replay_spawn(SpawnPid) -> Success when
  SpawnPid :: cauder_types:proc_id(),
  Success :: boolean().

eval_replay_spawn(SpawnPid) ->
  Sys0 = get(system),
  case replay:can_replay_spawn(Sys0, SpawnPid) of
    false -> false;
    true ->
      Sys1 = replay:replay_spawn(Sys0, SpawnPid),
      put(system, Sys1),
      true
  end.


-spec eval_replay_send(UID) -> Success when
  UID :: cauder_types:msg_id(),
  Success :: boolean().

eval_replay_send(UID) ->
  Sys0 = get(system),
  case replay:can_replay_send(Sys0, UID) of
    false -> false;
    true ->
      Sys1 = replay:replay_send(Sys0, UID),
      put(system, Sys1),
      true
  end.


-spec eval_replay_rec(UID) -> Success when
  UID :: cauder_types:msg_id(),
  Success :: boolean().

eval_replay_rec(UID) ->
  Sys0 = get(system),
  case replay:can_replay_rec(Sys0, UID) of
    false -> false;
    true ->
      Sys1 = replay:replay_rec(Sys0, UID),
      put(system, Sys1),
      true
  end.


%% ==================== Rollback evaluation ==================== %%


-spec eval_roll(Pid, Steps) -> {FocusLog, StepsDone} when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  FocusLog :: boolean(),
  StepsDone :: non_neg_integer().

eval_roll(Pid, Steps) ->
  Sys0 = get(system),
  Sys1 = utils:clear_log(Sys0),
  {Sys2, StepsDone} = eval_roll_1(Sys1, Pid, Steps, 0),
  put(system, Sys2),
  FocusLog = utils:must_focus_log(Sys2),
  {FocusLog, StepsDone}.


-spec eval_roll_1(cauder_types:system(), pos_integer(), pos_integer(), non_neg_integer()) -> {cauder_types:system(), non_neg_integer()}.

eval_roll_1(Sys0, _Pid, Steps, Steps) -> {Sys0, Steps};
eval_roll_1(Sys0, Pid, Steps, StepsDone) ->
  case roll:can_roll(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = roll:roll_step(Sys0, Pid),
      eval_roll_1(Sys1, Pid, Steps, StepsDone + 1)
  end.


-spec eval_roll_spawn(SpawnPid) -> {Success, FocusLog} when
  SpawnPid :: cauder_types:proc_id(),
  Success :: boolean(),
  FocusLog :: boolean().

eval_roll_spawn(SpawnPid) ->
  Sys0 = get(system),
  case roll:can_roll_spawn(Sys0, SpawnPid) of
    false -> {false, false};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_spawn(Sys1, SpawnPid),
      put(system, Sys2),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog}
  end.


-spec eval_roll_send(UID) -> {Success, FocusLog} when
  UID :: cauder_types:msg_id(),
  Success :: boolean(),
  FocusLog :: boolean().

eval_roll_send(UID) ->
  Sys0 = get(system),
  case roll:can_roll_send(Sys0, UID) of
    false -> {false, false};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_send(Sys1, UID),
      put(system, Sys2),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog}
  end.


-spec eval_roll_rec(UID) -> {Success, FocusLog} when
  UID :: cauder_types:msg_id(),
  Success :: boolean(),
  FocusLog :: boolean().

eval_roll_rec(UID) ->
  Sys0 = get(system),
  case roll:can_roll_rec(Sys0, UID) of
    false -> {false, false};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_rec(Sys1, UID),
      put(system, Sys2),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog}
  end.


-spec eval_roll_var(Name) -> {Success, FocusLog} when
  Name :: atom(),
  Success :: boolean(),
  FocusLog :: boolean().

eval_roll_var(Name) ->
  Sys0 = get(system),
  case roll:can_roll_var(Sys0, Name) of
    false -> {false, false};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_var(Sys1, Name),
      put(system, Sys2),
      FocusLog = utils:must_focus_log(Sys2),
      {true, FocusLog}
  end.


%% =====================================================================


edit_binding(Pid, {Key, NewValue}) ->
  Sys0 = get(system),
  PDict0 = Sys0#sys.procs,
  P0 = utils:find_process(PDict0, Pid),
  Bs0 = P0#proc.env,

  Bs1 = orddict:store(Key, NewValue, Bs0),
  P1 = P0#proc{env = Bs1},
  PDict1 = orddict:store(Pid, P1, PDict0),
  Sys1 = Sys0#sys{procs = PDict1},

  put(system, Sys1).


%% =====================================================================


db_get(Key) -> ets:lookup_element(get(db), Key, 2).

db_match(Key) -> ets:match_object(get(db), Key).
