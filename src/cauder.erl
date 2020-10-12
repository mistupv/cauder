%%%-------------------------------------------------------------------
%%% @doc The main module for the rev-erlang project.
%%% This module includes functions for starting the application
%%% and interact with the reversible semantics for Erlang
%%% @end
%%%-------------------------------------------------------------------

-module(cauder).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([load_file/1, init_system/3, init_system/1, stop_system/0]).
-export([eval_opts/1]).
-export([eval_step/2, eval_step_over/2]).
-export([eval_mult/2]).
-export([eval_replay/2, eval_replay_send/1, eval_replay_spawn/1, eval_replay_rec/1]).
-export([eval_roll/2, eval_roll_send/1, eval_roll_spawn/1, eval_roll_rec/1, eval_roll_var/1]).
-export([get_entry_points/1, get_system/0, get_path/0]).
-export([set_binding/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("cauder.hrl").

-record(state, {
  system :: cauder_types:system() | undefined
}).

-type state() :: #state{}.

-export([db_match/1]). % TODO Remove


%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).


-spec start_link() -> {ok, Pid :: pid()} | {error, Reason :: term()}.
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


-spec stop() -> 'ok'.
stop() -> gen_server:stop(?SERVER).


%% -------------------- System -------------------- %%

-spec load_file(File :: file:filename()) -> {ok, Module :: atom()}.
load_file(File) -> gen_server:call(?SERVER, {load, File}).


-spec init_system(Module :: atom(), Function :: atom(), Args :: list(cauder_types:abstract_expr())) -> 'ok'.
init_system(Mod, Fun, Args) -> gen_server:call(?SERVER, {system, {init, {Mod, Fun, Args}}}).


-spec init_system(LogPath :: file:filename()) -> {Module :: atom(), Function :: atom(), Arity :: arity()}.
init_system(Path) -> gen_server:call(?SERVER, {system, {init, Path}}).


-spec stop_system() -> 'ok'.
stop_system() -> gen_server:call(?SERVER, {system, stop}).


%% -------------------- Evaluation -------------------- %%

-spec eval_opts(System :: cauder_types:system()) -> Options :: list(cauder_types:option()).
eval_opts(Sys) -> fwd_sem:eval_opts(Sys) ++ bwd_sem:eval_opts(Sys).


%% -------------------- Manual -------------------- %%

-spec eval_step(Semantics :: cauder_types:semantics(), Pid :: cauder_types:proc_id()) -> Rule :: cauder_types:rule().
eval_step(Sem, Pid) -> gen_server:call(?SERVER, ?EVAL_MANUAL(step, {Sem, Pid})).


-spec eval_step_over(Semantics :: cauder_types:semantics(), Pid :: cauder_types:proc_id()) -> nomatch | (StepsDone :: pos_integer()).
eval_step_over(Sem, Pid) -> gen_server:call(?SERVER, ?EVAL_MANUAL(step_over, {Sem, Pid})).


%% -------------------- Automatic -------------------- %%

-spec eval_mult(Semantics :: cauder_types:semantics(), Steps :: pos_integer()) -> StepsDone :: non_neg_integer().
eval_mult(Sem, Steps) -> gen_server:call(?SERVER, ?EVAL_AUTOMATIC({Sem, Steps})).


%% -------------------- Replay -------------------- %%

-spec eval_replay(Pid :: cauder_types:proc_id(), Steps :: pos_integer()) -> StepsDone :: non_neg_integer().
eval_replay(Pid, Steps) -> gen_server:call(?SERVER, ?EVAL_REPLAY(steps, {Pid, Steps})).


-spec eval_replay_spawn(Pid :: cauder_types:proc_id()) -> Success :: boolean().
eval_replay_spawn(Pid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(spawn, Pid)).


-spec eval_replay_send(Uid :: cauder_types:msg_id()) -> Success :: boolean().
eval_replay_send(Uid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(send, Uid)).


-spec eval_replay_rec(Uid :: cauder_types:msg_id()) -> Success :: boolean().
eval_replay_rec(Uid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(rec, Uid)).


%% -------------------- Rollback -------------------- %%

-spec eval_roll(Pid :: cauder_types:proc_id(), Steps :: pos_integer()) -> {FocusLog :: boolean(), StepsDone :: non_neg_integer()}.
eval_roll(Pid, Steps) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(steps, {Pid, Steps})).


-spec eval_roll_spawn(Pid :: cauder_types:proc_id()) -> {Success :: boolean(), FocusLog :: boolean()}.
eval_roll_spawn(Pid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(spawn, Pid)).


-spec eval_roll_send(Uid :: cauder_types:msg_id()) -> {Success :: boolean(), FocusLog :: boolean()}.
eval_roll_send(Uid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(send, Uid)).


-spec eval_roll_rec(Uid :: cauder_types:msg_id()) -> {Success :: boolean(), FocusLog :: boolean()}.
eval_roll_rec(Uid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(rec, Uid)).


-spec eval_roll_var(Name :: atom()) -> {Success :: boolean(), FocusLog :: boolean()}.
eval_roll_var(Name) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(var, Name)).


%% -------------------- Retrieve information -------------------- %%

-spec get_entry_points(Module :: atom()) -> list({Module :: atom(), Function :: atom(), Arity :: arity()}).
get_entry_points(Module) -> gen_server:call(?SERVER, {get, {entry_points, Module}}).


-spec get_system() -> System :: cauder_types:system().
get_system() -> gen_server:call(?SERVER, {get, system}).


-spec get_path() -> Path :: file:filename().
get_path() -> gen_server:call(?SERVER, {get, path}).


%% -------------------- Modify information -------------------- %%

-spec set_binding(Pid :: cauder_types:proc_id(), {Key :: atom(), Value :: term()}) -> ok.
set_binding(Pid, {Key, Value}) -> gen_server:call(?SERVER, {set, {binding, Pid}, {Key, Value}}).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init(Args :: term()) -> {ok, State :: state()} | {ok, State :: state(), timeout() | hibernate} | {stop, Reason :: term()} | ignore.

init([]) ->
  Db = ets:new(?MODULE, [set, public, named_table]),
  put(db, Db),
  {ok, #state{}}.


-spec handle_call(Request :: any(), From :: any(), State :: state()) -> {reply, term(), state()} | {stop, term(), state()}.

handle_call({load, File}, _From, State) ->
  put(path, filename:absname(filename:dirname(File))),
  {ok, Module} = cauder_load:file(File),
  {reply, {ok, Module}, State};

handle_call({system, {init, {M, F, As}}}, _From, State) ->
  Pid = 1,
  Proc = #proc{
    pid   = Pid,
    exprs = [cauder_syntax:remote_call(M, F, As)],
    spf   = {M, F, length(As)}
  },
  System = #sys{
    procs = orddict:from_list([{Pid, Proc}])
  },

  put(last_pid, Pid),

  {reply, ok, State#state{system = System}};

handle_call({system, {init, LogPath}}, _From, State) ->
  #replay{log_path = LogPath, call = {M, F, As}, main_pid = Pid} = utils:load_replay_data(LogPath),
  Proc = #proc{
    pid   = Pid,
    exprs = [cauder_syntax:remote_call(M, F, As)],
    spf   = {M, F, length(As)}
  },
  System = #sys{
    procs = orddict:from_list([{Pid, Proc}]),
    logs  = load_logs(LogPath)
  },

  put(last_pid, Pid),

  {reply, {M, F, length(As)}, State#state{system = System}};

handle_call({system, stop}, _From, State) ->
  erase(last_pid),
  erase(last_uid),
  erase(last_var),
  {reply, ok, State#state{system = undefined}};

%% -------------------- Manual -------------------- %%

handle_call(?EVAL_MANUAL(step, {Sem, Pid}), _From, #state{system = Sys0} = State) ->
  Opts = utils:filter_options(eval_opts(Sys0), Pid),
  {value, #opt{pid = Pid, sem = Sem, rule = Rule}} = lists:search(fun(Opt) -> Opt#opt.sem =:= Sem end, Opts),
  Sys1 = Sem:eval_step(Sys0, Pid),
  {reply, Rule, State#state{system = Sys1}};

handle_call(?EVAL_MANUAL(step_over, {Sem, Pid}), _From, #state{system = Sys0} = State) ->
  {#proc{exprs = Es}, _} = orddict:take(Pid, Sys0#sys.procs),
  case eval_step_over(Sem, Sys0, Pid, Es) of
    {Sys1, Steps} ->
      {reply, Steps, State#state{system = Sys1}};
    nomatch ->
      {reply, nomatch, State}
  end;

%% -------------------- Automatic -------------------- %%

handle_call(?EVAL_AUTOMATIC({Sem, Steps}), _From, #state{system = Sys0} = State) ->
  {Sys1, StepsDone} = eval_mult(Sem, Sys0, Steps, 0),
  {reply, StepsDone, State#state{system = Sys1}};

%% -------------------- Replay -------------------- %%

handle_call(?EVAL_REPLAY(steps, {Pid, Steps}), _From, #state{system = Sys0} = State) ->
  {Sys1, StepsDone} = eval_replay(Sys0, Pid, Steps, 0),
  {reply, StepsDone, State#state{system = Sys1}};

handle_call(?EVAL_REPLAY(spawn, Pid), _From, #state{system = Sys0} = State) ->
  case replay:can_replay_spawn(Sys0, Pid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = replay:replay_spawn(Sys0, Pid),
      {reply, true, State#state{system = Sys1}}
  end;

handle_call(?EVAL_REPLAY(send, Uid), _From, #state{system = Sys0} = State) ->
  case replay:can_replay_send(Sys0, Uid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = replay:replay_send(Sys0, Uid),
      {reply, true, State#state{system = Sys1}}
  end;

handle_call(?EVAL_REPLAY(rec, Uid), _From, #state{system = Sys0} = State) ->
  case replay:can_replay_rec(Sys0, Uid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = replay:replay_rec(Sys0, Uid),
      {reply, true, State#state{system = Sys1}}
  end;

%% -------------------- Rollback -------------------- %%

handle_call(?EVAL_ROLLBACK(steps, {Pid, Steps}), _From, #state{system = Sys0} = State) ->
  Sys1 = utils:clear_log(Sys0),
  {Sys2, StepsDone} = eval_roll(Sys1, Pid, Steps, 0),
  FocusLog = utils:must_focus_log(Sys2),
  {reply, {FocusLog, StepsDone}, State#state{system = Sys2}};

handle_call(?EVAL_ROLLBACK(spawn, Pid), _From, #state{system = Sys0} = State) ->
  case roll:can_roll_spawn(Sys0, Pid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_spawn(Sys1, Pid),
      FocusLog = utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(send, Uid), _From, #state{system = Sys0} = State) ->
  case roll:can_roll_send(Sys0, Uid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_send(Sys1, Uid),
      FocusLog = utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(rec, Uid), _From, #state{system = Sys0} = State) ->
  case roll:can_roll_rec(Sys0, Uid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_rec(Sys1, Uid),
      FocusLog = utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(var, Name), _From, #state{system = Sys0} = State) ->
  case roll:can_roll_var(Sys0, Name) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = utils:clear_log(Sys0),
      Sys2 = roll:roll_var(Sys1, Name),
      FocusLog = utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

%% -------------------- Retrieve information -------------------- %%

handle_call({get, {entry_points, Module}}, _From, State) ->
  Defs = db_match({{Module, '_', '_', '_'}, '_'}),
  SortedDefs = lists:sort(fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end, Defs),
  % TODO Only allow to start system from an exported function?
  EntryPoints = lists:map(fun({{M, F, A, _}, _}) -> {M, F, A} end, SortedDefs),
  {reply, EntryPoints, State};

handle_call({get, system}, _From, State) -> {reply, State#state.system, State};

handle_call({get, path}, _From, State)   -> {reply, get(path), State};

%% -------------------- Modify information -------------------- %%

handle_call({set, {binding, Pid}, {Key, NewValue}}, _From, #state{system = Sys0} = State) ->
  PDict0 = Sys0#sys.procs,
  {ok, P0} = orddict:find(Pid, PDict0),
  Bs0 = P0#proc.env,

  Bs1 = orddict:store(Key, NewValue, Bs0),
  P1 = P0#proc{env = Bs1},
  PDict1 = orddict:store(Pid, P1, PDict0),
  Sys1 = Sys0#sys{procs = PDict1},

  {reply, ok, State#state{system = Sys1}};

handle_call(Request, _From, State) ->
  io:format("Unhandled Call:~n~p~n", [Request]),
  {reply, ok, State}.


-spec handle_cast(Request :: any(), State :: state()) -> {'noreply', state()}.

handle_cast(Request, State) ->
  io:format("Unhandled Cast:~n~p~n", [Request]),
  {noreply, State}.


-spec handle_info(Info :: any(), State :: state()) -> {'noreply', state()}.

handle_info(Info, State) ->
  io:format("Unhandled Info:~n~p~n", [Info]),
  {noreply, State}.


-spec terminate(Reason :: any(), State :: state()) -> 'ok'.

terminate(_Reason, _State) ->
  ets:delete(?MODULE),
  ok.


-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% -------------------------------------------------------------------
%% @private
%% @doc If replay data has been loaded, then it loads the log for all
%% the available processes.

-spec load_logs(LogPath :: file:filename()) -> cauder_types:log_dict().

load_logs(Path) ->
  {ok, Filenames} = file:list_dir(Path),
  orddict:from_list(
    lists:filtermap(
      fun(Filename) ->
        case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
          {match, [StrPid]} ->
            Pid = list_to_integer(StrPid),
            Log = utils:get_log_data(Path, Pid),
            {true, {Pid, Log}};
          nomatch -> false
        end
      end,
      Filenames
    )
  ).


%% -------------------- Manual -------------------- %%

-spec eval_step_over(Semantics, System, Pid, Expressions) -> {NewSystem, StepsDone} | nomatch when
  Semantics :: cauder_types:semantics(),
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Expressions :: list(cauder_types:abstract_expr()),
  NewSystem :: cauder_types:system(),
  StepsDone :: non_neg_integer().

eval_step_over(Sem, Sys, Pid, Es) ->
  RecStep =
    fun Name(Sys0, Steps) ->
      Sys1 =
        try
          Sem:eval_step(Sys0, Pid)
        catch
          error:{badmatch, nomatch} -> throw(nomatch)
        end,
      {#proc{exprs = Es1}, _} = orddict:take(Pid, Sys1#sys.procs),
      case Sem of
        ?FWD_SEM when Es1 =:= tl(Es) -> throw({Sys1, Steps});
        ?BWD_SEM when tl(tl(Es1)) =:= Es -> throw({Sys0, Steps});
        _ -> continue
      end,
      Name(Sys1, Steps + 1)
    end,
  catch RecStep(Sys, 0).


%% -------------------- Automatic -------------------- %%

-spec eval_mult(Semantics, System, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  Semantics :: cauder_types:semantics(),
  System :: cauder_types:system(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

eval_mult(_, Sys, Steps, Steps) -> {Sys, Steps};
eval_mult(Sem, Sys, Steps, StepsDone) ->
  case Sem:eval_opts(Sys) of
    [] -> {Sys, StepsDone};
    [#opt{pid = Pid, sem = Sem} | _] ->
      Sys1 = Sem:eval_step(Sys, Pid),
      eval_mult(Sem, Sys1, Steps, StepsDone + 1)
  end.


%% -------------------- Replay -------------------- %%

-spec eval_replay(System, Pid, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

eval_replay(Sys0, _, Steps, Steps) -> {Sys0, Steps};
eval_replay(Sys0, Pid, Steps, StepsDone) ->
  case replay:can_replay(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = replay:replay_step(Sys0, Pid),
      eval_replay(Sys1, Pid, Steps, StepsDone + 1)
  end.


%% -------------------- Rollback -------------------- %%

-spec eval_roll(System, Pid, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

eval_roll(Sys0, _, Steps, Steps) -> {Sys0, Steps};
eval_roll(Sys0, Pid, Steps, StepsDone) ->
  case roll:can_roll(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = roll:roll_step(Sys0, Pid),
      eval_roll(Sys1, Pid, Steps, StepsDone + 1)
  end.


%% =====================================================================


db_match(Key) -> ets:match_object(get(db), Key).
