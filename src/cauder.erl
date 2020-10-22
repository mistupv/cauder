%%%-----------------------------------------------------------------------------
%%% @doc CauDEr debugging server.
%%% This module includes functions for starting the server and interacting with
%%% it using the reversible semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder).

-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([load_file/1, init_system/3, init_system/1, stop_system/0]).
-export([eval_opts/1]).
-export([step/2, step_over/2]).
-export([step_multiple/2]).
-export([replay_steps/2, replay_send/1, replay_spawn/1, replay_receive/1]).
-export([rollback_steps/2, rollback_send/1, rollback_spawn/1, rollback_receive/1, rollback_variable/1]).
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


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Starts the debugging server.

-spec start() -> {ok, Pid} | {error, Reason} when
  Pid :: pid(),
  Reason :: term().

start() -> gen_server:start({local, ?SERVER}, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc Starts the debugging server as part of a supervision tree.

-spec start_link() -> {ok, Pid} | {error, Reason} when
  Pid :: pid(),
  Reason :: term().

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%------------------------------------------------------------------------------
%% @doc Stops the debugging server.

-spec stop() -> ok.

stop() -> gen_server:stop(?SERVER).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Loads the given file (module). Returns the name of the loaded module.

-spec load_file(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: module().

load_file(File) -> gen_server:call(?SERVER, {load, File}).


%%------------------------------------------------------------------------------
%% @doc Initializes the system in manual mode.
%% The system starts with a call to the given function, from the given module,
%% with the given arguments.

-spec init_system(Module, Function, Arguments) -> ok when
  Module :: module(),
  Function :: atom(),
  Arguments :: cauder_types:af_args().

init_system(Mod, Fun, Args) -> gen_server:call(?SERVER, {system, {init, {Mod, Fun, Args}}}).


%%------------------------------------------------------------------------------
%% @doc Initializes the system in replay mode.
%% The system starts with a call to the function specified in the specified
%% replay data.
%% Returns a MFA tuple that corresponds to the initial function call.

-spec init_system(LogPath) -> MFA when
  LogPath :: file:filename(),
  MFA :: mfa().

init_system(Path) -> gen_server:call(?SERVER, {system, {init, Path}}).


%%------------------------------------------------------------------------------
%% @doc Stops the system.

-spec stop_system() -> ok.

stop_system() -> gen_server:call(?SERVER, {system, stop}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation options for the given system.
%%
%% @todo Remove argument

-spec eval_opts(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

eval_opts(Sys) -> cauder_semantics_forwards:options(Sys) ++ cauder_semantics_backwards:options(Sys).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a step in the given process using the given semantics.
%% Returns the rule applied.

-spec step(Semantics, Pid) -> Rule when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  Rule :: cauder_types:rule().

step(Sem, Pid) -> gen_server:call(?SERVER, ?EVAL_MANUAL(step, {Sem, Pid})).


%%------------------------------------------------------------------------------
%% @doc Steps over to the next line in the given process using the given
%% semantics.
%% Returns the number of steps performed, or `nomatch' if the evaluation reaches
%% a receive expression that cannot evaluate.
%%
%% @todo Currently not in use.

-spec step_over(Semantics, Pid) -> nomatch | StepsDone when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  StepsDone :: pos_integer().

step_over(Sem, Pid) -> gen_server:call(?SERVER, ?EVAL_MANUAL(step_over, {Sem, Pid})).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs the given number of steps, in any process, using the given
%% semantics.
%% Returns the number of steps that were actually performed.
%% Note: `Steps >= StepsDone >= 0'.

-spec step_multiple(Semantics, Steps) -> StepsDone when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer().

step_multiple(Sem, Steps) -> gen_server:call(?SERVER, ?EVAL_AUTOMATIC({Sem, Steps})).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Replays the given number of steps in the given process.
%% Returns the actual number of steps that were replayed.
%% Note: `Steps >= StepsDone >= 0'.

-spec replay_steps(Pid, Steps) -> StepsDone when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer().

replay_steps(Pid, Steps) -> gen_server:call(?SERVER, ?EVAL_REPLAY(steps, {Pid, Steps})).


%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid.
%% Returns `true' if the process was spawned successfully, or `false' if the
%% process could not be spawned.

-spec replay_spawn(Pid) -> Success when
  Pid :: cauder_types:proc_id(),
  Success :: boolean().

replay_spawn(Pid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(spawn, Pid)).


%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid.
%% Returns `true' if the message was sent successfully, or `false' if the
%% message could not be sent.

-spec replay_send(Uid) -> Success when
  Uid :: cauder_types:msg_id(),
  Success :: boolean().

replay_send(Uid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(send, Uid)).


%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid.
%% Returns `true' if the message was received successfully, or `false' if the
%% message could not be received.

-spec replay_receive(Uid) -> Success when
  Uid :: cauder_types:msg_id(),
  Success :: boolean().

replay_receive(Uid) -> gen_server:call(?SERVER, ?EVAL_REPLAY(rec, Uid)).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Rolls back the given number of steps in the given process.
%% Returns `{FocusLog, StepsDone}' where `FocusLog' is a boolean value
%% indicating whether the roll log should be focused or not, and `StepsDone' is
%% the actual number of steps that were rolled back.
%% Note: `Steps >= StepsDone >= 0'.

-spec rollback_steps(Pid, Steps) -> {FocusLog, StepsDone} when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  FocusLog :: boolean(),
  StepsDone :: non_neg_integer().

rollback_steps(Pid, Steps) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(steps, {Pid, Steps})).


%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid.
%% Returns `{Success, FocusLog}' where `Success' indicates if the spawning of
%% the process was successfully rolled back (`true'), or if the process was not
%% found (`false'); and `FocusLog' is a boolean value indicating whether the
%% roll log should be focused or not.

-spec rollback_spawn(Pid) -> {Success, FocusLog} when
  Pid :: cauder_types:proc_id(),
  Success :: boolean(),
  FocusLog :: boolean().

rollback_spawn(Pid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(spawn, Pid)).


%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid.
%% Returns `{Success, FocusLog}' where `Success' indicates if the sending of
%% the message was successfully rolled back (`true'), or if the message was not
%% found (`false'); and `FocusLog' is a boolean value indicating whether the
%% roll log should be focused or not.

-spec rollback_send(Uid) -> {Success, FocusLog} when
  Uid :: cauder_types:msg_id(),
  Success :: boolean(),
  FocusLog :: boolean().

rollback_send(Uid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(send, Uid)).


%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid.
%% Returns `{Success, FocusLog}' where `Success' indicates if the reception of
%% the message was successfully rolled back (`true'), or if the message was not
%% found (`false'); and `FocusLog' is a boolean value indicating whether the
%% roll log should be focused or not.

-spec rollback_receive(Uid) -> {Success, FocusLog} when
  Uid :: cauder_types:msg_id(),
  Success :: boolean(),
  FocusLog :: boolean().

rollback_receive(Uid) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(rec, Uid)).


%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name.
%% Returns `{Success, FocusLog}' where `Success' indicates if the binding of
%% the variable was successfully rolled back (`true'), or if the variable was
%% not found (`false'); and `FocusLog' is a boolean value indicating whether the
%% roll log should be focused or not.

-spec rollback_variable(Name) -> {Success, FocusLog} when
  Name :: atom(),
  Success :: boolean(),
  FocusLog :: boolean().

rollback_variable(Name) -> gen_server:call(?SERVER, ?EVAL_ROLLBACK(var, Name)).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the possible entry points of the given module.

-spec get_entry_points(Module) -> MFAs when
  Module :: module(),
  MFAs :: [mfa()].

get_entry_points(Module) -> gen_server:call(?SERVER, {get, {entry_points, Module}}).


%%------------------------------------------------------------------------------
%% @doc Returns the system.

-spec get_system() -> System when
  System :: cauder_types:system() | undefined.

get_system() -> gen_server:call(?SERVER, {get, system}).


%%------------------------------------------------------------------------------
%% @doc Returns the current working directory.

-spec get_path() -> Path when
  Path :: file:filename() | undefined.

get_path() -> gen_server:call(?SERVER, {get, path}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Sets a new binding for the variable with the given name in the given
%% process.

-spec set_binding(Pid, {Key, Value}) -> ok when
  Pid :: cauder_types:proc_id(),
  Key :: atom(),
  Value :: term().

set_binding(Pid, {Key, Value}) -> gen_server:call(?SERVER, {set, {binding, Pid}, {Key, Value}}).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @private

-spec init(Args) -> {ok, State} when
  Args :: term(),
  State :: state().

init([]) ->
  ?APP_DB = ets:new(?APP_DB, [set, private, named_table]),
  {ok, #state{}}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} | {stop, Reply, NewState} when
  Request :: term(),
  From :: {pid(), term()},
  State :: state(),
  Reply :: term(),
  NewState :: state().

handle_call({load, File}, _From, State) ->
  put(path, filename:absname(filename:dirname(File))),
  {ok, Module} = cauder_load:file(File),
  {reply, {ok, Module}, State};

handle_call({system, {init, {M, F, As}}}, _From, State) ->
  Pid = cauder_utils:fresh_pid(),
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
  #replay{log_path = LogPath, call = {M, F, As}, main_pid = Pid} = cauder_utils:load_replay_data(LogPath),
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

%%%=============================================================================

handle_call(?EVAL_MANUAL(step, {Sem, Pid}), _From, #state{system = Sys0} = State) ->
  Opts = cauder_utils:filter_options(eval_opts(Sys0), Pid),
  {value, #opt{pid = Pid, sem = Sem, rule = Rule}} = lists:search(fun(Opt) -> Opt#opt.sem =:= Sem end, Opts),
  Sys1 = Sem:step(Sys0, Pid),
  {reply, Rule, State#state{system = Sys1}};

handle_call(?EVAL_MANUAL(step_over, {Sem, Pid}), _From, #state{system = Sys0} = State) ->
  {#proc{exprs = Es}, _} = orddict:take(Pid, Sys0#sys.procs),
  case step_over(Sem, Sys0, Pid, Es) of
    {Sys1, Steps} ->
      {reply, Steps, State#state{system = Sys1}};
    nomatch ->
      {reply, nomatch, State}
  end;

%%%=============================================================================

handle_call(?EVAL_AUTOMATIC({Sem, Steps}), _From, #state{system = Sys0} = State) ->
  {Sys1, StepsDone} = step_multiple(Sem, Sys0, Steps, 0),
  {reply, StepsDone, State#state{system = Sys1}};

%%%=============================================================================

handle_call(?EVAL_REPLAY(steps, {Pid, Steps}), _From, #state{system = Sys0} = State) ->
  {Sys1, StepsDone} = replay_steps(Sys0, Pid, Steps, 0),
  {reply, StepsDone, State#state{system = Sys1}};

handle_call(?EVAL_REPLAY(spawn, Pid), _From, #state{system = Sys0} = State) ->
  case cauder_replay:can_replay_spawn(Sys0, Pid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = cauder_replay:replay_spawn(Sys0, Pid),
      {reply, true, State#state{system = Sys1}}
  end;

handle_call(?EVAL_REPLAY(send, Uid), _From, #state{system = Sys0} = State) ->
  case cauder_replay:can_replay_send(Sys0, Uid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = cauder_replay:replay_send(Sys0, Uid),
      {reply, true, State#state{system = Sys1}}
  end;

handle_call(?EVAL_REPLAY(rec, Uid), _From, #state{system = Sys0} = State) ->
  case cauder_replay:can_replay_receive(Sys0, Uid) of
    false ->
      {reply, false, State};
    true ->
      Sys1 = cauder_replay:replay_receive(Sys0, Uid),
      {reply, true, State#state{system = Sys1}}
  end;

%%%=============================================================================

handle_call(?EVAL_ROLLBACK(steps, {Pid, Steps}), _From, #state{system = Sys0} = State) ->
  Sys1 = cauder_utils:clear_log(Sys0),
  {Sys2, StepsDone} = rollback_steps(Sys1, Pid, Steps, 0),
  FocusLog = cauder_utils:must_focus_log(Sys2),
  {reply, {FocusLog, StepsDone}, State#state{system = Sys2}};

handle_call(?EVAL_ROLLBACK(spawn, Pid), _From, #state{system = Sys0} = State) ->
  case cauder_rollback:can_rollback_spawn(Sys0, Pid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = cauder_utils:clear_log(Sys0),
      Sys2 = cauder_rollback:rollback_spawn(Sys1, Pid),
      FocusLog = cauder_utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(send, Uid), _From, #state{system = Sys0} = State) ->
  case cauder_rollback:can_rollback_send(Sys0, Uid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = cauder_utils:clear_log(Sys0),
      Sys2 = cauder_rollback:rollback_send(Sys1, Uid),
      FocusLog = cauder_utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(rec, Uid), _From, #state{system = Sys0} = State) ->
  case cauder_rollback:can_rollback_receive(Sys0, Uid) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = cauder_utils:clear_log(Sys0),
      Sys2 = cauder_rollback:rollback_receive(Sys1, Uid),
      FocusLog = cauder_utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

handle_call(?EVAL_ROLLBACK(var, Name), _From, #state{system = Sys0} = State) ->
  case cauder_rollback:can_rollback_variable(Sys0, Name) of
    false ->
      {reply, {false, false}, State};
    true ->
      Sys1 = cauder_utils:clear_log(Sys0),
      Sys2 = cauder_rollback:rollback_variable(Sys1, Name),
      FocusLog = cauder_utils:must_focus_log(Sys2),
      {reply, {true, FocusLog}, State#state{system = Sys2}}
  end;

%%%=============================================================================

handle_call({get, {entry_points, Module}}, _From, State) ->
  Defs = ets:match_object(?APP_DB, {{Module, '_', '_', '_'}, '_'}),
  SortedDefs = lists:sort(fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end, Defs),
  % TODO Only allow to start system from an exported function?
  EntryPoints = lists:map(fun({{M, F, A, _}, _}) -> {M, F, A} end, SortedDefs),
  {reply, EntryPoints, State};

handle_call({get, system}, _From, State) -> {reply, State#state.system, State};

handle_call({get, path}, _From, State)   -> {reply, get(path), State};

%%%=============================================================================

handle_call({set, {binding, Pid}, {Key, NewValue}}, _From, #state{system = Sys0} = State) ->
  PDict0 = Sys0#sys.procs,
  {ok, P0} = orddict:find(Pid, PDict0),
  Bs0 = P0#proc.env,

  Bs1 = orddict:store(Key, NewValue, Bs0),
  P1 = P0#proc{env = Bs1},
  PDict1 = orddict:store(Pid, P1, PDict0),
  Sys1 = Sys0#sys{procs = PDict1},

  {reply, ok, State#state{system = Sys1}};

%%%=============================================================================

handle_call(Request, _From, State) ->
  io:format("Unhandled Call:~n~p~n", [Request]),
  {reply, ok, State}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_cast(Request, State) -> {noreply, NewState} when
  Request :: any(),
  State :: state(),
  NewState :: state().

handle_cast(Request, State) ->
  io:format("Unhandled Cast:~n~p~n", [Request]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_info(Info, State) -> {noreply, NewState} when
  Info :: any(),
  State :: state(),
  NewState :: state().

handle_info(Info, State) ->
  io:format("Unhandled Info:~n~p~n", [Info]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private

-spec terminate(Reason, State) -> ok when
  Reason :: any(),
  State :: state().

terminate(_Reason, _State) ->
  ets:delete(?APP_DB),
  ok.


%%------------------------------------------------------------------------------
%% @private

-spec code_change(OldVsn, State, Extra) -> {ok, NewState} when
  OldVsn :: (term() | {down, term()}),
  State :: state(),
  Extra :: term(),
  NewState :: state().

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Loads the logs for all the available processes.

-spec load_logs(LogPath) -> LogDict when
  LogPath :: file:filename(),
  LogDict :: cauder_types:log_dict().

load_logs(LogPath) ->
  {ok, Filenames} = file:list_dir(LogPath),
  orddict:from_list(
    lists:filtermap(
      fun(Filename) ->
        case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
          {match, [StrPid]} ->
            Pid = list_to_integer(StrPid),
            Log = load_log(Pid, LogPath),
            {true, {Pid, Log}};
          nomatch -> false
        end
      end,
      Filenames
    )
  ).


%%------------------------------------------------------------------------------
%% @doc Loads the log of the given process.

-spec load_log(Pid, LogPath) -> Log when
  Pid :: cauder_types:proc_id(),
  LogPath :: file:filename(),
  Log :: cauder_types:log().

load_log(Pid, LogPath) ->
  File = filename:join(LogPath, "trace_" ++ integer_to_list(Pid) ++ ".log"),
  {ok, FileHandler} = file:open(File, [read]),
  Log = read_log(FileHandler, Pid, []),
  file:close(FileHandler),
  Log.


%%------------------------------------------------------------------------------
%% @doc Reads and parses the log from the given `IoDevice'.

-spec read_log(IoDevice, Pid, Log1) -> Log2 when
  IoDevice :: file:io_device(),
  Pid :: cauder_types:proc_id(),
  Log1 :: cauder_types:log(),
  Log2 :: cauder_types:log().

read_log(FileHandler, Pid, Data) ->
  case file:read_line(FileHandler) of
    eof -> lists:reverse(Data);
    {ok, Line} ->
      Entry = parse_log_entry(string:chomp(Line), Pid),
      read_log(FileHandler, Pid, [Entry | Data])
  end.


%%------------------------------------------------------------------------------
%% @doc Parses a string as a log entry and return it.

-spec parse_log_entry(String, Pid) -> LogEntry when
  String :: string(),
  Pid :: cauder_types:proc_id(),
  LogEntry :: cauder_types:log_entry().

parse_log_entry(String, Pid) ->
  case erl_scan:string(String ++ ".") of
    {ok, Tokens, _} ->
      case erl_parse:parse_exprs(Tokens) of
        {ok, Exprs} ->
          [{value, _, {Pid, Action, Id}}] = cauder_syntax:expr_list(Exprs),
          {Action, Id};
        _Err -> error({parse_error, String, Tokens})
      end;
    _Err -> error({parse_error, String})
  end.


%%%=============================================================================


-spec step_over(Semantics, System, Pid, Expressions) -> {NewSystem, StepsDone} | nomatch when
  Semantics :: cauder_types:semantics(),
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Expressions :: [cauder_types:abstract_expr()],
  NewSystem :: cauder_types:system(),
  StepsDone :: non_neg_integer().

step_over(Sem, Sys, Pid, Es) ->
  RecStep =
    fun Name(Sys0, Steps) ->
      Sys1 =
        try
          Sem:step(Sys0, Pid)
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


-spec step_multiple(Semantics, System, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  Semantics :: cauder_types:semantics(),
  System :: cauder_types:system(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

step_multiple(_, Sys, Steps, Steps) -> {Sys, Steps};
step_multiple(Sem, Sys, Steps, StepsDone) ->
  case Sem:options(Sys) of
    [] -> {Sys, StepsDone};
    [#opt{pid = Pid, sem = Sem} | _] ->
      Sys1 = Sem:step(Sys, Pid),
      step_multiple(Sem, Sys1, Steps, StepsDone + 1)
  end.


-spec replay_steps(System, Pid, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

replay_steps(Sys0, _, Steps, Steps) -> {Sys0, Steps};
replay_steps(Sys0, Pid, Steps, StepsDone) ->
  case cauder_replay:can_replay_step(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = cauder_replay:replay_step(Sys0, Pid),
      replay_steps(Sys1, Pid, Steps, StepsDone + 1)
  end.


-spec rollback_steps(System, Pid, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  StepsDone :: non_neg_integer(),
  NewSystem :: cauder_types:system(),
  NewStepsDone :: non_neg_integer().

rollback_steps(Sys0, _, Steps, Steps) -> {Sys0, Steps};
rollback_steps(Sys0, Pid, Steps, StepsDone) ->
  case cauder_rollback:can_rollback_step(Sys0, Pid) of
    false -> {Sys0, StepsDone};
    true ->
      Sys1 = cauder_rollback:rollback_step(Sys0, Pid),
      rollback_steps(Sys1, Pid, Steps, StepsDone + 1)
  end.
