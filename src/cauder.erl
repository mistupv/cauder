%%%-----------------------------------------------------------------------------
%%% @doc CauDEr debugging server.
%%% This module includes functions for starting the server and interacting with
%%% it using the reversible semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder).

-behaviour(gen_server).

%% API
-export([main/0, main/1, start/0, start_link/0, stop/0]).
-export([subscribe/0, unsubscribe/0]).
-export([load_file/1, init_system/3, init_system/1, stop_system/0]).
-export([eval_opts/1]).
-export([step/2, step_over/2]).
-export([step_multiple/2]).
-export([replay_steps/2, replay_send/1, replay_spawn/1, replay_receive/1, replay_full_log/0]).
-export([rollback_steps/2, rollback_send/1, rollback_spawn/1, rollback_receive/1, rollback_variable/1]).
-export([get_entry_points/1, get_system/0, get_path/0]).
-export([set_binding/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("cauder.hrl").

-record(state, {
  subs = [] :: [pid()],
  system :: cauder_types:system() | undefined,
  task :: {atom(), pid()} | undefined
}).

-type state() :: #state{}.


%%%=============================================================================
%%% API
%%%=============================================================================


main() -> main([]).

main([]) ->
  application:load(cauder),
  {ok, _} = cauder:start_link(),
  case cauder_wx:start_link() of
    {ok, _, WxObject} -> cauder_wx:wait_forever(WxObject);
    Error -> cauder_wx:show_error(Error)
  end.


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
%% @doc Subscribes the calling process to receive information about system
%% changes.

-spec subscribe() -> ok.

subscribe() -> gen_server:call(?SERVER, {subscribe, self()}).


%%------------------------------------------------------------------------------
%% @doc Unsubscribes the calling process to receive information about system
%% changes.

-spec unsubscribe() -> ok.

unsubscribe() -> gen_server:call(?SERVER, {unsubscribe, self()}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Loads the given file (module).
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_load/2

-spec load_file(File) -> Reply when
  File :: file:filename(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

load_file(File) -> gen_server:call(?SERVER, {user, {load, File}}).


%%------------------------------------------------------------------------------
%% @doc Initializes the system in manual mode.
%% The system starts with a call to the given function, from the given module,
%% with the given arguments.
%%
%% This is an asynchronous action: this functions returns the atom `ok' if the
%% server accepted the task, or the atom `busy' if the server is currently
%% executing a different task.
%%
%% @see task_start/2

-spec init_system(Module, Function, Arguments) -> Reply when
  Module :: module(),
  Function :: atom(),
  Arguments :: cauder_types:af_args(),
  Reply :: ok | busy.

init_system(Mod, Fun, Args) ->
  case gen_server:call(?SERVER, {user, {start, {Mod, Fun, Args}}}) of
    {ok, _} -> ok;
    busy -> busy
  end.


%%------------------------------------------------------------------------------
%% @doc Initializes the system in replay mode.
%% The system starts with a call to the function specified in the specified
%% replay data.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_start/2

-spec init_system(LogPath) -> Reply when
  LogPath :: file:filename(),
  Reply :: ok | busy.

init_system(Path) ->
  case gen_server:call(?SERVER, {user, {start, Path}}) of
    {ok, _} -> ok;
    busy -> busy
  end.


%%------------------------------------------------------------------------------
%% @doc Stops the system and any running task in the server, and returns the
%% current system.
%%
%% This is a synchronous action.
%%
%% All the subscribers excluding the one who called this function (if a
%% subscriber) will be notified.

-spec stop_system() -> {ok, CurrentSystem} when
  CurrentSystem :: cauder_types:system().

stop_system() -> gen_server:call(?SERVER, {user, stop}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation options for the given system.
%%
%% This is a synchronous action.
%%
%% @todo Remove argument

-spec eval_opts(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

eval_opts(Sys) -> cauder_semantics_forwards:options(Sys) ++ cauder_semantics_backwards:options(Sys).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a step in the given process using the given semantics.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_step/2

-spec step(Semantics, Pid) -> Reply when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

step(Sem, Pid) -> gen_server:call(?SERVER, {user, {step, {Sem, Pid}}}).


%%------------------------------------------------------------------------------
%% @doc Steps over to the next line in the given process using the given
%% semantics.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_step_over/2
%%
%% @todo Currently not in use.

-spec step_over(Semantics, Pid) -> Reply when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

step_over(Sem, Pid) -> gen_server:call(?SERVER, {user, {step_over, {Sem, Pid}}}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs the given number of steps, in any process, using the given
%% semantics.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_step_multiple/2

-spec step_multiple(Semantics, Steps) -> Reply when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

step_multiple(Sem, Steps) -> gen_server:call(?SERVER, {user, {step_multiple, {Sem, Steps}}}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Replays the given number of steps in the given process.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_steps/2

-spec replay_steps(Pid, Steps) -> Reply when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

replay_steps(Pid, Steps) -> gen_server:call(?SERVER, {user, {replay_steps, {Pid, Steps}}}).


%%------------------------------------------------------------------------------
%% @doc Replays the spawning of the process with the given pid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_spawn/2

-spec replay_spawn(Pid) -> Reply when
  Pid :: cauder_types:proc_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

replay_spawn(Pid) -> gen_server:call(?SERVER, {user, {replay_spawn, Pid}}).


%%------------------------------------------------------------------------------
%% @doc Replays the sending of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_send/2

-spec replay_send(Uid) -> Reply when
  Uid :: cauder_types:msg_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

replay_send(Uid) -> gen_server:call(?SERVER, {user, {replay_send, Uid}}).


%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_receive/2

-spec replay_receive(Uid) -> Reply when
  Uid :: cauder_types:msg_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

replay_receive(Uid) -> gen_server:call(?SERVER, {user, {replay_receive, Uid}}).


%%------------------------------------------------------------------------------
%% @doc Replays the full log.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_full_log/2

-spec replay_full_log() -> Reply when
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

replay_full_log() -> gen_server:call(?SERVER, {user, {replay_full_log, []}}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Rolls back the given number of steps in the given process.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_steps/2

-spec rollback_steps(Pid, Steps) -> Reply when
  Pid :: cauder_types:proc_id(),
  Steps :: pos_integer(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

rollback_steps(Pid, Steps) -> gen_server:call(?SERVER, {user, {rollback_steps, {Pid, Steps}}}).


%%------------------------------------------------------------------------------
%% @doc Rolls back the spawning of the process with the given pid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_spawn/2

-spec rollback_spawn(Pid) -> Reply when
  Pid :: cauder_types:proc_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

rollback_spawn(Pid) -> gen_server:call(?SERVER, {user, {rollback_spawn, Pid}}).


%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_send/2

-spec rollback_send(Uid) -> Reply when
  Uid :: cauder_types:msg_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

rollback_send(Uid) -> gen_server:call(?SERVER, {user, {rollback_send, Uid}}).


%%------------------------------------------------------------------------------
%% @doc Rolls back the reception of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_receive/2

-spec rollback_receive(Uid) -> Reply when
  Uid :: cauder_types:msg_id(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

rollback_receive(Uid) -> gen_server:call(?SERVER, {user, {rollback_receive, Uid}}).


%%------------------------------------------------------------------------------
%% @doc Rolls back the binding of the variable with the given name.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_variable/2

-spec rollback_variable(Name) -> Reply when
  Name :: atom(),
  Reply :: {ok, CurrentSystem} | busy,
  CurrentSystem :: cauder_types:system().

rollback_variable(Name) -> gen_server:call(?SERVER, {user, {rollback_variable, Name}}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the possible entry points of the given module.
%%
%% This is a synchronous action.

-spec get_entry_points(Module) -> MFAs when
  Module :: module(),
  MFAs :: [mfa()].

get_entry_points(Module) -> gen_server:call(?SERVER, {user, {get, {entry_points, Module}}}).


%%------------------------------------------------------------------------------
%% @doc Returns the system.
%%
%% This is a synchronous action.

-spec get_system() -> System when
  System :: cauder_types:system() | undefined.

get_system() -> gen_server:call(?SERVER, {user, {get, system}}).


%%------------------------------------------------------------------------------
%% @doc Returns the current working directory.
%%
%% This is a synchronous action.

-spec get_path() -> Path when
  Path :: file:filename() | undefined.

get_path() -> gen_server:call(?SERVER, {user, {get, path}}).


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Sets a new binding for the variable with the given name in the given
%% process.
%%
%% This is a synchronous action: the atom `ok' is returned if this action
%% succeeds, however if a task is running when this function is called, this
%% action will not be performed, to avoid consistency problems, and the atom
%% `busy' will be returned instead.

-spec set_binding(Pid, {Key, Value}) -> ok | busy when
  Pid :: cauder_types:proc_id(),
  Key :: atom(),
  Value :: term().

set_binding(Pid, {Key, Value}) -> gen_server:call(?SERVER, {user, {set, {binding, Pid}, {Key, Value}}}).


%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @private

-spec init(Args) -> {ok, State} when
  Args :: term(),
  State :: state().

init([]) ->
  ?APP_DB = ets:new(?APP_DB, [set, public, named_table]), % TODO Can this be not public?
  {ok, #state{}}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} when
  Request :: term(),
  From :: {pid(), term()},
  State :: state(),
  Reply :: Reply,
  NewState :: state().


handle_call({subscribe, Sub}, _From, #state{subs = Subs} = State) ->
  {reply, ok, State#state{subs = [Sub | Subs]}};

handle_call({unsubscribe, Sub}, _From, #state{subs = Subs} = State) ->
  {reply, ok, State#state{subs = lists:delete(Sub, Subs)}};

%%%=============================================================================

handle_call({task, {finish, Value, Time, NewSystem}}, {Pid, _}, #state{subs = Subs, task = {Task, Pid}} = State) ->
  % Check finished task matches running task
  Task =
    if
      is_tuple(Value) -> element(1, Value);
      true -> Value
    end,
  lists:foreach(fun(Sub) -> Sub ! {dbg, {finish, Value, Time, NewSystem}} end, Subs),
  {reply, ok, State#state{system = NewSystem, task = undefined}};

handle_call({task, {fail, Reason}}, {Pid, _}, #state{subs = Subs, task = {Task, Pid}} = State) ->
  lists:foreach(fun(Sub) -> Sub ! {dbg, {fail, Task, Reason}} end, Subs),
  {reply, ok, State#state{task = undefined}};

%%%=============================================================================

handle_call({user, {get, {entry_points, Module}}}, _From, State) ->
  Defs = ets:match_object(?APP_DB, {{Module, '_', '_', '_'}, '_'}),
  SortedDefs = lists:sort(fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end, Defs),
  % TODO Only allow to start system from an exported function?
  EntryPoints = lists:map(fun({{M, F, A, _}, _}) -> {M, F, A} end, SortedDefs),
  {reply, EntryPoints, State};

handle_call({user, {get, system}}, _From, State) -> {reply, State#state.system, State};

handle_call({user, {get, path}}, _From, State)   -> {reply, ets:lookup_element(?APP_DB, path, 2), State};

%%%=============================================================================

handle_call({user, stop}, {FromPid, _}, #state{subs = Subs, system = System, task = Task} = State) ->
  case Task of
    {_, Pid} -> exit(Pid, kill);
    undefined -> ok
  end,
  ets:delete(?APP_DB, last_pid),
  ets:delete(?APP_DB, last_uid),
  ets:delete(?APP_DB, last_var),
  [Sub ! {dbg, stop} || Sub <- Subs, Sub =/= FromPid], % TODO Add dialog when UI receives this message
  {reply, {ok, System}, State#state{system = undefined, task = undefined}};

%%%=============================================================================

handle_call({user, _}, _From, #state{task = {_, _}} = State) ->
  {reply, busy, State};

%%%=============================================================================

handle_call({user, {set, {binding, Pid}, {Key, NewValue}}}, _From, #state{system = Sys0} = State) ->
  #sys{procs = #{Pid := #proc{env = Bs} = P} = Ps} = Sys0,
  Sys1 = Sys0#sys{procs = Ps#{Pid := P#proc{env = Bs#{Key => NewValue}}}},
  {reply, ok, State#state{system = Sys1}};

%%%=============================================================================

handle_call({user, {Task, Args}}, _From, #state{system = System} = State) ->
  % IMPORTANT: Given a task 'example', the name of the task function must be
  % 'task_example' and its arity must be 2, where the first argument are the
  % arguments passed by the user and the second is the current system.
  Fun =
    case Task of
      load -> fun task_load/2;
      start -> fun task_start/2;
      step -> fun task_step/2;
      step_over -> fun task_step_over/2;
      step_multiple -> fun task_step_multiple/2;
      replay_steps -> fun task_replay_steps/2;
      replay_spawn -> fun task_replay_spawn/2;
      replay_send -> fun task_replay_send/2;
      replay_receive -> fun task_replay_receive/2;
      replay_full_log -> fun task_replay_full_log/2;
      rollback_steps -> fun task_rollback_steps/2;
      rollback_spawn -> fun task_rollback_spawn/2;
      rollback_send -> fun task_rollback_send/2;
      rollback_receive -> fun task_rollback_receive/2;
      rollback_variable -> fun task_rollback_variable/2
    end,
  Pid = run_task(Fun, Args, System),
  {reply, {ok, System}, State#state{task = {Task, Pid}}};

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


-spec run_task(Function, Arguments, System) -> TaskPid when
  Function :: function(),
  Arguments :: term(),
  System :: cauder_types:system(),
  TaskPid :: pid().

run_task(Fun, Args, System) when is_function(Fun, 2) ->
  spawn(
    fun
      () ->
        try Fun(Args, System) of
          {Value, Time, NewSystem} -> gen_server:call(?SERVER, {task, {finish, Value, Time, NewSystem}})
        catch
          error:Reason -> gen_server:call(?SERVER, {task, {fail, Reason}})
        end
    end
  ).


%%%=============================================================================


-spec task_load(File, System) -> {{load, File, Module}, Time, NewSystem} when
  File :: file:filename(),
  System :: cauder_types:system(),
  Module :: module(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_load(File, System) ->
  {Time, {ok, Module}} = timer:tc(cauder_load, file, [File]),
  ets:insert(?APP_DB, {path, filename:absname(filename:dirname(File))}),

  {{load, File, Module}, Time, System}.


-spec task_start({Module, Function, Arguments}, System) -> {start, Time, NewSystem} when
  Module :: module(),
  Function :: atom(),
  Arguments :: [cauder_types:af_literal()],
  System :: undefined, % Since we are starting the system, there is no current system
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system()
;               (LogPath, System) -> {start, Time, NewSystem} when
  LogPath :: file:filename(),
  System :: undefined, % Since we are starting the system, there is no current system
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_start({M, F, As}, undefined) ->
  {Time, System} =
    timer:tc(
      fun() ->
        Pid = cauder_utils:fresh_pid(),
        Proc = #proc{
          pid   = Pid,
          exprs = [cauder_syntax:remote_call(M, F, As)],
          spf   = {M, F, length(As)}
        },
        #sys{
          procs = #{Pid => Proc}
        }
      end
    ),

  {start, Time, System};

task_start(LogPath, undefined) ->
  {Time, System} =
    timer:tc(
      fun() ->
        #replay{log_path = LogPath, call = {M, F, As}, main_pid = Pid} = cauder_utils:load_replay_data(LogPath),
        Proc = #proc{
          pid   = Pid,
          exprs = [cauder_syntax:remote_call(M, F, As)],
          spf   = {M, F, length(As)}
        },
        #sys{
          procs = #{Pid => Proc},
          logs  = load_logs(LogPath)
        }
      end
    ),

  {start, Time, System}.


%%%=============================================================================


-spec task_step({Semantics, Pid}, System) -> {{step, Semantics, Rule}, Time, NewSystem} when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  System :: cauder_types:system(),
  Rule :: cauder_types:rule(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_step({Sem, Pid}, Sys0) ->
  {Time, {Rule, Sys1}} =
    timer:tc(
      fun() ->
        Opts = cauder_utils:filter_options(eval_opts(Sys0), Pid),
        {value, #opt{pid = Pid, sem = Sem, rule = Rule}} = lists:search(fun(Opt) -> Opt#opt.sem =:= Sem end, Opts),
        Sys1 = Sem:step(Sys0, Pid),
        {Rule, Sys1}
      end
    ),

  {{step, Sem, Rule}, Time, Sys1}.


-spec task_step_over({Semantics, Pid}, System) -> {{step_over, Semantics, StepsDone}, Time, NewSystem} when
  Semantics :: cauder_types:semantics(),
  Pid :: cauder_types:proc_id(),
  System :: cauder_types:system(),
  StepsDone :: non_neg_integer(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_step_over({Sem, Pid}, #sys{procs = PMap} = Sys0) ->
  #{Pid := #proc{exprs = Es}} = PMap,
  {Time, {Sys1, StepsDone}} =
    timer:tc(
      fun() ->
        step_over(Sem, Sys0, Pid, Es)
      end
    ),

  {{step_over, Sem, StepsDone}, Time, Sys1}.


-spec task_step_multiple({Semantics, Steps}, System) -> {{step_multiple, Semantics, {StepsDone, Steps}}, Time, NewSystem} when
  Semantics :: cauder_types:semantics(),
  Steps :: non_neg_integer(),
  System :: cauder_types:system(),
  StepsDone :: non_neg_integer(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_step_multiple({Sem, Steps}, Sys0) ->
  {Time, {Sys1, StepsDone}} =
    timer:tc(
      fun() ->
        step_multiple(Sem, Sys0, Steps, 0)
      end
    ),

  {{step_multiple, Sem, {StepsDone, Steps}}, Time, Sys1}.


%%%=============================================================================


-spec task_replay_steps({Pid, Steps}, System) -> {{replay_steps, {StepsDone, Steps}}, Time, NewSystem} when
  Pid :: cauder_types:proc_id(),
  Steps :: non_neg_integer(),
  System :: cauder_types:system(),
  StepsDone :: non_neg_integer(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_replay_steps({Pid, Steps}, Sys0) ->
  {Time, {Sys1, StepsDone}} =
    timer:tc(
      fun() ->
        replay_steps(Sys0, Pid, Steps, 0)
      end
    ),

  {{replay_steps, {StepsDone, Steps}}, Time, Sys1}.


-spec task_replay_spawn(Pid, System) -> {{replay_spawn, Pid}, Time, NewSystem} when
  Pid :: cauder_types:proc_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_replay_spawn(Pid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_replay:can_replay_spawn(Sys0, Pid) of
          false -> error(no_replay);
          true -> cauder_replay:replay_spawn(Sys0, Pid)
        end
      end
    ),

  {{replay_spawn, Pid}, Time, Sys1}.


-spec task_replay_send(Uid, System) -> {{replay_send, Uid}, Time, NewSystem} when
  Uid :: cauder_types:msg_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_replay_send(Uid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_replay:can_replay_send(Sys0, Uid) of
          false -> error(no_replay);
          true -> cauder_replay:replay_send(Sys0, Uid)
        end
      end
    ),

  {{replay_send, Uid}, Time, Sys1}.


-spec task_replay_receive(Uid, System) -> {{replay_receive, Uid}, Time, NewSystem} when
  Uid :: cauder_types:msg_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_replay_receive(Uid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_replay:can_replay_receive(Sys0, Uid) of
          false -> error(no_replay);
          true -> cauder_replay:replay_receive(Sys0, Uid)
        end
      end
    ),

  {{replay_receive, Uid}, Time, Sys1}.


-spec task_replay_full_log([], System) -> {replay_full_log, Time, NewSystem} when
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_replay_full_log([], Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        replay_full_log(Sys0)
      end
    ),

  {replay_full_log, Time, Sys1}.


%%%=============================================================================


-spec task_rollback_steps({Pid, Steps}, System) -> {{rollback_steps, {StepsDone, Steps}}, Time, NewSystem} when
  Pid :: cauder_types:proc_id(),
  Steps :: non_neg_integer(),
  System :: cauder_types:system(),
  StepsDone :: non_neg_integer(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_rollback_steps({Pid, Steps}, Sys0) ->
  {Time, {Sys1, StepsDone}} =
    timer:tc(
      fun() ->
        rollback_steps(Sys0, Pid, Steps, 0)
      end
    ),

  {{rollback_steps, {StepsDone, Steps}}, Time, Sys1}.


-spec task_rollback_spawn(Pid, System) -> {{rollback_spawn, Pid}, Time, NewSystem} when
  Pid :: cauder_types:proc_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_rollback_spawn(Pid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_rollback:can_rollback_spawn(Sys0, Pid) of
          false -> error(no_rollback);
          true -> cauder_rollback:rollback_spawn(Sys0, Pid)
        end
      end
    ),

  {{rollback_spawn, Pid}, Time, Sys1}.


-spec task_rollback_send(Uid, System) -> {{rollback_send, Uid}, Time, NewSystem} when
  Uid :: cauder_types:msg_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_rollback_send(Uid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_rollback:can_rollback_send(Sys0, Uid) of
          false -> error(no_rollback);
          true -> cauder_rollback:rollback_send(Sys0, Uid)
        end
      end
    ),

  {{rollback_send, Uid}, Time, Sys1}.


-spec task_rollback_receive(Uid, System) -> {{rollback_receive, Uid}, Time, NewSystem} when
  Uid :: cauder_types:msg_id(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_rollback_receive(Uid, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_rollback:can_rollback_receive(Sys0, Uid) of
          false -> error(no_rollback);
          true -> cauder_rollback:rollback_receive(Sys0, Uid)
        end
      end
    ),

  {{rollback_receive, Uid}, Time, Sys1}.


-spec task_rollback_variable(Name, System) -> {{rollback_variable, Name}, Time, NewSystem} when
  Name :: atom(),
  System :: cauder_types:system(),
  Time :: non_neg_integer(),
  NewSystem :: cauder_types:system().

task_rollback_variable(Name, Sys0) ->
  {Time, Sys1} =
    timer:tc(
      fun() ->
        case cauder_rollback:can_rollback_variable(Sys0, Name) of
          false -> error(no_rollback);
          true -> cauder_rollback:rollback_variable(Sys0, Name)
        end
      end
    ),

  {{rollback_variable, Name}, Time, Sys1}.


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
      #{Pid := #proc{exprs = Es1}} = Sys1#sys.procs,
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


-spec replay_full_log(System) -> NewSystem when
  System :: cauder_types:system(),
  NewSystem :: cauder_types:system().

replay_full_log(Sys0 = #sys{logs = LMap}) ->
  lists:foldl(
    fun
      ([], Sys) -> Sys;
      (Log, Sys) ->
        case lists:last(Log) of
          {spawn, Pid} -> cauder_replay:replay_spawn(Sys, Pid);
          {send, Uid} -> cauder_replay:replay_send(Sys, Uid);
          {'receive', Uid} -> cauder_replay:replay_receive(Sys, Uid)
        end
    end,
    Sys0,
    maps:values(LMap)
  ).


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


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Loads the logs for all the available processes.

-spec load_logs(LogPath) -> LogMap when
  LogPath :: file:filename(),
  LogMap :: cauder_types:log_map().

load_logs(LogPath) ->
  {ok, Filenames} = file:list_dir(LogPath),
  lists:foldl(
    fun(Filename, Map) ->
      case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
        {match, [StrPid]} ->
          Pid = list_to_integer(StrPid),
          Log = load_log(Pid, LogPath),
          Map#{Pid => Log};
        nomatch -> Map
      end
    end,
    maps:new(),
    Filenames
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
