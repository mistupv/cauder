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
-export([
    subscribe/0, subscribe/1,
    unsubscribe/0, unsubscribe/1
]).
-export([load_file/1, init_system/4, init_system/1, stop_system/0]).
-export([suspend_task/3, resume_task/0]).
% Manual action
-export([step/4]).
% Automatic actions
-export([step_multiple/3]).
% Replay actions
-export([
    replay_steps/2,
    replay_start/1,
    replay_spawn/1,
    replay_send/1,
    replay_receive/1,
    replay_register/1,
    replay_delete/1,
    replay_full_log/0
]).
% Rollback actions
-export([
    rollback_steps/2,
    rollback_start/1,
    rollback_spawn/1,
    rollback_send/1,
    %rollback_senda/1,
    rollback_receive/1,
    rollback_variable/1,
    rollback_reg/1,
    rollback_del/1
]).
-export([resume/1, cancel/0]).
-export([get_entry_points/1, get_system/0, get_path/0]).
-export([set_binding/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-elvis([{elvis_style, god_modules, disable}]).

-ignore_xref([main/0, main/1, start/0, start_link/0, stop/0]).
-ignore_xref([subscribe/0, subscribe/1, unsubscribe/0, unsubscribe/1]).

-define(SERVER, ?MODULE).

-include("cauder.hrl").
-include("cauder_system.hrl").
-include("cauder_process.hrl").
-include("cauder_message.hrl").
-include("cauder_tracer.hrl").
-include("cauder_semantics.hrl").

-record(state, {
    subs = [] :: [pid()],
    system :: cauder_system:system() | undefined,
    task :: {Name :: atom(), Pid :: pid(), State :: task_state()} | undefined
}).

-type state() :: #state{}.

-type task_state() :: running | suspended.

-type task_result() :: task_result({}).
-type task_result(Result) :: {
    task_completion(),
    Result,
    Time :: non_neg_integer(),
    NewSystem :: cauder_system:system()
}.

-type task_completion() :: success | cancel | failure.

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
%% @doc Stops the debugging server

-spec stop() -> ok.

stop() -> gen_server:stop(?SERVER).

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Subscribes the calling process to receive information about system
%% changes.

-spec subscribe() -> ok.

subscribe() -> subscribe(self()).

%%------------------------------------------------------------------------------
%% @doc Subscribes the process with the given `Pid' to receive information about
%% system changes.

-spec subscribe(Pid) -> ok when
    Pid :: pid().

subscribe(Pid) -> gen_server:call(?SERVER, {subscribe, Pid}).

%%------------------------------------------------------------------------------
%% @doc Unsubscribes the calling process to receive information about system
%% changes.

-spec unsubscribe() -> ok.

unsubscribe() -> unsubscribe(self()).

%%------------------------------------------------------------------------------
%% @doc Unsubscribes the process with the given `Pid' to receive information about
%% system changes.

-spec unsubscribe(Pid) -> ok when
    Pid :: pid().

unsubscribe(Pid) -> gen_server:call(?SERVER, {unsubscribe, Pid}).

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
    CurrentSystem :: cauder_system:system().

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

-spec init_system(Node, Module, Function, Arguments) -> Reply when
    Node :: node(),
    Module :: module(),
    Function :: atom(),
    Arguments :: cauder_syntax:af_args(),
    Reply :: ok | busy.

init_system(Node, Mod, Fun, Args) ->
    case gen_server:call(?SERVER, {user, {start_manual, {Node, {Mod, Fun, Args}}}}) of
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
    case gen_server:call(?SERVER, {user, {start_replay, Path}}) of
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
    CurrentSystem :: cauder_system:system().

stop_system() -> gen_server:call(?SERVER, {user, stop}).

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

-spec step(Pid, Semantics, Steps, Scheduler) -> Reply when
    Semantics :: cauder_semantics:semantics(),
    Pid :: cauder_process:id(),
    Steps :: pos_integer(),
    Scheduler :: cauder_message:scheduler(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

step(Pid, Sem, Steps, Scheduler) -> gen_server:call(?SERVER, {user, {step, {Sem, Pid, Steps, Scheduler}}}).

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

-spec step_multiple(Semantics, Steps, Scheduler) -> Reply when
    Semantics :: cauder_semantics:semantics(),
    Steps :: pos_integer(),
    Scheduler :: cauder_process:scheduler(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

step_multiple(Sem, Steps, Scheduler) -> gen_server:call(?SERVER, {user, {step_multiple, {Sem, Steps, Scheduler}}}).

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
    Pid :: cauder_process:id(),
    Steps :: pos_integer(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

replay_steps(Pid, Steps) -> gen_server:call(?SERVER, {user, {replay_steps, {Pid, Steps}}}).

%%------------------------------------------------------------------------------
%% @doc Replays the start of the node with the given name.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_spawn/2

-spec replay_start(Node) -> Reply when
    Node :: node(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

replay_start(Node) -> gen_server:call(?SERVER, {user, {replay_start, Node}}).

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
    Pid :: cauder_process:id(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

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
    Uid :: cauder_message:uid(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

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
    Uid :: cauder_message:uid(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

replay_receive(Uid) -> gen_server:call(?SERVER, {user, {replay_receive, Uid}}).

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_register/2

-spec replay_register(El) -> Reply when
    El :: cauder_map:map_element(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

replay_register({A,B,C}) -> gen_server:call(?SERVER, {user, {replay_register, {A,B,C,top}}}).

%%------------------------------------------------------------------------------
%% @doc Replays the reception of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_replay_delete/2

-spec replay_delete(El) -> Reply when
    El :: cauder_map:map_element(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

replay_delete({A,B,C}) -> gen_server:call(?SERVER, {user, {replay_delete, {A,B,C,bot}}}).

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
    CurrentSystem :: cauder_system:system().

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
    Pid :: cauder_process:id(),
    Steps :: pos_integer(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_steps(Pid, Steps) -> gen_server:call(?SERVER, {user, {rollback_steps, {Pid, Steps}}}).

%%------------------------------------------------------------------------------
%% @doc Rolls back the start of the node with the given name.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_start/2

-spec rollback_start(Node) -> Reply when
    Node :: node(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_start(Node) -> gen_server:call(?SERVER, {user, {rollback_start, Node}}).

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
    Pid :: cauder_process:id(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

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
    Uid :: cauder_message:uid(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_send(Uid) -> gen_server:call(?SERVER, {user, {rollback_send, Uid}}).

%%------------------------------------------------------------------------------
%% @doc Rolls back the sending of the message with the given uid.
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_senda/2

%-spec rollback_senda(Uid) -> Reply when
%    Uid :: cauder_message:uid(),
%    Reply :: {ok, CurrentSystem} | busy,
%    CurrentSystem :: cauder_system:system().

%rollback_senda(Uid) -> gen_server:call(?SERVER, {user, {rollback_senda, Uid}}).

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
    Uid :: cauder_message:uid(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_receive(Uid) -> gen_server:call(?SERVER, {user, {rollback_receive, Uid}}).

%%------------------------------------------------------------------------------
%% @doc Rolls back
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_reg/2

-spec rollback_reg(El) -> Reply when
    El :: cauder_map:map_element(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_reg(El) -> gen_server:call(?SERVER, {user, {rollback_reg, El}}).

%%------------------------------------------------------------------------------
%% @doc Rolls back
%%
%% This is an asynchronous action: if the server accepts the task then the tuple
%% `{ok, CurrentSystem}' is returned, where `CurrentSystem' is the current
%% system prior to executing this action, otherwise the atom `busy' is returned,
%% to indicate that the server is currently executing a different task.
%%
%% @see task_rollback_del/2

-spec rollback_del(El) -> Reply when
    El :: cauder_map:map_element(),
    Reply :: {ok, CurrentSystem} | busy,
    CurrentSystem :: cauder_system:system().

rollback_del(El) -> gen_server:call(?SERVER, {user, {rollback_del, El}}).

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
    CurrentSystem :: cauder_system:system().

rollback_variable(Name) -> gen_server:call(?SERVER, {user, {rollback_variable, Name}}).

%%%=============================================================================

resume(MessageId) -> gen_server:call(?SERVER, {user, {resume, MessageId}}).

cancel() -> gen_server:call(?SERVER, {user, cancel}).

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
    System :: cauder_system:system() | undefined.

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
    Pid :: cauder_process:id(),
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
    % TODO Can this be not public?
    ?APP_DB = ets:new(?APP_DB, [set, public, named_table]),
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

handle_call({task, {suspend, Receiver, Messages, NewSystem}}, {Pid, _}, #state{task = {Task, Pid, running}} = State) ->
    notify_subscribers({suspend, Task, {Receiver, Messages}, NewSystem}, State#state.subs),
    {reply, ok, State#state{task = {Task, Pid, suspended}, system = NewSystem}};
handle_call({task, resume}, {Pid, _}, #state{task = {Task, Pid, suspended}} = State) ->
    notify_subscribers({resume, Task}, State#state.subs),
    {reply, ok, State#state{task = {Task, Pid, running}}};
handle_call({task, {cancel, Value, Time, NewSystem}}, {Pid, _}, #state{task = {Task, Pid, suspended}} = State) ->
    notify_subscribers({cancel, Task, Value, Time, NewSystem}, State#state.subs),
    {reply, ok, State#state{task = undefined, system = NewSystem}};
handle_call({task, {success, Value, Time, NewSystem}}, {Pid, _}, #state{task = {Task, Pid, running}} = State) ->
    notify_subscribers({success, Task, Value, Time, NewSystem}, State#state.subs),
    {reply, ok, State#state{task = undefined, system = NewSystem}};
handle_call({task, {failure, no_alive, Stacktrace}}, {Pid, _}, #state{task = {Task, Pid, running}} = State) ->
    notify_subscribers({failure, Task, no_alive, Stacktrace}, State#state.subs),
    cauder_wx:show_error("tried to start a node in a non-distributed system"),
    {reply, ok, State#state{task = undefined}};
handle_call({task, {failure, Reason, Stacktrace}}, {Pid, _}, #state{task = {Task, Pid, running}} = State) ->
    notify_subscribers({failure, Task, Reason, Stacktrace}, State#state.subs),
    {reply, ok, State#state{task = undefined}};
%%%=============================================================================

handle_call({user, {resume, MessageId}}, _From, #state{task = {_, Pid, suspended}} = State) ->
    Pid ! {resume, MessageId},
    {reply, ok, State};
handle_call({user, cancel}, _From, #state{task = {_, Pid, suspended}} = State) ->
    Pid ! cancel,
    {reply, ok, State};
%%%=============================================================================

handle_call({user, {get, {entry_points, Module}}}, _From, State) ->
    Defs = ets:match_object(?APP_DB, {{Module, '_', '_', '_'}, '_'}),
    LineComparator = fun({_, [{_, LineA, _, _, _} | _]}, {_, [{_, LineB, _, _, _} | _]}) -> LineA =< LineB end,
    SortedDefs = lists:sort(LineComparator, Defs),
    % TODO Only allow to start system from an exported function?
    EntryPoints = lists:map(fun({{M, F, A, _}, _}) -> {M, F, A} end, SortedDefs),
    {reply, EntryPoints, State};
handle_call({user, {get, system}}, _From, State) ->
    {reply, State#state.system, State};
handle_call({user, {get, path}}, _From, State) ->
    {reply, ets:lookup_element(?APP_DB, path, 2), State};
%%%=============================================================================

handle_call({user, stop}, {FromPid, _}, #state{system = System, task = Task} = State) ->
    case Task of
        {_, Pid, _} -> exit(Pid, kill);
        undefined -> ok
    end,
    ets:delete(?APP_DB, last_pid),
    ets:delete(?APP_DB, last_uid),
    ets:delete(?APP_DB, last_var),
    ets:delete(?APP_DB, last_key),
    % TODO Add dialog when UI receives this message
    [Sub ! {dbg, stop} || Sub <- State#state.subs, Sub =/= FromPid],
    {reply, {ok, System}, State#state{system = undefined, task = undefined}};
%%%=============================================================================

handle_call({user, _}, _From, #state{task = {_, _, _}} = State) ->
    {reply, busy, State};
%%%=============================================================================

handle_call({user, {set, {binding, Pid}, {Name, Value}}}, _From, State0) ->
    #state{system = Sys0} = State0,
    #system{pool = Pool0} = Sys0,
    Pool1 = cauder_pool:update_with(
        Pid,
        fun(P) -> cauder_process:add_binding(Name, Value, P) end,
        Pool0
    ),
    Sys1 = Sys0#system{pool = Pool1},
    State1 = State0#state{system = Sys1},
    {reply, ok, State1};
%%%=============================================================================

handle_call({user, {task, {resume, MessageId}}}, _From, #state{task = {_, Pid, suspended}} = State) ->
    Pid ! {resume, MessageId},
    {reply, ok, State};
handle_call({user, {task, cancel}}, _From, #state{task = {_, Pid, suspended}} = State) ->
    Pid ! cancel,
    {reply, ok, State};
handle_call({user, {Task, Args}}, _From, #state{system = System} = State) ->
    % IMPORTANT: Given a task 'example', the name of the task function must be
    % 'task_example' and its arity must be 2, where the first argument are the
    % arguments passed by the user and the second is the current system.
    Fun =
        case Task of
            load -> fun task_load/2;
            start_manual -> fun task_start_manual/2;
            start_replay -> fun task_start_replay/2;
            step -> fun task_step/2;
            step_multiple -> fun task_step_multiple/2;
            replay_steps -> fun task_replay_steps/2;
            replay_start -> fun task_replay_start/2;
            replay_spawn -> fun task_replay_spawn/2;
            replay_send -> fun task_replay_send/2;
            replay_receive -> fun task_replay_receive/2;
            replay_register -> fun task_replay_register/2;
            replay_delete -> fun task_replay_delete/2;
            replay_full_log -> fun task_replay_full_log/2;
            rollback_steps -> fun task_rollback_steps/2;
            rollback_start -> fun task_rollback_start/2;
            rollback_spawn -> fun task_rollback_spawn/2;
            rollback_send -> fun task_rollback_send/2;
            rollback_senda -> fun task_rollback_senda/2;
            rollback_receive -> fun task_rollback_receive/2;
            rollback_reg -> fun task_rollback_reg/2;
            rollback_del -> fun task_rollback_del/2;
            rollback_variable -> fun task_rollback_variable/2
        end,
    Pid = run_task(Fun, Args, System),
    {reply, {ok, System}, State#state{task = {Task, Pid, running}}};
%%%=============================================================================

handle_call(Request, _From, State) ->
    io:format("[~p:~p] Unhandled Call:~n~p~n", [?MODULE, ?LINE, Request]),
    {reply, ok, State}.

notify_subscribers(Message, Subs) ->
    lists:foreach(fun(Sub) -> Sub ! {dbg, Message} end, Subs).

%%------------------------------------------------------------------------------
%% @private

-spec handle_cast(Request, State) -> {noreply, NewState} when
    Request :: any(),
    State :: state(),
    NewState :: state().

handle_cast(Request, State) ->
    io:format("[~p:~p] Unhandled Cast:~n~p~n", [?MODULE, ?LINE, Request]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_info(Info, State) -> {noreply, NewState} when
    Info :: any(),
    State :: state(),
    NewState :: state().

handle_info(Info, State) ->
    io:format("[~p:~p] Unhandled Info:~n~p~n", [?MODULE, ?LINE, Info]),
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

-spec run_task(TaskFunction, Arguments, InitialSystem) -> TaskPid when
    TaskFunction :: fun((Arguments, InitialSystem) -> task_result(any())),
    Arguments :: term(),
    InitialSystem :: cauder_system:system(),
    TaskPid :: pid().

run_task(Task, Args, System) when is_function(Task, 2) ->
    spawn(
        fun() ->
            try Task(Args, System) of
                {_, _, _, _} = Result ->
                    ok = gen_server:call(?SERVER, {task, Result})
            catch
                error:Reason:Stacktrace ->
                    ok = gen_server:call(?SERVER, {task, {failure, Reason, Stacktrace}})
            end
        end
    ).

-spec suspend_task(Receiver, Messages, CurrentSystem) -> {SuspendTime, ({resume, MessageId} | cancel)} when
    Receiver :: cauder_process:id(),
    Messages :: [cauder_message:uid()],
    CurrentSystem :: cauder_system:system(),
    SuspendTime :: integer(),
    MessageId :: cauder_message:uid().

suspend_task(Receiver, Messages, System) ->
    ok = gen_server:call(?SERVER, {task, {suspend, Receiver, Messages, System}}),
    timer:tc(
        fun() ->
            receive
                Msg -> Msg
            end
        end
    ).

resume_task() ->
    ok = gen_server:call(?SERVER, {task, resume}).

%%%=============================================================================

-spec task_load(File, System) -> task_result({File, Module}) when
    File :: file:filename(),
    System :: cauder_system:system(),
    Module :: module().

task_load(File, System) ->
    {Time, {ok, Module}} = timer:tc(cauder_load, file, [File]),
    ets:insert(?APP_DB, {path, filename:absname(filename:dirname(File))}),

    {success, {File, Module}, Time, System}.

-spec task_start_manual({Node, MFA}, System :: undefined) -> task_result() when
    Node :: node(),
    MFA :: {Module, Function, Arguments},
    Module :: module(),
    Function :: atom(),
    Arguments :: [cauder_syntax:af_literal()].

task_start_manual({Node, {Mod, Fun, Args}}, undefined) ->
    {Time, System} =
        timer:tc(
            fun() ->
                Pid = cauder_process:new_pid(),
                Proc = #process{
                    node = Node,
                    pid = Pid,
                    expr = [cauder_syntax:remote_call(Mod, Fun, Args)],
                    mfa = {Mod, Fun, length(Args)}
                },
                #system{
                    pool = cauder_pool:new(Proc),
                    nodes = [Node]
                }
            end
        ),

    {success, {}, Time, System}.

-spec task_start_replay(TracePath, System :: undefined) -> task_result() when
    TracePath :: file:filename().

task_start_replay(TracePath, undefined) ->
    {Time, System} =
        timer:tc(
            fun() ->
                #trace_info{
                    node = Node,
                    pid = Pid,
                    call = {M, F, As},
                    trace = Trace
                } = cauder_utils:load_trace(TracePath),
                AbstractArgs = cauder_syntax:expr_list(lists:map(fun erl_parse:abstract/1, As)),
                P = #process{
                    node = Node,
                    pid = Pid,
                    mfa = {M, F, length(As)},
                    expr = [cauder_syntax:remote_call(M, F, AbstractArgs)]
                },
                #system{
                    pool = cauder_pool:new(P),
                    nodes = [Node],
                    log = cauder_trace:to_log(Trace)
                }
            end
        ),

    {success, {}, Time, System}.

%%%=============================================================================

-spec task_step({Semantics, Pid, Steps, Scheduler}, System) -> task_result({Semantics, {StepsDone, Steps}}) when
    Semantics :: cauder_semantics:semantics(),
    Pid :: cauder_process:id(),
    Steps :: non_neg_integer(),
    Scheduler :: cauder_message:scheduler(),
    System :: cauder_system:system(),
    StepsDone :: non_neg_integer().

task_step({Sem, Pid, Steps, Scheduler}, Sys0) ->
    {Time, {Completion, Sys1, StepsDone}} = timer:tc(fun() -> step(Sem, Scheduler, Pid, Sys0, Steps) end),

    {Completion, {Sem, {StepsDone, Steps}}, Time, Sys1}.

-spec task_step_multiple({Semantics, Steps, Scheduler}, System) -> task_result({Semantics, {StepsDone, Steps}}) when
    Semantics :: cauder_semantics:semantics(),
    Steps :: non_neg_integer(),
    Scheduler :: cauder_process:scheduler(),
    System :: cauder_system:system(),
    StepsDone :: non_neg_integer().

task_step_multiple({Sem, Steps, Scheduler}, Sys0) ->
    {Time, {Sys1, StepsDone}} = timer:tc(fun() -> step_multiple(Sem, Scheduler, Sys0, Steps) end),

    {success, {Sem, {StepsDone, Steps}}, Time, Sys1}.

%%%=============================================================================

-spec task_replay_steps({Pid, Steps}, System) -> task_result({StepsDone, Steps}) when
    Pid :: cauder_process:id(),
    Steps :: non_neg_integer(),
    System :: cauder_system:system(),
    StepsDone :: non_neg_integer().

task_replay_steps({Pid, Steps}, Sys0) ->
    {Time, {Sys1, StepsDone}} = timer:tc(fun() -> replay_steps(Pid, Sys0, Steps, 0) end),

    {success, {StepsDone, Steps}, Time, Sys1}.

-spec task_replay_start(Node, System) -> task_result(Node) when
    Node :: node(),
    System :: cauder_system:system().

task_replay_start(Node, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_start(Node, Sys0) of
                    false -> error(no_replay);
                    _ -> cauder_replay:replay_start(Node, Sys0)
                end
            end
        ),

    {success, Node, Time, Sys1}.

-spec task_replay_spawn(Pid, System) -> task_result(Pid) when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

task_replay_spawn(Pid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_spawn(Pid, Sys0) of
                    false -> error(no_replay);
                    true -> cauder_replay:replay_spawn(Pid, Sys0)
                end
            end
        ),

    {success, Pid, Time, Sys1}.

-spec task_replay_send(Uid, System) -> task_result(Uid) when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

task_replay_send(Uid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_send(Uid, Sys0) of
                    false -> error(no_replay);
                    true -> cauder_replay:replay_send(Uid, Sys0)
                end
            end
        ),

    {success, Uid, Time, Sys1}.

-spec task_replay_receive(Uid, System) -> task_result(Uid) when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

task_replay_receive(Uid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_receive(Uid, Sys0) of
                    false -> error(no_replay);
                    true -> cauder_replay:replay_receive(Uid, Sys0)
                end
            end
        ),

    {success, Uid, Time, Sys1}.

-spec task_replay_register(El, System) -> task_result(El) when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

task_replay_register(El, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_register(El, Sys0) of
                    false -> error(no_replay);
                    true -> cauder_replay:replay_register(El, Sys0)
                end
            end
        ),

    {success, El, Time, Sys1}.

-spec task_replay_delete(El, System) -> task_result(El) when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

task_replay_delete(El, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_replay:can_replay_delete(El, Sys0) of
                    false -> error(no_replay);
                    true -> cauder_replay:replay_delete(El, Sys0)
                end
            end
        ),

    {success, El, Time, Sys1}.

-spec task_replay_full_log([], System) -> task_result() when
    System :: cauder_system:system().

task_replay_full_log([], Sys0) ->
    {Time, Sys1} = timer:tc(fun() -> replay_full_log(Sys0) end),

    {success, {}, Time, Sys1}.

%%%=============================================================================

-spec task_rollback_steps({Pid, Steps}, System) -> task_result({StepsDone, Steps}) when
    Pid :: cauder_process:id(),
    Steps :: non_neg_integer(),
    System :: cauder_system:system(),
    StepsDone :: non_neg_integer().

task_rollback_steps({Pid, Steps}, Sys0) ->
    {Time, {Sys1, StepsDone}} = timer:tc(fun() -> rollback_steps(Pid, Sys0, Steps, 0) end),

    {success, {StepsDone, Steps}, Time, Sys1}.

-spec task_rollback_start(Node, System) -> task_result(Node) when
    Node :: node(),
    System :: cauder_system:system().

task_rollback_start(Node, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_start(Node, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_start(Node, Sys0)
                end
            end
        ),

    {success, Node, Time, Sys1}.

-spec task_rollback_spawn(Pid, System) -> task_result(Pid) when
    Pid :: cauder_process:id(),
    System :: cauder_system:system().

task_rollback_spawn(Pid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_spawn(Pid, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_spawn(Pid, Sys0)
                end
            end
        ),

    {success, Pid, Time, Sys1}.

-spec task_rollback_send(Uid, System) -> task_result(Uid) when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

task_rollback_send(Uid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_send(Uid, Sys0) of
                    false ->
                        case cauder_rollback:can_rollback_senda(Uid, Sys0) of
                            false -> error(no_rollback);
                            true -> cauder_rollback:rollback_senda(Uid, Sys0)
                        end;
                    true ->
                        cauder_rollback:rollback_send(Uid, Sys0)
                end
            end
        ),

    {success, Uid, Time, Sys1}.

-spec task_rollback_senda(Uid, System) -> task_result(Uid) when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

task_rollback_senda(Uid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_senda(Uid, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_senda(Uid, Sys0)
                end
            end
        ),

    {success, Uid, Time, Sys1}.

-spec task_rollback_receive(Uid, System) -> task_result(Uid) when
    Uid :: cauder_message:uid(),
    System :: cauder_system:system().

task_rollback_receive(Uid, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_receive(Uid, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_receive(Uid, Sys0)
                end
            end
        ),

    {success, Uid, Time, Sys1}.

-spec task_rollback_reg(El, System) -> task_result(El) when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

task_rollback_reg(El, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_reg(El, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_reg(El, Sys0)
                end
            end
        ),

    {success, El, Time, Sys1}.

-spec task_rollback_del(El, System) -> task_result(El) when
    El :: cauder_map:map_element(),
    System :: cauder_system:system().

task_rollback_del(El, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_del(El, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_del(El, Sys0)
                end
            end
        ),

    {success, El, Time, Sys1}.

-spec task_rollback_variable(Name, System) -> task_result(Name) when
    Name :: atom(),
    System :: cauder_system:system().

task_rollback_variable(Name, Sys0) ->
    {Time, Sys1} =
        timer:tc(
            fun() ->
                case cauder_rollback:can_rollback_variable(Name, Sys0) of
                    false -> error(no_rollback);
                    true -> cauder_rollback:rollback_variable(Name, Sys0)
                end
            end
        ),

    {success, Name, Time, Sys1}.

%%%=============================================================================

-spec step(Semantics, Scheduler, Pid, System, Steps) -> {Completion, NewSystem, StepsDone} when
    Semantics :: cauder_semantics:semantics(),
    Scheduler :: cauder_message:scheduler(),
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Steps :: pos_integer(),
    Completion :: success | cancel,
    NewSystem :: cauder_system:system(),
    StepsDone :: non_neg_integer().

step(Sem, Scheduler, Pid, Sys, Steps) ->
    Fun =
        case Sem of
            ?SEM_FWD ->
                fun(Step, Sys0) ->
                    Opts = cauder_semantics_forwards:options(Sys0, normal),
                    case maps:is_key(Pid, Opts) of
                        false ->
                            throw({success, Sys0, Step});
                        true ->
                            try
                                cauder_semantics_forwards:step(Pid, Sys0, Scheduler, normal)
                            catch
                                throw:cancel -> throw({cancel, Sys0, Step})
                            end
                    end
                end;
            ?SEM_BWD ->
                fun(Step, Sys0) ->
                    Opts = cauder_semantics_backwards:options(Sys0),
                    case maps:is_key(Pid, Opts) of
                        false ->
                            throw({success, Sys0, Step});
                        true ->
                            cauder_semantics_backwards:step(Pid, Sys0)
                    end
                end
        end,
    try lists:foldl(Fun, Sys, lists:seq(0, Steps - 1)) of
        Sys1 -> {success, Sys1, Steps}
    catch
        throw:{_, _, _} = Result -> Result
    end.

-spec step_multiple(Semantics, Scheduler, System, Steps) -> {NewSystem, StepsDone} when
    Semantics :: cauder_semantics:semantics(),
    Scheduler :: cauder_process:scheduler(),
    System :: cauder_system:system(),
    Steps :: pos_integer(),
    NewSystem :: cauder_system:system(),
    StepsDone :: non_neg_integer().

step_multiple(Sem, Scheduler, Sys, Steps) ->
    SchedFun = cauder_scheduler:get(Scheduler),
    DoStep =
        fun(Step, {Sys0, Set0, PidQueue0}) ->
            Opts =
                case Sem of
                    ?SEM_FWD -> cauder_semantics_forwards:options(Sys0, normal);
                    ?SEM_BWD -> cauder_semantics_backwards:options(Sys0)
                end,
            case maps:keys(Opts) of
                [] ->
                    throw({Sys0, Step});
                Pids ->
                    Set1 = sets:from_list(Pids),
                    Change =
                        case {sets:size(Set0), sets:size(Set1)} of
                            {0, _} ->
                                {init, Pids};
                            {Size0, Size1} when Size0 < Size1 ->
                                [AddedPid] = sets:to_list(sets:subtract(Set1, Set0)),
                                {add, AddedPid};
                            {Size0, Size1} when Size0 > Size1 ->
                                [RemovedPid] = sets:to_list(sets:subtract(Set0, Set1)),
                                {remove, RemovedPid};
                            {Size, Size} ->
                                case
                                    {
                                        sets:to_list(sets:subtract(Set1, Set0)),
                                        sets:to_list(sets:subtract(Set0, Set1))
                                    }
                                of
                                    {[], []} ->
                                        none;
                                    {[AddedPid], [RemovedPid]} ->
                                        {update, AddedPid, RemovedPid}
                                end
                        end,
                    {Pid, PidQueue1} = SchedFun(PidQueue0, Change),
                    Sys1 =
                        case Sem of
                            ?SEM_FWD -> cauder_semantics_forwards:step(Pid, Sys0, ?SCHEDULER_Random, normal);
                            ?SEM_BWD -> cauder_semantics_backwards:step(Pid, Sys0)
                        end,
                    {Sys1, Set1, PidQueue1}
            end
        end,
    try lists:foldl(DoStep, {Sys, sets:new(), queue:new()}, lists:seq(0, Steps - 1)) of
        {Sys1, _, _} -> {Sys1, Steps}
    catch
        throw:{Sys1, StepsDone} -> {Sys1, StepsDone}
    end.

%%%=============================================================================

-spec replay_steps(Pid, System, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Steps :: pos_integer(),
    StepsDone :: non_neg_integer(),
    NewSystem :: cauder_system:system(),
    NewStepsDone :: non_neg_integer().

replay_steps(_, Sys0, Steps, Steps) ->
    {Sys0, Steps};
replay_steps(Pid, Sys0, Steps, StepsDone) ->
    case cauder_replay:can_replay_step(Pid, Sys0) of
        false ->
            {Sys0, StepsDone};
        true ->
            Sys1 = cauder_replay:replay_step(Pid, Sys0),
            replay_steps(Pid, Sys1, Steps, StepsDone + 1)
    end.

-spec replay_full_log(System) -> NewSystem when
    System :: cauder_system:system(),
    NewSystem :: cauder_system:system().

replay_full_log(#system{log = Log} = Sys) ->
    case cauder_log:pids(Log) of
        [] ->
            Sys;
        [Pid | _] ->
            Sys1 = cauder_replay:replay_step(Pid, Sys),
            replay_full_log(Sys1)
    end.

%%%=============================================================================

-spec rollback_steps(Pid, System, Steps, StepsDone) -> {NewSystem, NewStepsDone} when
    Pid :: cauder_process:id(),
    System :: cauder_system:system(),
    Steps :: pos_integer(),
    StepsDone :: non_neg_integer(),
    NewSystem :: cauder_system:system(),
    NewStepsDone :: non_neg_integer().

rollback_steps(_, Sys0, Steps, Steps) ->
    {Sys0, Steps};
rollback_steps(Pid, Sys0, Steps, StepsDone) ->
    case cauder_rollback:can_rollback_step(Pid, Sys0) of
        false ->
            {Sys0, StepsDone};
        true ->
            Sys1 = cauder_rollback:rollback_step(Pid, Sys0),
            rollback_steps(Pid, Sys1, Steps, StepsDone + 1)
    end.
