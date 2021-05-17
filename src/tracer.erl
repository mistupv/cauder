-module(tracer).

-behaviour(gen_server).

%% API
-export([trace/3, trace/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([trace/3, trace/4]).

-define(SERVER, ?MODULE).

-record(state, {
    main_pid :: pid(),
    ets :: ets:tid(),
    stamps = #{} :: #{integer() => non_neg_integer()},
    trace = #{} :: cauder_types:log_map(),
    procs :: sets:set(pid()),
    result :: undefined | {value, term()},
    slave_starters = #{} :: #{pid() => {node(), pid()}}
}).

-type state() :: #state{}.

-record(trace, {
    % Initial node
    node :: node(),
    % Initial pid
    pid :: pid(),
    % Initial function call
    call :: {module(), atom(), [term()]},
    % Whether the execution completed or the timeout was reached
    tracing :: success | timeout,
    % Result of the function application
    result :: undefined | {value, term()},
    % Compile time in microseconds
    comp :: non_neg_integer(),
    % Execution time in microseconds
    exec :: non_neg_integer(),
    log = #{} :: cauder_types:log_map()
}).

-type trace() :: #trace{}.

-type some_options() :: #{
    dir => file:filename(),
    modules => [atom()],
    timeout => timeout(),
    stamp_mode => distributed | centralized
}.

-type all_options() :: #{
    dir := file:filename(),
    modules := [atom()],
    timeout := timeout(),
    stamp_mode := distributed | centralized
}.

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% @doc Equivalent to `trace(Mod, Fun, Args, [])'.
%%
%% @see trace/4

-spec trace(Module, Function, Arguments) -> Trace when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Trace :: trace().

trace(Mod, Fun, Args) -> trace(Mod, Fun, Args, #{}).

%%------------------------------------------------------------------------------
%% @doc Traces the evaluation of the expression `apply(Mod, Fun, Args)'.

-spec trace(Module, Function, Arguments, Options) -> Trace when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: some_options(),
    Trace :: trace().

trace(Mod, Fun, Args, Opts) -> do_trace(Mod, Fun, Args, maps:merge(default_options(), Opts)).

%%------------------------------------------------------------------------------
%% @doc Returns a map with the default value for each of the available options.

-spec default_options() -> all_options().

default_options() ->
    #{
        dir => ".",
        modules => [],
        timeout => 10000,
        stamp_mode => centralized
    }.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private

-spec init({MainPid, TracedPid}) -> {ok, State} when
    MainPid :: pid(),
    TracedPid :: pid(),
    State :: state().

init({MainPid, TracedPid}) ->
    {ok, #state{
        main_pid = MainPid,
        ets = ets:new(?MODULE, []),
        procs = sets:add_element(TracedPid, sets:new())
    }}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} when
    Request :: term(),
    From :: {pid(), term()},
    State :: state(),
    Reply :: Reply,
    NewState :: state().

handle_call(get_trace, _From, #state{trace = Trace} = State) ->
    ReversedTrace = maps:map(fun(_, V) -> lists:reverse(V) end, Trace),
    {reply, {ok, ReversedTrace}, State};
handle_call(get_result, _From, #state{result = Result} = State) ->
    {reply, Result, State};
%% ========== 'call' trace messages ========== %%
%% Generate message stamp
handle_call({trace, Pid, call, {tracer_erlang, send_centralized, [_, _]}}, _From, State) ->
    Pid ! {stamp, erlang:unique_integer()},
    {reply, ok, State};
%% ========== 'return_from' trace messages ========== %%
%% Save return value
handle_call({trace, _Pid, return_from, {tracer_erlang, apply, 3}, Result}, _From, #state{result = undefined} = State) ->
    {reply, ok, State#state{result = {value, Result}}};
%% Call to `nodes()`
handle_call({trace, Pid, return_from, {tracer_erlang, nodes, 0}, Nodes}, _From, State) ->
    State1 = add_to_trace(Pid, {nodes, Nodes}, State),
    {reply, ok, State1};
%% ========== 'send' trace messages ========== %%
%% Send message
handle_call({trace, Pid, send, {{stamp, Stamp}, _Message}, _}, _From, State) ->
    State1 = add_to_trace(Pid, send, Stamp, State),
    {reply, ok, State1};
%% Receive message
handle_call({trace, Pid, send, {receive_evaluated, Stamp}, {undefined, _}}, _From, State) ->
    State1 = add_to_trace(Pid, 'receive', Stamp, State),
    {reply, ok, State1};
%% Slave started
handle_call({trace, Pid, send, {result, {ok, Node}}, ParentPid}, _From, State) ->
    #{Pid := {Node, ParentPid}} = State#state.slave_starters,
    State1 = add_to_trace(ParentPid, {start, Node, success}, State),
    {reply, ok, State1};
%% Slave failed to start
handle_call({trace, Pid, send, {result, {error, _}}, ParentPid}, _From, State) ->
    #{Pid := {Node, ParentPid}} = State#state.slave_starters,
    State1 = add_to_trace(ParentPid, {start, Node, failure}, State),
    {reply, ok, State1};
%% ========== 'receive' trace messages ========== %%
%% Deliver message
handle_call({trace, Pid, 'receive', {{stamp, Stamp}, _Message}}, _From, State) ->
    State1 = add_to_trace(Pid, deliver, Stamp, State),
    {reply, ok, State1};
%% ========== 'spawn' trace messages ========== %%
%% Starting a slave
handle_call({trace, Pid, spawn, SlavePid, {slave, wait_for_slave, [Pid, _, _, Node, _, _, _]}}, _From, State) ->
    SlaveStarters1 = maps:put(SlavePid, {Node, Pid}, State#state.slave_starters),
    {reply, ok, State#state{slave_starters = SlaveStarters1}};
%% Spawn failed
handle_call({trace, Pid, spawn, ChildPid, {erts_internal, crasher, [ChildNode, _, _, _, _, _]}}, _From, State) ->
    ChildIdx = pid_index(ChildPid),
    State1 = add_to_trace(Pid, {spawn, {ChildNode, ChildIdx}, failure}, State),
    {reply, ok, State1};
%% Spawn succeeded
handle_call({trace, Pid, spawn, ChildPid, {_, _, _}}, _From, State) ->
    ChildNode = node(ChildPid),
    ChildIdx = pid_index(ChildPid),
    State1 = add_to_trace(Pid, {spawn, {ChildNode, ChildIdx}, success}, State),
    State2 = State1#state{procs = sets:add_element(ChildPid, State1#state.procs)},
    {reply, ok, State2};
%% ========== 'exit' trace messages ========== %%
%% Process exited
handle_call({trace, Pid, exit, _Reason}, _From, #state{main_pid = MainPid, procs = ProcSet0} = State) ->
    ProcSet1 = sets:del_element(Pid, ProcSet0),
    State1 = State#state{procs = ProcSet1},
    case sets:is_empty(ProcSet1) of
        true -> MainPid ! finished;
        false -> ok
    end,
    {reply, ok, State1};
%% ========== Ignored trace messages ========== %%
handle_call({trace, _, call, {tracer_erlang, apply, [_, _, _]}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, call, {tracer_erlang, nodes, []}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, return_from, {tracer_erlang, send_centralized, 2}, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, spawned, _, {_, _, _}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, register, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, unregister, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, link, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, getting_unlinked, _}, _From, State) ->
    {reply, ok, State};
%% ========== Unhandled trace messages ========== %%
handle_call(Request, _From, State) ->
    io:format("[~p:~p] Unhandled Call: ~p~n", [?MODULE, ?LINE, Request]),
    {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_cast(Request, State) -> {noreply, NewState} when
    Request :: any(),
    State :: state(),
    NewState :: state().

handle_cast(Request, State) ->
    io:format("[~p:~p] Unhandled Cast: ~p~n", [?MODULE, ?LINE, Request]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_info(Info, State) -> {noreply, NewState} when
    Info :: any(),
    State :: state(),
    NewState :: state().

handle_info(Info, State) ->
    io:format("[~p:~p] Unhandled Info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private

-spec terminate(Reason, State) -> ok when
    Reason :: any(),
    State :: state().

terminate(_Reason, State) ->
    ProcSet = State#state.procs,
    case sets:is_empty(ProcSet) of
        true -> ok;
        false -> lists:foreach(fun(P) -> true = erlang:exit(P, kill) end, sets:to_list(ProcSet))
    end,
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

-spec do_trace(Module, Function, Arguments, Options) -> Trace when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: all_options(),
    Trace :: trace().

do_trace(Module, Function, Args, Opts) ->
    Dir = maps:get(dir, Opts),
    Modules = maps:get(modules, Opts),
    Timeout = maps:get(timeout, Opts),
    StampMode = maps:get(stamp_mode, Opts),

    CompTime = lists:foldl(
        fun(Mod, AccTime) ->
            {Time, {Mod, Bin}} = instrument(Mod, Dir, StampMode),
            File = filename:absname(filename:join(Dir, [Mod, ".beam"])),
            ok = load(Mod, Bin, File),
            Time + AccTime
        end,
        0,
        lists:usort([Module | Modules])
    ),

    load(tracer_erlang),

    MainPid = self(),
    TracedPid = spawn_link(tracer_erlang, start, [MainPid, Module, Function, Args]),
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPid, TracedPid}, []),

    dbg:tracer(process, {fun trace_handler/2, []}),
    dbg:p(TracedPid, [m, c, p, sos]),
    dbg:tpe(send, [
        {['_', {{stamp, '_'}, '_'}], [], []},
        {[{undefined, '_'}, {receive_evaluated, '_'}], [], []},
        {['_', {result, {ok, '_'}}], [], []},
        {['_', {result, {error, '_'}}], [], []}
    ]),
    dbg:tpe('receive', [{['_', '_', {{stamp, '_'}, '_'}], [], []}]),
    dbg:tpl(tracer_erlang, apply, 3, [{'_', [], [{return_trace}]}]),
    dbg:tpl(tracer_erlang, send_centralized, 2, [{'_', [], [{return_trace}]}]),
    dbg:tpl(tracer_erlang, nodes, 0, [{'_', [], [{return_trace}]}]),

    receive
        setup_complete -> ok
    end,

    {ExecTime, Tracing} = timer:tc(
        fun() ->
            TracedPid ! start,
            receive
                finished -> success
            after Timeout -> timeout
            end
        end
    ),

    dbg:stop(),

    {ok, Trace} = gen_server:call(?SERVER, get_trace),
    Result = gen_server:call(?SERVER, get_result),
    gen_server:stop(?SERVER),

    #trace{
        call = {Module, Function, Args},
        comp = CompTime,
        node = node(),
        pid = TracedPid,
        tracing = Tracing,
        result = Result,
        exec = ExecTime,
        log = Trace
    }.

-spec instrument(Module, Dir, StampMode) -> {Time, {Module, Binary}} when
    Module :: module(),
    Dir :: file:filename(),
    % TODO Specify values
    StampMode :: atom(),
    Time :: non_neg_integer(),
    Binary :: binary().

instrument(Mod, Dir, StampMode) ->
    CompileOpts = [
        % Standard compile options
        binary,
        return,
        {i, Dir},
        {parse_transform, tracer_transform},
        % Tracer options
        {stamp_mode, StampMode}
    ],
    File = filename:join(Dir, Mod),
    {Time, {ok, Mod, Binary, _}} = timer:tc(compile, file, [File, CompileOpts]),
    {Time, {Mod, Binary}}.

-spec load(Module) -> ok when
    Module :: module().

load(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    load(Module, Binary, Filename).

-spec load(Module, Binary, Filename) -> ok when
    Module :: module(),
    Binary :: binary(),
    Filename :: file:filename().

load(Module, Binary, Filename) ->
    %rpc:call(Node, code, purge, [Module]),
    code:purge(Module),
    %rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
    {module, Module} = code:load_binary(Module, Filename, Binary),
    ok.

trace_handler(Trace, []) ->
    ok = gen_server:call(?SERVER, Trace),
    [].

-spec add_to_trace(Pid, Entry, State) -> NewState when
    Pid :: pid(),
    Entry :: cauder_types:log_entry(),
    State :: state(),
    NewState :: state().

add_to_trace(Pid, Entry, #state{trace = Trace0} = State) ->
    Index = pid_index(Pid),
    Trace1 = maps:update_with(Index, fun(Other) -> [Entry | Other] end, [Entry], Trace0),
    State#state{trace = Trace1}.

-spec add_to_trace(Pid, Tag, Stamp, State) -> NewState when
    Pid :: pid(),
    Tag :: send | deliver | 'receive',
    Stamp :: integer(),
    State :: state(),
    NewState :: state().

add_to_trace(Pid, Tag, Stamp, #state{ets = Table, stamps = StampMap0, trace = Trace0} = State) when is_atom(Tag) ->
    PidIndex = pid_index(Pid),
    {Uid, StampMap1} = get_uid(Stamp, StampMap0, Table),
    Entry = {Tag, Uid},
    Trace1 = maps:update_with(PidIndex, fun(L) -> [Entry | L] end, [Entry], Trace0),
    State#state{stamps = StampMap1, trace = Trace1}.

-spec pid_index(Pid) -> Index when
    Pid :: pid(),
    Index :: non_neg_integer().

pid_index(Pid) ->
    [_Node, Index, _Serial] = string:lexemes(pid_to_list(Pid), "<.>"),
    list_to_integer(Index).

-spec get_uid(Stamp, Map, Table) -> {Uid, NewMap} when
    Stamp :: integer(),
    Map :: #{integer() => non_neg_integer()},
    Table :: ets:tid(),
    Uid :: non_neg_integer(),
    NewMap :: #{integer() => non_neg_integer()}.

get_uid(Stamp, Map, Table) ->
    case maps:find(Stamp, Map) of
        error ->
            Uid = ets:update_counter(Table, uid, 1, {uid, -1}),
            NewMap = maps:put(Stamp, Uid, Map),
            {Uid, NewMap};
        {ok, Uid} ->
            {Uid, Map}
    end.
