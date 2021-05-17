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
    tracing :: success | timeout,
    result :: undefined | {value, term()},
    comp :: non_neg_integer(),
    exec :: non_neg_integer(),
    log = #{} :: cauder_types:log_map()
}).

-type trace() :: #trace{}.

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

trace(Mod, Fun, Args) -> trace(Mod, Fun, Args, []).

%%------------------------------------------------------------------------------
%% @doc Traces the evaluation of the expression `apply(Mod, Fun, Args)'.

-spec trace(Module, Function, Arguments, Options) -> Trace when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: [Timeout | Dir | StampMode],
    Timeout :: {timeout, timeout()},
    Dir :: {dir, file:filename()},
    StampMode :: {stamp_mode, distributed | centralized},
    Trace :: trace().

trace(Mod, Fun, Args, Opts) ->
    DefaultOpts = [
        {timeout, 10000},
        {dir, "."},
        {mods, []},
        {stamp_mode, centralized}
    ],
    do_trace(Mod, Fun, Args, Opts ++ DefaultOpts).

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
    Table = ets:new(?MODULE, []),
    Set = sets:new(),
    {ok, #state{main_pid = MainPid, ets = Table, procs = sets:add_element(TracedPid, Set)}}.

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
    {reply, ok, State1};
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
handle_call({trace, _, call, {?MODULE, run, [_, _, _]}}, _From, State) ->
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

terminate(_Reason, _State) ->
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
    Options :: [term()],
    Trace :: trace().

do_trace(Module, Function, Args, Opts) ->
    Timeout = proplists:get_value(timeout, Opts),
    Dir = proplists:get_value(dir, Opts),
    Mods = proplists:get_value(mods, Opts),
    StampMode = proplists:get_value(stamp_mode, Opts),
    Distributed = proplists:get_bool(distributed, Opts),

    [_Name, Host] = string:lexemes(atom_to_list(node()), [$@]),

    % TODO detached?
    % TODO node name
    {ok, TracedNode} = slave:start_link(Host, trace),

    {CompTime, {Module, Binary}} = instrument(Module, Dir, StampMode),
    Filename = filename:absname(filename:join(Dir, atom_to_list(Module) ++ ".beam")),
    load(TracedNode, Module, Binary, Filename),
    load(TracedNode, tracer_erlang),

    MainPid = self(),
    TracedPid = spawn_link(TracedNode, tracer_erlang, start, [MainPid, Module, Function, Args]),
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPid, TracedPid}, []),

    dbg:tracer(process, {fun trace_handler/2, []}),
    dbg:n(TracedNode),
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

    % dbg:tpl modules to instrument

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
    slave:stop(TracedNode),

    {ok, Trace} = gen_server:call(?SERVER, get_trace),
    Result = gen_server:call(?SERVER, get_result),
    gen_server:stop(?SERVER),

    Node =
        case Distributed of
            true -> TracedNode;
            false -> 'nonode@nohost'
        end,

    #trace{
        call = {Module, Function, Args},
        comp = CompTime,
        node = Node,
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

instrument(Module, Dir, StampMode) ->
    CompileOpts = [
        % Standard compile options
        binary,
        return,
        {i, Dir},
        {parse_transform, tracer_transform},
        % Tracer options
        {stamp_mode, StampMode},
        {inst_mod, get(modules_to_instrument)}
    ],
    File = filename:join(Dir, atom_to_list(Module)),
    {Time, {ok, Module, Binary, _}} = timer:tc(compile, file, [File, CompileOpts]),
    {Time, {Module, Binary}}.

-spec load(Node, Module) -> ok when
    Node :: node(),
    Module :: module().

load(Node, Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    load(Node, Module, Binary, Filename).

-spec load(Node, Module, Binary, Filename) -> ok when
    Node :: node(),
    Module :: module(),
    Binary :: binary(),
    Filename :: file:filename().

load(Node, Module, Binary, Filename) ->
    rpc:call(Node, code, purge, [Module]),
    {module, Module} = rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
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
