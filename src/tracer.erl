-module(tracer).

-behaviour(gen_server).

%% API
-export([trace/3, trace/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([trace/3, trace/4]).

-include("tracer.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    main_pid :: pid(),
    ets :: ets:tid(),
    stamps = #{} :: #{integer() => non_neg_integer()},
    trace = #{} :: cauder_types:log_map(),
    procs :: sets:set(pid()),
    result :: undefined | {value, term()},
    slave_starters = #{} :: #{pid() => {pid(), node()}}
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
    Options :: [Timeout | Dir],
    Timeout :: {timeout, timeout()},
    Dir :: {dir, file:filename()},
    Trace :: trace().

trace(Mod, Fun, Args, Opts) ->
    DefaultOpts = [
        {timeout, 10000},
        {dir, "."},
        {mods, []},
        {stamp_mode, "central"}
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
%% Save return value
handle_call({trace, Pid, return_from, {?MODULE, run, 3}, Result}, _From, #state{result = undefined} = State) ->
    check_process(Pid, State),
    {reply, ok, State#state{result = {value, Result}}};
%% Generate message stamp
handle_call({trace, Pid, send, {?SEND_SENT, []}, {undefined, _}}, _From, State) ->
    check_process(Pid, State),
    Pid ! {?RECV_STAMP, erlang:unique_integer()},
    {reply, ok, State};
%% Send message
handle_call({trace, Pid, send, {{stamp, Stamp}, _Message}, _}, _From, State) ->
    check_process(Pid, State),
    State1 = trace_with_stamp(send, Pid, Stamp, State),
    {reply, ok, State1};
%% Deliver message
handle_call({trace, Pid, 'receive', {{stamp, Stamp}, _Message}}, _From, State) ->
    check_process(Pid, State),
    State1 = trace_with_stamp(deliver, Pid, Stamp, State),
    {reply, ok, State1};
%% Receive message
handle_call({trace, Pid, send, {?RECEIVE_EVALUATED, Stamp}, {undefined, _}}, _From, State) ->
    check_process(Pid, State),
    State1 = trace_with_stamp('receive', Pid, Stamp, State),
    {reply, ok, State1};
%% Spawn failed
handle_call({trace, Pid, spawn, ChildPid, {erts_internal, crasher, [Node, _, _, _, _, _]}}, _From, State) ->
    check_process(Pid, State),
    Idx = pid_index(Pid),
    ChildIdx = pid_index(ChildPid),
    Entry = {spawn, {Node, node(ChildPid), ChildIdx}, fail},
    Trace1 = maps:update_with(Idx, fun(L) -> [Entry | L] end, [Entry], State#state.trace),
    {reply, ok, State#state{trace = Trace1}};
%% Spawn succeeded
handle_call({trace, Pid, spawn, ChildPid, {_, _, _}}, _From, State) ->
    check_process(Pid, State),
    Idx = pid_index(Pid),
    ChildNode = node(ChildPid),
    ChildIdx = pid_index(ChildPid),
    Entry = {spawn, {ChildNode, ChildIdx}, success},
    Trace1 = maps:update_with(Idx, fun(L) -> [Entry | L] end, [Entry], State#state.trace),
    ProcSet1 = sets:add_element(ChildPid, State#state.procs),
    {reply, ok, State#state{trace = Trace1, procs = ProcSet1}};
%% Process exited
handle_call({trace, Pid, exit, _Reason}, _From, #state{main_pid = MainPid, procs = ProcSet0} = State) ->
    check_process(Pid, State),
    ProcSet1 = sets:del_element(Pid, ProcSet0),
    State1 = State#state{procs = ProcSet1},
    case sets:is_empty(ProcSet1) of
        true -> MainPid ! finished;
        false -> ok
    end,
    {reply, ok, State1};
%% ----- Unused -----
handle_call({trace, Pid, 'receive', {?RECV_STAMP, _}}, _From, State) ->
    check_process(Pid, State),
    {reply, ok, State};
handle_call({trace, Pid, call, {?MODULE, run, [_, _, _]}}, _From, State) ->
    check_process(Pid, State),
    {reply, ok, State};
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
    % FIXME
    Trace :: term().

do_trace(Module, Function, Args, Opts) ->
    Timeout = proplists:get_value(timeout, Opts),
    Dir = proplists:get_value(dir, Opts),
    Mods = proplists:get_value(mods, Opts),
    StampMode = proplists:get_value(stamp_mode, Opts),

    [_Name, Host] = string:lexemes(atom_to_list(node()), [$@]),
    % TODO detached?
    % TODO node name
    {ok, TracedNode} = slave:start_link(Host, trace, io_lib:format("-setcookie ~p", [erlang:get_cookie()])),

    {CompTime, {Module, Binary}} = instrument(Module, Dir, StampMode),
    Filename = filename:absname(filename:join(Dir, atom_to_list(Module) ++ ".beam")),
    reload(TracedNode, Module, Binary, Filename),

    MainPid = self(),
    TracedPid = execute(TracedNode, Module, Function, Args),

    gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPid, TracedPid}, []),
    dbg:tracer(process, {fun trace_handler/2, []}),
    dbg:n(TracedNode),
    dbg:p(TracedPid, [m, c, p, sos]),
    dbg:tpl(?MODULE, run, 3, [{'_', [], [{return_trace}]}]),

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

    #trace{
        call = {Module, Function, Args},
        comp = CompTime,
        node = TracedNode,
        pid = TracedPid,
        tracing = Tracing,
        result = Result,
        exec = ExecTime,
        log = Trace
    }.

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

reload(Node, Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    reload(Node, Module, Binary, Filename).

reload(Node, Module, Binary, Filename) ->
    rpc:call(Node, code, purge, [Module]),
    {module, Module} = rpc:call(Node, code, load_binary, [Module, Filename, Binary]),
    ok.

execute(Node, Module, Fun, Args) ->
    reload(Node, ?MODULE),
    MainPid = self(),
    F = fun() ->
        MainPid ! setup_complete,
        receive
            start -> ok
        end,
        run(Module, Fun, Args)
    end,
    spawn_link(Node, F).

%%------------------------------------------------------------------------------
%% @doc Wrapper function used to capture the return value of the traced function
%% using the `dbg'. See dbg:tpl/4 and `return_trace'.

run(Module, Function, Args) -> apply(Module, Function, Args).

trace_handler(Trace, []) ->
    ok = gen_server:call(?SERVER, Trace),
    [].

trace_with_stamp(Tag, Pid, Stamp, #state{ets = Table, stamps = StampMap0, trace = Trace0} = State) ->
    PidIndex = pid_index(Pid),
    {Uid, StampMap1} = get_uid(Stamp, StampMap0, Table),
    Entry = {Tag, Uid},
    Trace1 = maps:update_with(PidIndex, fun(L) -> [Entry | L] end, [Entry], Trace0),
    State#state{stamps = StampMap1, trace = Trace1}.

pid_index(Pid) ->
    [_Node, Index, _Serial] = string:lexemes(pid_to_list(Pid), "<.>"),
    list_to_integer(Index).

get_uid(Stamp, Map, Table) ->
    case maps:find(Stamp, Map) of
        error ->
            Uid = ets:update_counter(Table, uid, 1, {uid, -1}),
            NewMap = maps:put(Stamp, Uid, Map),
            {Uid, NewMap};
        {ok, Uid} ->
            {Uid, Map}
    end.

check_process(Pid, #state{procs = ProcSet}) ->
    true = sets:is_element(Pid, ProcSet),
    ok.
