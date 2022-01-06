-module(cauder_tracer).

-behaviour(gen_server).

%% API
-export([
    trace/3,
    trace/4
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-ignore_xref([trace/3, trace/4]).

-include("cauder_trace.hrl").
-include("cauder_tracer.hrl").

-define(SERVER, ?MODULE).

-type trace_info() :: #trace_info{}.

-record(state, {
    % The pid of the tracer process, used to notify when the execution of the traced process has finished
    main_pid :: pid(),
    ets :: ets:tid(),
    % A map between 'unique integers' and stamps
    stamps = maps:new() :: #{integer() => non_neg_integer()},
    % The trace
    trace = cauder_trace:new() :: cauder_trace:trace(),
    % A set with all the processes being traced
    processes :: sets:set(pid()),
    % The value returned by the function application
    return = none :: none | {value, term()},
    slave_starters = #{} :: #{pid() => {node(), pid()}}
}).

-type state() :: #state{}.

-type some_options() :: #{
    dir => file:filename(),
    modules => [atom()],
    timeout => timeout(),
    stamp_mode => distributed | centralized,
    output => file:filename()
}.

-type some_options_proplist() :: [
    {dir, file:filename()}
    | {modules, [atom()]}
    | {timeout, timeout()}
    | {stamp_mode, distributed | centralized}
    | {output, file:filename()}
].

-type all_options() :: #{
    dir := file:filename(),
    modules := [atom()],
    timeout := timeout(),
    stamp_mode := distributed | centralized,
    output := file:filename()
}.

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% @doc Equivalent to `trace(Mod, Fun, Args, #{})'.
%%
%% @see trace/4

-spec trace(Module, Function, Arguments) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    TraceInfo :: cauder_tracer:trace_info().

trace(Mod, Fun, Args) -> trace(Mod, Fun, Args, #{}).

%%------------------------------------------------------------------------------
%% @doc Traces the evaluation of the expression `apply(Mod, Fun, Args)'.

-spec trace(Module, Function, Arguments, Options) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: some_options() | some_options_proplist(),
    TraceInfo :: cauder_tracer:trace_info().

trace(Mod, Fun, Args, Opts) when is_list(Opts) -> trace(Mod, Fun, Args, maps:from_list(Opts));
trace(Mod, Fun, Args, Opts) when is_map(Opts) -> do_trace(Mod, Fun, Args, maps:merge(default_options(), Opts)).

%%------------------------------------------------------------------------------
%% @doc Returns a map with the default value for each of the available options.

-spec default_options() -> all_options().

default_options() ->
    #{
        dir => ".",
        modules => [],
        timeout => 10000,
        stamp_mode => centralized,
        output => []
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
        processes = sets:from_list([TracedPid])
    }}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} when
    Request :: term(),
    From :: {pid(), term()},
    State :: state(),
    Reply :: Reply,
    NewState :: state().

handle_call(get_traces, _From, #state{trace = Trace0} = State) ->
    Trace1 = cauder_trace:reverse_actions(Trace0),
    {reply, {ok, Trace1}, State};
handle_call(get_return_value, _From, #state{return = ReturnValue} = State) ->
    {reply, ReturnValue, State};
%% ========== 'call' trace messages ========== %%
%% Generate message stamp
handle_call({trace, Pid, call, {cauder_tracer_erlang, send_centralized, [_, _]}}, _From, State) ->
    Pid ! {stamp, erlang:unique_integer()},
    {reply, ok, State};
%% ========== 'return_from' trace messages ========== %%
%% Save return value
handle_call(
    {trace, _Pid, return_from, {cauder_tracer_erlang, apply, 3}, ReturnValue},
    _From,
    #state{return = none} = State
) ->
    {reply, ok, State#state{return = {value, ReturnValue}}};
%% Call to `nodes()`
handle_call({trace, Pid, return_from, {cauder_tracer_erlang, nodes, 0}, Nodes}, _From, State) ->
    Action = #trace_nodes{nodes = Nodes},
    State1 = add_to_trace(Pid, Action, State),
    {reply, ok, State1};
%% ========== 'send' trace messages ========== %%
%% Send message
handle_call({trace, Pid, send, {{stamp, Stamp}, _Message}, _}, _From, State0) ->
    {Uid, State1} = get_uid(Stamp, State0),
    Action = #trace_send{uid = Uid},
    State2 = add_to_trace(Pid, Action, State1),
    {reply, ok, State2};
%% Receive message
handle_call({trace, Pid, send, {receive_evaluated, Stamp}, {undefined, _}}, _From, State0) ->
    {Uid, State1} = get_uid(Stamp, State0),
    Action = #trace_receive{uid = Uid},
    State2 = add_to_trace(Pid, Action, State1),
    {reply, ok, State2};
%% Slave started/failed
handle_call({trace, Pid, send, {result, Result}, ParentPid}, _From, State) ->
    #{Pid := {Node, ParentPid}} = State#state.slave_starters,
    Success =
        case Result of
            {ok, Node} -> true;
            {error, _} -> false
        end,
    Action = #trace_start{node = Node, success = Success},
    State1 = add_to_trace(ParentPid, Action, State),
    {reply, ok, State1};
%% ========== 'spawn' trace messages ========== %%
%% Starting a slave
handle_call({trace, Pid, spawn, SlavePid, {slave, wait_for_slave, [Pid, _, _, Node, _, _, _]}}, _From, State) ->
    SlaveStarters1 = maps:put(SlavePid, {Node, Pid}, State#state.slave_starters),
    {reply, ok, State#state{slave_starters = SlaveStarters1}};
%% Spawn failed
handle_call({trace, Pid, spawn, ChildPid, {erts_internal, crasher, [ChildNode, _, _, _, _, _]}}, _From, State) ->
    ChildIdx = cauder_process:from_pid(ChildPid),
    Action = #trace_spawn{node = ChildNode, pid = ChildIdx, success = false},
    State1 = add_to_trace(Pid, Action, State),
    {reply, ok, State1};
%% Spawn succeeded
handle_call({trace, Pid, spawn, ChildPid, {_, _, _}}, _From, State) ->
    ChildIdx = cauder_process:from_pid(ChildPid),
    Action = #trace_spawn{node = node(ChildPid), pid = ChildIdx, success = true},
    State1 = add_to_trace(Pid, Action, State),
    State2 = State1#state{processes = sets:add_element(ChildPid, State1#state.processes)},
    {reply, ok, State2};
%% ========== 'exit' trace messages ========== %%
%% Process exited
handle_call({trace, Pid, exit, _Reason}, _From, #state{main_pid = MainPid, processes = ProcSet0} = State) ->
    ProcSet1 = sets:del_element(Pid, ProcSet0),
    State1 = State#state{processes = ProcSet1},
    case sets:is_empty(ProcSet1) of
        true -> MainPid ! finished;
        false -> ok
    end,
    {reply, ok, State1};
%% ========== Ignored trace messages ========== %%
handle_call({trace, _, call, {cauder_tracer_erlang, apply, [_, _, _]}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, call, {cauder_tracer_erlang, nodes, []}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, return_from, {cauder_tracer_erlang, send_centralized, 2}, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, send_to_non_existing_process, {{stamp, _Stamp}, _Message}, _}, _From, State) ->
    % TODO Log lost messages
    {reply, ok, State};
handle_call({trace, _, 'receive', {{stamp, _}, _}}, _From, State) ->
    % TODO Log deliver events
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
    ProcSet = State#state.processes,
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

-spec do_trace(Module, Function, Arguments, Options) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: all_options(),
    TraceInfo :: cauder_tracer:trace_info().

do_trace(Module, Function, Args, Opts) ->
    Dir = maps:get(dir, Opts),
    Modules = maps:get(modules, Opts),
    Timeout = maps:get(timeout, Opts),
    StampMode = maps:get(stamp_mode, Opts),
    Output = maps:get(output, Opts),

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

    load(cauder_tracer_erlang),

    MainPid = self(),
    TracedPid = spawn(cauder_tracer_erlang, start, [MainPid, Module, Function, Args]),
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
    dbg:tpl(cauder_tracer_erlang, apply, 3, [{'_', [], [{return_trace}]}]),
    dbg:tpl(cauder_tracer_erlang, send_centralized, 2, [{'_', [], [{return_trace}]}]),
    dbg:tpl(cauder_tracer_erlang, nodes, 0, [{'_', [], [{return_trace}]}]),

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

    {ok, Traces} = gen_server:call(?SERVER, get_traces),
    ReturnValue = gen_server:call(?SERVER, get_return_value),
    gen_server:stop(?SERVER),

    Result = #trace_info{
        node = node(),
        pid = cauder_process:from_pid(TracedPid),
        call = {Module, Function, Args},
        tracing = Tracing,
        return = ReturnValue,
        comp = CompTime,
        exec = ExecTime,
        trace = Traces
    },

    case Output of
        [] -> ok;
        _ -> write_trace(Output, Result)
    end,

    Result.

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
        {parse_transform, cauder_tracer_transform},
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

-spec add_to_trace(Pid, Action, State) -> NewState when
    Pid :: pid(),
    Action :: cauder_trace:action(),
    State :: state(),
    NewState :: state().

add_to_trace(Pid, Action, #state{trace = Trace0} = State) ->
    Id = cauder_process:from_pid(Pid),
    Trace1 = cauder_trace:push(Id, Action, Trace0),
    State#state{trace = Trace1}.

-spec get_uid(Stamp, State) -> {Uid, NewState} when
    Stamp :: integer(),
    State :: state(),
    Uid :: cauder_message:uid(),
    NewState :: state().

get_uid(Stamp, #state{ets = Table, stamps = Map} = State) ->
    case maps:find(Stamp, Map) of
        error ->
            Uid = ets:update_counter(Table, uid, 1, {uid, -1}),
            NewMap = maps:put(Stamp, Uid, Map),
            {Uid, State#state{stamps = NewMap}};
        {ok, Uid} ->
            {Uid, State}
    end.

-spec write_trace(Dir, TraceInfo) -> ok when
    Dir :: file:filename(),
    TraceInfo :: cauder_tracer:trace_info().

write_trace(Dir, TraceResult) ->
    % This not compile time safe but there is no other way to keep it human-friendly and simple
    [trace_info | Values] = tuple_to_list(TraceResult),
    Fields = record_info(fields, trace_info),
    {Traces, ResultInfo} = maps:take(trace, maps:from_list(lists:zip(Fields, Values))),

    ok = filelib:ensure_dir(Dir),
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> ok = file:make_dir(Dir)
    end,

    ResultFile = filename:join(Dir, "trace_result.log"),
    ResultContent = lists:join($\n, lists:map(fun(T) -> io_lib:format("~p.", [T]) end, maps:to_list(ResultInfo))),
    ok = file:write_file(ResultFile, ResultContent),

    lists:foreach(
        fun({Index, List}) ->
            File = filename:join(Dir, io_lib:format("trace_~b.log", [Index])),
            Content = lists:join($\n, lists:map(fun(T) -> io_lib:format("~p.", [T]) end, List)),
            ok = file:write_file(File, Content)
        end,
        maps:to_list(Traces)
    ).
