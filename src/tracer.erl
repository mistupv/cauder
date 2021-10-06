-module(tracer).

-behaviour(gen_server).

%% API
-export([trace/3, trace/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([trace/3, trace/4]).

-include("cauder.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    % The pid of the tracer process, used to notify when the execution of the traced process has finished
    main_pid :: pid(),
    ets :: ets:tid(),
    % A map between 'unique integers' and stamps
    stamps = maps:new() :: #{integer() => non_neg_integer()},
    % The trace
    trace = maps:new() :: cauder_types:trace(),
    % A set with all the processes being traced
    processes :: sets:set(pid()),
    % The value returned by the initial function application
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
    TraceInfo :: cauder_types:trace_info().

trace(Mod, Fun, Args) -> trace(Mod, Fun, Args, #{}).

%%------------------------------------------------------------------------------
%% @doc Traces the evaluation of the expression `apply(Mod, Fun, Args)'.

-spec trace(Module, Function, Arguments, Options) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Options :: some_options() | some_options_proplist(),
    TraceInfo :: cauder_types:trace_info().

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

handle_call(get_traces, _From, #state{trace = Trace} = State) ->
    ReversedTrace = maps:map(fun(_, V) -> lists:reverse(map_pids(V)) end, Trace),
    {reply, {ok, ReversedTrace}, State};
handle_call(get_return_value, _From, #state{return = ReturnValue} = State) ->
    {reply, map_pids(ReturnValue), State};
%% ========== 'call' trace messages ========== %%
%% Generate message stamp
handle_call({trace, Pid, call, {tracer_erlang, send_centralized, [_, _]}}, _From, State) ->
    Pid ! {stamp, erlang:unique_integer()},
    {reply, ok, State};
%% ========== 'return_from' trace messages ========== %%
%% Save return value
handle_call({trace, _Pid, return_from, {tracer_erlang, apply, 3}, ReturnValue}, _From, #state{return = none} = State) ->
    {reply, ok, State#state{return = {value, ReturnValue}}};
%% Call to `nodes()`
handle_call({trace, Pid, return_from, {tracer_erlang, nodes, 0}, Nodes}, _From, State) ->
    State1 = add_to_trace(Pid, {nodes, Nodes}, State),
    {reply, ok, State1};
%% ========== 'send' trace messages ========== %%
%% Send message
handle_call({trace, Pid, send, {send, Stamp, Dst, Msg}, _}, _From, State) ->
    State1 = add_to_trace(Pid, {send, Stamp, Dst, Msg}, State),
    {reply, ok, State1};
%% Receive message
handle_call({trace, Pid, send, {'receive', Stamp, Constraints}, {undefined, _}}, _From, State) ->
    State1 = add_to_trace(Pid, {'receive', Stamp, Constraints}, State),
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
handle_call({trace, Pid, 'receive', {send, Stamp, _Dst, _Msg}}, _From, State) ->
    State1 = add_to_trace(Pid, {deliver, Stamp}, State),
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
handle_call({trace, _, call, {tracer_erlang, apply, [_, _, _]}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, call, {tracer_erlang, nodes, []}}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, return_from, {tracer_erlang, send_distributed, 2}, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, return_from, {tracer_erlang, send_centralized, 2}, _}, _From, State) ->
    {reply, ok, State};
handle_call({trace, _, send_to_non_existing_process, {send, _Stamp, _Dst, _Msg}, _}, _From, State) ->
    % TODO Trace lost messages
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
    TraceInfo :: cauder_types:trace_info().

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

    load(tracer_erlang),

    MainPid = self(),
    TracedPid = spawn(tracer_erlang, start, [MainPid, Module, Function, Args]),
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, {MainPid, TracedPid}, []),

    dbg:tracer(process, {fun trace_handler/2, []}),
    dbg:p(TracedPid, [m, c, p, sos]),
    dbg:tpe(send, [
        % {send, Stamp, Dst, Msg}
        {['_', {'send', '_', '_', '_'}], [], []},
        % {'receive', Stamp, ConstraintsFun}
        {[{'undefined', '_'}, {'receive', '_', '_'}], [], []},
        % {result, {ok, Node}}
        {['_', {'result', {'ok', '_'}}], [], []},
        % {result, {error, _}}
        {['_', {'result', {'error', '_'}}], [], []}
    ]),
    dbg:tpe('receive', [
        % {send, Stamp, Dst, Msg}
        {['_', '_', {send, '_', '_', '_'}], [], []}
    ]),
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

    {ok, Traces} = gen_server:call(?SERVER, get_traces),
    ReturnValue = gen_server:call(?SERVER, get_return_value),
    gen_server:stop(?SERVER),

    Result = #trace_info{
        node = node(),
        pid = pid_index(TracedPid),
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
    Entry :: cauder_types:trace_action(),
    State :: state(),
    NewState :: state().

add_to_trace(Pid, Entry0, #state{ets = Table, stamps = StampMap0, trace = Trace0} = State) ->
    Index = pid_index(Pid),
    {Entry1, StampMap1} =
        case Entry0 of
            {send, Stamp, Dst, Msg} ->
                {Uid, StampMap} = get_uid(Stamp, StampMap0, Table),
                {{send, Uid, Dst, Msg}, StampMap};
            {deliver, Stamp} ->
                {Uid, StampMap} = get_uid(Stamp, StampMap0, Table),
                {{deliver, Uid}, StampMap};
            {'receive', Stamp, Constraints} ->
                {Uid, StampMap} = get_uid(Stamp, StampMap0, Table),
                {{'receive', Uid, Constraints}, StampMap};
            _ ->
                {Entry0, StampMap0}
        end,
    Trace1 = maps:update_with(Index, fun(Other) -> [Entry1 | Other] end, [Entry1], Trace0),
    State#state{stamps = StampMap1, trace = Trace1}.

-spec pid_index(Pid) -> Index when
    Pid :: pid(),
    Index :: non_neg_integer().

pid_index(Pid) when is_pid(Pid) ->
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

-spec write_trace(Dir, TraceInfo) -> ok when
    Dir :: file:filename(),
    TraceInfo :: cauder_types:trace_info().

write_trace(Dir, TraceInfo) ->
    % This not compile time safe but there is no other way to keep it human-friendly and simple
    [trace_info | Values] = tuple_to_list(TraceInfo),
    Fields = record_info(fields, trace_info),
    {Trace, ResultInfo} = maps:take(trace, maps:from_list(lists:zip(Fields, Values))),

    ok = filelib:ensure_dir(Dir),
    case filelib:is_dir(Dir) of
        true -> ok;
        false -> ok = file:make_dir(Dir)
    end,

    ResultFilename = filename:join(Dir, "trace_result.log"),
    write_terms(ResultFilename, maps:to_list(ResultInfo)),

    lists:foreach(
        fun({Index, Terms}) ->
            Filename = filename:join(Dir, io_lib:format("trace_~b.log", [Index])),
            write_terms(Filename, Terms)
        end,
        maps:to_list(Trace)
    ).

-spec write_terms(Filename, Terms) -> ok when
    Filename :: file:name_all(),
    Terms :: [term()].

write_terms(Filename, Terms) ->
    Format = fun(Term) -> io_lib:format("~tp.~n", [Term]) end,
    Text = unicode:characters_to_binary(lists:map(Format, Terms)),
    file:write_file(Filename, Text).

-spec map_pids(term()) -> term().

map_pids(List) when is_list(List) ->
    lists:map(fun map_pids/1, List);
map_pids(Tuple) when is_tuple(Tuple) ->
    list_to_tuple(lists:map(fun map_pids/1, tuple_to_list(Tuple)));
map_pids(Map) when is_map(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            K1 = map_pids(K),
            V1 = map_pids(V),
            false = maps:is_key(K1, Acc),
            Acc#{K1 => V1}
        end,
        #{},
        Map
    );
map_pids(Pid) when is_pid(Pid) ->
    pid_index(Pid);
map_pids(Term) ->
    Term.
