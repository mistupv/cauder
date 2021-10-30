-module(prefix_tracer).

-behaviour(gen_server).

%% API
-export([instrument/1, trace/4, trace/5, write_trace/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ignore_xref([instrument/1, trace/4, trace/5, write_trace/2]).

-include("cauder.hrl").

-define(SERVER, sched).

-record(state, {
    % The pid of the tracer process, used to notify when the execution of the traced process has finished
    main_pid :: pid(),
    initial_pid :: cauder_types:proc_id(),
    pid_map :: #{pid() => cauder_types:proc_id()},
    alive_pids :: sets:set(pid()),
    log = maps:new() :: cauder_types:log(),
    % The actions of this trace are stored in reverse order
    trace = maps:new() :: cauder_types:trace(),
    mail :: cauder_mailbox:mailbox(pid()),
    % The value returned by the initial function application
    return = none :: none | {value, term()}
}).

-type state() :: #state{}.

-type options() :: [
    {timeout, timeout()}
].

-define(DEFAULT_OPTIONS, [{timeout, 5000}]).

%%%===================================================================
%%% API
%%%===================================================================

%%------------------------------------------------------------------------------
%% @doc Instrument the code in the file `File', which is an Erlang source code
%% file.

-spec instrument(File) -> {ok, ModuleName} when
    File :: file:filename(),
    ModuleName :: module().

instrument(File) ->
    {ok, _ModuleName} = compile:file(File, [
        verbose,
        report_errors,
        report_warnings,
        {parse_transform, prefix_tracer_transform}
    ]).

%%------------------------------------------------------------------------------
%% @doc Equivalent to `trace(Mod, Fun, Args, Log, [{timeout, 5000}])'.
%%
%% @see trace/5

-spec trace(Module, Function, Arguments, Log) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Log :: cauder_types:log(),
    TraceInfo :: cauder_types:trace_info().

trace(Mod, Fun, Args, Log) -> trace(Mod, Fun, Args, Log, ?DEFAULT_OPTIONS).

%%------------------------------------------------------------------------------
%% @doc Traces the evaluation of the expression `apply(Mod, Fun, Args)'.
%%
%% Note: The module to be evaluated must have been previously instrumented,
%% otherwise this function will fail.

-spec trace(Module, Function, Arguments, Log, Options) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Log :: cauder_types:log(),
    Options :: options(),
    TraceInfo :: cauder_types:trace_info().

trace(Mod, Fun, Args, Log, Opts) -> do_trace(Mod, Fun, Args, Log, Opts).

%%------------------------------------------------------------------------------
%% @doc Saves the given trace info to disk in the specified folder.

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

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%------------------------------------------------------------------------------
%% @private

-spec init({MainPid, TracedPid, Log}) -> {ok, State} when
    MainPid :: pid(),
    TracedPid :: pid(),
    Log :: cauder_types:log(),
    State :: state().

init({TracerPid, TracedPid, Log}) ->
    ?SERVER = ets:new(?SERVER, [set, private, named_table]),

    InitialPid =
        case maps:keys(Log) of
            [Pid | _] -> Pid;
            [] -> new_pid()
        end,

    true = ets:insert_new(?SERVER, {taken_pids, sets:from_list(maps:keys(Log))}),

    {ok, #state{
        main_pid = TracerPid,
        initial_pid = InitialPid,
        pid_map = #{TracedPid => InitialPid},
        alive_pids = sets:from_list([TracedPid]),
        log = Log,
        mail = cauder_mailbox:new()
    }}.

%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} when
    Request :: term(),
    From :: {pid(), term()},
    State :: state(),
    Reply :: term(),
    NewState :: state().

handle_call(get_trace, _From, #state{trace = Trace} = State) ->
    ReversedTrace = maps:map(fun(_, V) -> lists:reverse(map_pids(V)) end, Trace),
    {reply, {ok, ReversedTrace}, State};
handle_call(get_return_value, _From, #state{return = ReturnValue} = State) ->
    {reply, map_pids(ReturnValue), State};
handle_call(get_initial_pid, _From, #state{initial_pid = Pid} = State) ->
    {reply, Pid, State};
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

handle_info({P, spawn, P1}, #state{pid_map = PidMap0, alive_pids = AlivePids0, log = Log0, trace = Trace0} = State0) ->
    R = maps:get(P, PidMap0),
    State1 =
        case maps:get(R, Log0, []) of
            [] ->
                R1 = new_pid(),
                PidMap1 = maps:put(P1, R1, PidMap0),
                AlivePids1 = sets:add_element(P1, AlivePids0),
                Trace1 = update_trace(R, {spawn, {'nonode@nohost', R1}, success}, Trace0),
                State0#state{pid_map = PidMap1, alive_pids = AlivePids1, trace = Trace1};
            [{spawn, {'nonode@nohost', R1}, success} | Actions] ->
                PidMap1 = maps:put(P1, R1, PidMap0),
                AlivePids1 = sets:add_element(P1, AlivePids0),
                NewLog = Log0#{R := Actions},
                Trace1 = update_trace(R, {spawn, {'nonode@nohost', R1}, success}, Trace0),
                State0#state{pid_map = PidMap1, alive_pids = AlivePids1, log = NewLog, trace = Trace1}
        end,
    prefix_tracer_erlang:send_ack(P),
    State2 = try_deliver(P, State1),
    {noreply, State2};
handle_info({P, send, P1, V}, #state{pid_map = PidMap0, log = Log0, trace = Trace0} = State0) ->
    R = maps:get(P, PidMap0),
    R1 = maps:get(P1, PidMap0),
    State1 =
        case maps:get(R, Log0, []) of
            [] ->
                L = new_uid(),
                Trace1 = update_trace(R, {send, L, R1}, Trace0),
                process_new_msg({P, P1, L, V}, State0#state{trace = Trace1});
            [{send, L} | Actions] ->
                Log1 = Log0#{R := Actions},
                Trace1 = update_trace(R, {send, L, R1}, Trace0),
                process_msg({P, P1, L, V}, State0#state{log = Log1, trace = Trace1})
        end,
    State2 = try_deliver(P, State1),
    {noreply, State2};
handle_info({P, 'receive', L}, #state{pid_map = PidMap0, log = Log0, trace = Trace0} = State0) ->
    R = maps:get(P, PidMap0),
    State1 =
        case maps:get(R, Log0, []) of
            [] ->
                Trace1 = update_trace(R, {'receive', L}, Trace0),
                State0#state{trace = Trace1};
            [{'receive', L} | Actions] ->
                Log1 = Log0#{R := Actions},
                Trace1 = update_trace(R, {'receive', L}, Trace0),
                State0#state{log = Log1, trace = Trace1}
        end,
    State2 = try_deliver(P, State1),
    {noreply, State2};
handle_info({return, ReturnValue}, #state{return = none} = State) ->
    {noreply, State#state{return = {value, ReturnValue}}};
handle_info({P, exit}, #state{main_pid = TracerPid, alive_pids = AlivePids0} = State0) ->
    AlivePids1 = sets:del_element(P, AlivePids0),
    State1 = State0#state{alive_pids = AlivePids1},
    case sets:is_empty(AlivePids1) of
        true -> TracerPid ! finished;
        false -> ok
    end,
    {noreply, State1};
handle_info(Info, State) ->
    io:format("[~p:~p] Unhandled Info: ~p~n", [?MODULE, ?LINE, Info]),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private

-spec terminate(Reason, State) -> ok when
    Reason :: any(),
    State :: state().

terminate(_Reason, State) ->
    ets:delete(?SERVER),
    lists:foreach(fun(P) -> true = erlang:exit(P, kill) end, maps:keys(State#state.pid_map)),
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

-spec do_trace(Module, Function, Arguments, Log, Options) -> TraceInfo when
    Module :: module(),
    Function :: atom(),
    Arguments :: [term()],
    Log :: cauder_types:log(),
    Options :: options(),
    TraceInfo :: cauder_types:trace_info().

do_trace(Module, Function, Args, Log, Opts) ->
    {timeout, Timeout} = proplists:lookup(timeout, Opts),

    load(prefix_tracer_erlang),

    TracerPid = self(),
    TracedPid = spawn(prefix_tracer_erlang, start, [Module, Function, Args]),
    {ok, _} = gen_server:start_link({local, ?SERVER}, ?MODULE, {TracerPid, TracedPid, Log}, []),

    TracedPid ! start,
    Tracing =
        receive
            finished -> success
        after Timeout -> timeout
        end,

    {ok, Trace} = gen_server:call(?SERVER, get_trace),
    ReturnValue = gen_server:call(?SERVER, get_return_value),
    InitialPid = gen_server:call(?SERVER, get_initial_pid),
    gen_server:stop(?SERVER),

    #trace_info{
        node = node(),
        pid = InitialPid,
        call = {Module, Function, Args},
        tracing = Tracing,
        return = ReturnValue,
        trace = Trace
    }.

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
    code:purge(Module),
    {module, Module} = code:load_binary(Module, Filename, Binary),
    ok.

-spec new_pid() -> cauder_types:proc_id().

new_pid() ->
    TakenPids =
        case ets:member(?SERVER, taken_pids) of
            true -> ets:lookup_element(?SERVER, taken_pids, 2);
            false -> sets:new()
        end,
    NextPid = fun F() ->
        Pid = ets:update_counter(?SERVER, last_pid, 1, {last_pid, -1}),
        case sets:is_element(Pid, TakenPids) of
            true -> F();
            false -> Pid
        end
    end,
    NextPid().

-spec new_uid() -> cauder_mailbox:uid().

new_uid() ->
    ets:update_counter(?SERVER, last_uid, 1, {last_uid, -1}).

-spec update_trace(Pid, Action, Trace) -> NewTrace when
    Pid :: cauder_types:proc_id(),
    Action :: cauder_types:trace_action(),
    Trace :: cauder_types:trace(),
    NewTrace :: cauder_types:trace().

update_trace(Pid, Action, Trace) ->
    maps:update_with(Pid, fun(Actions) -> [Action | Actions] end, [Action], Trace).

-spec try_deliver(Pid, State) -> NewState when
    Pid :: pid(),
    State :: state(),
    NewState :: state().

try_deliver(P, #state{pid_map = PidMap0, log = Log0, trace = Trace0, mail = Mail0} = State0) ->
    R = maps:get(P, PidMap0),
    case maps:get(R, Log0, []) of
        [{'receive', L} | _] ->
            case cauder_mailbox:uid_take_oldest(L, Mail0) of
                {#message{uid = L, dest = P, value = Value}, Mail1} ->
                    P ! {L, Value},
                    Trace1 = update_trace(R, {deliver, L}, Trace0),
                    State0#state{trace = Trace1, mail = Mail1};
                false ->
                    State0
            end;
        [{deliver, L} | Actions] ->
            State1 =
                case cauder_mailbox:uid_take_oldest(L, Mail0) of
                    {#message{uid = L, dest = P, value = Value}, Mail1} ->
                        P ! {L, Value},
                        Log1 = Log0#{R := Actions},
                        Trace1 = update_trace(R, {deliver, L}, Trace0),
                        State0#state{log = Log1, trace = Trace1, mail = Mail1};
                    false ->
                        State0
                end,
            try_deliver(P, State1);
        _ ->
            State0
    end.

-spec process_new_msg({SenderPid, DestinationPid, Uid, Value}, State) -> NewState when
    SenderPid :: pid(),
    DestinationPid :: pid(),
    Uid :: cauder_mailbox:uid(),
    Value :: term(),
    State :: state(),
    NewState :: state().

process_new_msg({P, P1, L, Value}, #state{pid_map = PidMap0, log = Log0, trace = Trace0, mail = Mail0} = State0) ->
    R1 = maps:get(P1, PidMap0),
    case maps:get(R1, Log0, []) of
        [] ->
            P1 ! {L, Value},
            Trace1 = update_trace(R1, {deliver, L}, Trace0),
            State0#state{trace = Trace1};
        Actions ->
            Mail1 = cauder_mailbox:add(#message{uid = L, value = Value, src = P, dest = P1}, Mail0),
            Log1 = Log0#{R1 => Actions ++ [{deliver, L}]},
            State0#state{log = Log1, mail = Mail1}
    end.

-spec process_msg({SenderPid, DestinationPid, Uid, Value}, State) -> NewState when
    SenderPid :: pid(),
    DestinationPid :: pid(),
    Uid :: cauder_mailbox:uid(),
    Value :: term(),
    State :: state(),
    NewState :: state().

process_msg({P, P1, L, Value}, #state{pid_map = PidMap0, log = Log0, trace = Trace0, mail = Mail0} = State0) ->
    R1 = maps:get(P1, PidMap0),
    case maps:get(R1, Log0, []) of
        [{'receive', L} | _] ->
            P1 ! {L, Value},
            Trace1 = update_trace(R1, {deliver, L}, Trace0),
            State0#state{trace = Trace1};
        Actions ->
            Mail1 = cauder_mailbox:add(#message{uid = L, value = Value, src = P, dest = P1}, Mail0),
            Log1 =
                case lists:member({'receive', L}, Actions) of
                    true -> Log0;
                    false -> Log0#{R1 => Actions ++ [{deliver, L}]}
                end,
            State0#state{log = Log1, mail = Mail1}
    end.

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

-spec pid_index(Pid) -> Index when
    Pid :: pid(),
    Index :: non_neg_integer().

pid_index(Pid) when is_pid(Pid) ->
    [_Node, Index, _Serial] = string:lexemes(pid_to_list(Pid), "<.>"),
    list_to_integer(Index).
