-module(tracer).

-export([trace/2, trace/3]).

trace(InitialCall, PidAnswer) ->
    trace(InitialCall, PidAnswer, []).

trace(InitialCall, PidAnswer, Opts) ->
    NOpts0 =
        case proplists:is_defined(timeout, Opts) of
            true -> Opts;
            false -> [{timeout, 10000} | Opts]
        end,
    NOpts1 =
        case proplists:is_defined(dir, NOpts0) of
            true -> NOpts0;
            false -> [{dir, "."} | NOpts0]
        end,
    NOpts2 =
        case proplists:is_defined(mods, NOpts1) of
            true -> NOpts1;
            false -> [{mods, []} | NOpts1]
        end,
    NOpts3 =
        case proplists:is_defined(log_dir, NOpts2) of
            true -> NOpts2;
            false -> [{log_dir, "trace"} | NOpts2]
        end,
    NOpts4 =
        case proplists:is_defined(stamp_mode, NOpts3) of
            true -> NOpts3;
            false -> [{stamp_mode, "central"} | NOpts3]
        end,
    trace_1(InitialCall, PidAnswer, NOpts4).

trace_1(InitialCall, PidAnswer, Opts) ->
    ModName = get_mod_name(InitialCall),
    {ok, TracingNode} =
        slave:start_link(
          list_to_atom(net_adm:localhost()),
          tracing,
          "-setcookie cookie"),
    Timeout = proplists:get_value(timeout, Opts),
    Dir     = proplists:get_value(dir,     Opts),
    Mods    = proplists:get_value(mods,    Opts),
    LogDir  = proplists:get_value(log_dir, Opts),
    StampMode = proplists:get_value(stamp_mode, Opts),
    put(modules_to_instrument, Mods),
    LogHandler = log:init_log_dir(LogDir),
    put(log_handler, LogHandler),
    log:append_data(io_lib:fwrite("call ~p~n", [InitialCall])),
                                                % io:format("~p\n", [SO]),
                                                % io:format("~p\n~p\n", [ModName, Dir]),
                                                % OriginalLibCode =
                                                %     [code:get_object_code(Mod) || Mod <- [gen_server, supervisor, gen_fsm, proc_lib, gen]],
    instrument_and_reload(ModName, Dir, TracingNode, StampMode),
    PidMain = self(),
    PidCall = execute_call(InitialCall, self(), Dir, TracingNode),
    SPidCall = log:slpid(PidCall),
    log:append_data(io_lib:fwrite("main_pid ~p~n", [SPidCall])),
    case proplists:get_value(distributed, Opts, false) of
        true -> log:append_data(io_lib:fwrite("main_node ~p~n", [TracingNode]));
        false -> log:append_data(io_lib:fwrite("main_node nonode@nohost~n", []))
    end,
    RunningProcs = [{PidCall, log:init_log_file(LogDir, PidCall)}],
    InstMod = get(modules_to_instrument),
    PidManager = spawn(fun() -> trace_manager(PidMain, RunningProcs,[]) end),
    PidTrace =
        spawn(
          fun() ->
                  put(modules_to_instrument, InstMod),
                  put(log_handler, LogHandler),
                  receive
                      setup_complete ->
                          setup_debug_server(TracingNode, PidCall, {#{}, [], PidManager, LogDir, RunningProcs, #{}}),
                          PidCall!start
                  end
          end),
    register(tracer, PidTrace),

    InitTime = erlang:monotonic_time(),
    receive
        all_done ->
            log:append_data(io_lib:fwrite("tracing success~n", [])),
            receive
                {result,Result} ->
                    log:append_data(io_lib:fwrite("result ~p~n", [Result]))
            end
    after
        Timeout ->
            PidManager ! {idle, self()},
            receive {PidManager, done} -> ok end,
            log:append_data(io_lib:fwrite("tracing timeout~n", [])),
            receive
                {result,Result} ->
                    log:append_data(io_lib:fwrite("result ~p~n", [Result]))
            after
                0 ->
                    log:append_data(io_lib:fwrite("result none~n", []))
            end
    end,
    EndTime =  erlang:monotonic_time(),
    DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
    log:append_data(io_lib:fwrite("exec ~p~n", [DiffTime])),
    dbg:stop(),
    slave:stop(TracingNode),
    Trace =
        receive
            {trace,Trace0} ->
                lists:reverse(Trace0)
        end,
    %% Loaded = % Commented to avoid warning
    %%receive
    %%    {loaded,Loaded0} ->
    %%        Loaded0
    %%end,
    PidAnswer!{Trace}.

human_readable_stamp(Map, Stamp) ->
    case maps:get(Stamp, Map, not_found) of
        not_found -> HRStamp = get_stamp(),
                     { maps:put(Stamp, HRStamp, Map), HRStamp};
        HRStamp -> {Map, HRStamp}
    end.

                                                % @doc
                                                % keeps a copy of trace and running process, it's useful when we reach a timeout and we need to
                                                % dump the trace of eventual process(es) still running
                                                % doc@
trace_manager(PidMain, RunningProcs, Trace) ->

    case RunningProcs of
        [] ->
            PidMain ! all_done,
            PidMain ! {trace, Trace};
                                                %PidMain ! {loaded, Loaded};
        _ -> receive
                 {update, NRunningProcs, NTrace} ->
                     trace_manager(PidMain, NRunningProcs, NTrace);
                 {idle, PidMain}  ->
                     IdlePids = [Pid || {Pid, _} <- RunningProcs],
                     [
                      begin
                          LogHandler = proplists:get_value(IdlePid, RunningProcs),
                          log:append_pid_data(LogHandler, Trace, IdlePid),
                          log:stop_log_file(LogHandler)
                      end || IdlePid <- IdlePids],
                     PidMain ! {self(), done},
                     PidMain ! {trace, Trace};
                 Other -> io:format("Unexpected message: ~p~n", [Other])
             end
    end.

trace_handler(TraceItem, {StampMap, Trace, PidManager, LogDir, RunningProcs, SlaveStarters}) ->
    {NStampMap, NTraceItem, NSlaveStarters} =
        case TraceItem of
            {trace, Pid, send, {receive_evaluated, Pid, {{stamp, Stamp}}}, _} ->
                SPid = log:slpid(Pid),
                {_NStampMap, HRStamp} = human_readable_stamp(StampMap, Stamp),
                {_NStampMap, {SPid, 'receive', HRStamp}, SlaveStarters};
            {trace, Pid, send, {send_sent, Pid, {}}, _} -> % act as a central authority for the stamp
                Pid ! {recv_stamp, erlang:unique_integer()},
                {StampMap, none, SlaveStarters};
            {trace, ParentPid, send, {log_nodes, Nodes}, _} -> % act as a central authority for the stamp
                SPid = log:slpid(ParentPid),
                {StampMap, {SPid, nodes, {Nodes}}, SlaveStarters};
            {trace, Pid, send, {{stamp, Stamp}, _Message}, _} ->
                SPid = log:slpid(Pid),
                {_NStampMap, HRStamp} = human_readable_stamp(StampMap, Stamp),
                {_NStampMap, {SPid, send, HRStamp}, SlaveStarters};
            {trace, ParentPid, spawn, SlavePid, {slave, wait_for_slave, Opts}} -> %Trying to start a node
                Node = lists:nth(4, Opts),
                {StampMap, none, maps:put(SlavePid, {ParentPid, Node},SlaveStarters)};
            {trace, ParentPid, 'receive', {result, {ok, NodeName}}} ->
                SPid = log:slpid(ParentPid),
                {StampMap, {SPid, 'start', {succ, NodeName}}, SlaveStarters};
            {trace, SStarterPid, send, {result, {error, _}}, _} ->
                case maps:get(SStarterPid, SlaveStarters, not_found) of
                    not_found             -> {StampMap, none ,SlaveStarters};
                    {ParentPid, NodeName} ->
                        SPid = log:slpid(ParentPid),
                        {StampMap, {SPid, 'start', {fail, NodeName}}, maps:remove(SStarterPid ,SlaveStarters)}
                end;
            {trace, ParentPid, spawn, SpawnPid, {erts_internal, crasher, [Node|_]}} ->
                SPid = log:slpid(ParentPid),
                SSPid = log:slpid(SpawnPid),
                {StampMap, {SPid, spawn, {Node, fail, SSPid}}, SlaveStarters};
            {trace, ParentPid, spawn, SpawnPid, _} ->
                SPid = log:slpid(ParentPid),
                SSPid = log:slpid(SpawnPid),
                {StampMap, {SPid, spawn, {node(SpawnPid), succ, SSPid}}, SlaveStarters};
            _ ->
                {StampMap, none, SlaveStarters}
        end,

                                                % we are interested to add just send, spawn and receive other messages will be excluded from the trace
    NTrace =
        case NTraceItem of
            none -> Trace;
            _ -> [NTraceItem | Trace]
        end,

    NRunningProcs =
        case {TraceItem, NTraceItem} of
            {{trace, PidDone, exit, _Value}, _} ->
                LogHandler = proplists:get_value(PidDone, RunningProcs, not_found),
                case LogHandler of
                    not_found ->
                        RunningProcs;
                    _ ->
                        log:append_pid_data(LogHandler, Trace, PidDone),
                        log:stop_log_file(LogHandler),
                        lists:delete({PidDone, LogHandler}, RunningProcs)
                end;
            {{trace, _, spawn, ChildPid, _},{_, spawn, {_, succ, _}}} ->
                LogItem = {ChildPid, log:init_log_file(LogDir, ChildPid)},
                [LogItem | RunningProcs];
            _ -> RunningProcs
        end,
                                                %io:format("Running processes: ~p~n", [NRunningProcs]),
    PidManager ! {update, NRunningProcs, NTrace},
    {NStampMap, NTrace, PidManager, LogDir, NRunningProcs, NSlaveStarters}.

setup_debug_server(TracingNode, ProcessPid, TracerState) ->

    dbg:tracer(process, {fun trace_handler/2, TracerState}), % starting the server for debugging
    dbg:n(TracingNode), % adding the node to the list of node controlled by the dbg
    dbg:p(ProcessPid, [m, sos, p]). % m means tracing messages, sos means set on spawn, p stands for process

send_module(TracingNode, Module, Dir) ->
    CompileOpts =
        [binary, {i,Dir}, {outdir,Dir}, return],
    File =
        get_file_path(Module, Dir),
    {ok, Module, Bin , _} =
        compile:file(File, CompileOpts),
    {_ResL, _BadNodes} =
        rpc:call(
          TracingNode, code, load_binary, [Module, File, Bin]),
    ok.

execute_call(Call, PidParent, _Dir, TracingNode) ->
    send_module(TracingNode, ?MODULE, filename:absname(filename:dirname(code:which(?MODULE)) ++ "/..") ++ "/src"),
    send_module(TracingNode, smerl, filename:absname(filename:dirname(code:which(?MODULE)) ++ "/..") ++ "/src"),
    MainNodeStr = atom_to_list(node()),
    FUN =
        fun() ->
                M1 = smerl:new(foo),
                {ok, M2} =
                    smerl:add_func(M1, "bar() ->
                                    {tracer, '" ++ MainNodeStr ++ "'} ! setup_complete,
                                   receive
                                       start -> ok
                                   end,
                                   try MainRes = " ++ Call ++
                           ",MainRes catch E1:E2 -> {E1,E2} end."),
        smerl:compile(M2,[nowarn_format]),
        Res = foo:bar(),
        PidParent!{result,Res}
        end,
    spawn(TracingNode, FUN).

get_mod_name(InitialCall) ->
    AExpr =
        case is_list(InitialCall) of
            true ->
                hd(parse_expr(InitialCall++"."));
            false ->
                InitialCall
        end,
    {call,_,{remote,_,{atom,_,ModName},_},_} = AExpr,
    ModName.

get_file_path(ModName, Dir) ->
    case Dir of
        none ->
            atom_to_list(ModName) ++ ".erl";
        _ ->
            Dir ++ "/" ++ atom_to_list(ModName) ++ ".erl"
    end.

instrument_and_reload(ModName, Dir, TracingNode, StampMode) ->
    CompileOpts =
        [{parse_transform, trace_pt}, {stamp_mode, StampMode}, binary, {i,Dir}, {outdir,Dir}, return, {inst_mod, get(modules_to_instrument)}],
    Msg =
        "Instrumenting...",
    instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode).

instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg, TracingNode) ->
    % [gen_server, gen_fsm, supervisor, proc_lib, gen]
    case lists:member(ModName, get(modules_to_instrument)) of
        true ->
            instrument_and_reload_sticky(ModName, Dir, CompileOpts, Msg, TracingNode);
        false ->
            FilePath = get_file_path(ModName, Dir),
            io:format("~s~p~n", [Msg, FilePath]),
            % io:format("~p\n", [CompileOpts]),
            InitTime = erlang:monotonic_time(),
            {ok,ModName,Binary,_} =
                case compile:file(FilePath, CompileOpts) of
                    {ok,_,_,_} = Res ->
                        Res
                    %     ;
                    % Other ->
                    %     io:format("~p\n", [Other])
                    % _ ->
                    %     io:format("~p\n", [element(1, filename:find_src(ModName))]),
                    %     Res = compile:file(element(1, filename:find_src(ModName)) ++ ".erl", CompileOpts),
                    %     io:format("~p\n", [Res]),
                    %     Res
                end,
            EndTime =  erlang:monotonic_time(),
            DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
            log:append_data(io_lib:fwrite("comp ~p ~p~n", [FilePath, DiffTime])),
            % io:format("~p\n", [get_file_path(ModName, Dir)]),
            % io:format("~p\n", [filename:find_src(ModName)]),
            % io:format("~p\n", [ file:get_cwd()]),
            %  =
            %     compile:file(get_file_path(ModName, Dir),),
            reload_module(ModName, Binary, TracingNode)
            % catch
            %     _:_ -> ok
            % end.
            ,ok
    end.

instrument_and_reload_sticky(ModName, _UserDir, CompileOpts, Msg, TracingNode) ->
    LibDir =
        code:lib_dir(stdlib, src),
    BeamDir =
        code:lib_dir(stdlib, ebin),
    FilePath =
        get_file_path(ModName, LibDir),
    io:format("~s~p\n", [Msg, FilePath]),
    InitTime = erlang:monotonic_time(),
    {ok, ModName, Binary,_} =
        case compile:file(FilePath, CompileOpts) of
            {ok,_,_,_} = Res ->
                Res;
            Other ->
                io:format("~p\n", [Other])
        end,
    EndTime =  erlang:monotonic_time(),
    DiffTime = erlang:convert_time_unit(EndTime - InitTime, native, microsecond),
    log:append_data(io_lib:fwrite("comp ~p ~p~n", [FilePath, DiffTime])),
    % ok =
    %     code:unstick_dir(BeamDir),
    %% TODO: Tracer gets stuck from here
    rpc:call(
        TracingNode, code, unstick_dir, [BeamDir]),
    reload_module(ModName, Binary, TracingNode),
    rpc:call(
        TracingNode, code, stick_dir, [BeamDir]).
% ok =
%     code:stick_dir(BeamDir).

% undo_instrument_and_reload(ModName, Dir) ->
%     CompileOpts =
%         [binary, {i,Dir}, {outdir,Dir}, return],
%     Msg =
%         "Restoring...",
%     instrument_and_reload_gen(ModName, Dir, CompileOpts, Msg).
%     % case lists:member(ModName, [gen_server, gen_fsm, supervisor]) of
%     %     true ->
%     % {ok,ModName,Binary} =
%     %     compile:file(get_file_path(ModName, Dir), [binary, {i,Dir}, {outdir,Dir}]),
%     % reload_module(ModName, Binary).

reload_module(ModName, Binary, TracingNode) ->
    try
        rpc:call(
            TracingNode, erlang, purge_module, [ModName])
    catch
        _:_ -> ok
    end,
    rpc:call(
        TracingNode, code, load_binary, [ModName, atom_to_list(ModName) ++ ".erl", Binary]).
% code:load_binary(ModName, atom_to_list(ModName) ++ ".erl", Binary).
% code:load_abs(atom_to_list(ModName)).

parse_expr(Func) ->
    case erl_scan:string(Func) of
        {ok, Toks, _} ->
            case erl_parse:parse_exprs(Toks) of
                {ok, _Term} ->
                    _Term;
                _Err ->
                    {error, parse_error}
            end;
        _Err ->
            {error, parse_error}
    end.

get_stamp() ->
    case get(stamp) of
        undefined -> put(stamp, 0),
            0;
        Stamp -> put(stamp, Stamp + 1) + 1
    end.
