%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-ignore_xref([rule_deliver/2]).

-include("cauder.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> NewSystem when
    System :: cauder_types:system(),
    Pid :: cauder_types:proc_id(),
    NewSystem :: cauder_types:system().

step(Sys0, Pid) ->
    #process{pid = Pid, hist = [Entry | _]} = P0 = maps:get(Pid, Sys0#system.pool),
    case Entry of
        {tau, _Bs, _Es, _Stk} ->
            rule_local(Sys0, P0);
        {self, _Bs, _Es, _Stk} ->
            rule_self(Sys0, P0);
        {node, _Bs, _Es, _Stk} ->
            rule_node(Sys0, P0);
        {nodes, _Bs, _Es, _Stk, _Nodes} ->
            rule_nodes(Sys0, P0);
        {spawn, _Bs, _Es, _Stk, _SpawnNode, _SpawnPid} ->
            rule_spawn(Sys0, P0);
        {start, _Result, _Bs, _Es, _Stk, _Node} ->
            rule_start(Sys0, P0);
        {send, _Bs, _Es, _Stk, #message{uid = Uid, dst = Dest}} ->
            Sys1 = rule_deliver(Sys0, maps:get(Dest, Sys0#system.pool), Uid),
            rule_send(Sys1, maps:get(Pid, Sys1#system.pool));
        {rec, _Bs, _Es, _Stk, _Msg, _QPos} ->
            rule_receive(Sys0, P0)
    end.

-spec rule_local(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_local(
    #system{pool = PMap} = Sys,
    #process{pid = Pid, hist = [{tau, Bs0, Es0, Stk0} | Hist0]} = P0
) ->
    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    Sys#system{
        pool = PMap#{Pid := P1}
    }.

-spec rule_self(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_self(
    #system{pool = PMap} = Sys,
    #process{pid = Pid, hist = [{self, Bs0, Es0, Stk0} | Hist0]} = P0
) ->
    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    Sys#system{
        pool = PMap#{Pid := P1}
    }.

-spec rule_node(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_node(
    #system{pool = PMap} = Sys,
    #process{pid = Pid, hist = [{node, Bs0, Es0, Stk0} | Hist0]} = P0
) ->
    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    Sys#system{
        pool = PMap#{Pid := P1}
    }.

-spec rule_nodes(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_nodes(
    #system{log = Log0, pool = PMap} = Sys,
    #process{pid = Pid, hist = [{nodes, Bs0, Es0, Stk0, Nodes0} | Hist0]} = P0
) ->
    Log1 = log_prepend(Pid, Log0, {nodes, Nodes0}),

    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    Sys#system{
        pool = PMap#{Pid := P1},
        log = Log1
    }.

-spec rule_spawn(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_spawn(
    #system{log = Log0, pool = PMap} = Sys,
    #process{pid = Pid, hist = [{spawn, Bs0, Es0, Stk0, SpawnNode, SpawnPid} | Hist0]} = P0
) ->
    Result =
        case cauder_utils:process_node(PMap, SpawnPid) of
            false -> failure;
            _ -> success
        end,
    Log1 = log_prepend(Pid, Log0, {spawn, {SpawnNode, SpawnPid}, Result}),

    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    %%    T = #x_trace{
    %%        type = ?RULE_SPAWN,
    %%        from = Pid,
    %%        to = SpawnPid
    %%    },
    Sys#system{
        pool = maps:remove(SpawnPid, PMap#{Pid := P1}),
        log = Log1
    }.

-spec rule_start(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_start(
    #system{nodes = Nodes0, log = Log0, pool = PMap} = Sys,
    #process{pid = Pid, hist = [{start, Result, Bs0, Es0, Stk0, Node0} | Hist0]} = P0
) ->
    Nodes1 =
        case Result of
            success -> lists:delete(Node0, Nodes0);
            failure -> Nodes0
        end,
    Log1 = log_prepend(Pid, Log0, {start, Node0, Result}),

    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    %%    T = #x_trace{
    %%        type = ?RULE_START,
    %%        from = Pid,
    %%        res = Result,
    %%        node = Node0
    %%    },
    Sys#system{
        nodes = Nodes1,
        pool = PMap#{Pid := P1},
        log = Log1
    }.

-spec rule_send(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_send(
    #system{mail = Mail0, log = Log0, pool = PMap} = Sys,
    #process{pid = Pid, hist = [{send, Bs0, Es0, Stk0, #message{dst = Dest, val = Val, uid = Uid} = Msg} | Hist0]} = P0
) ->
    Mail1 = cauder_mailbox:remove(Msg, Mail0),
    Log1 = log_prepend(Pid, Log0, {send, Uid}),

    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0
    },
    %%    T = #x_trace{
    %%        type = ?RULE_SEND,
    %%        from = Pid,
    %%        to = Dest,
    %%        val = Val,
    %%        time = Uid
    %%    },
    Sys#system{
        mail = Mail1,
        pool = PMap#{Pid := P1},
        log = Log1
    }.

-spec rule_deliver(System, Process, Uid) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    Uid :: cauder_mailbox:uid(),
    NewSystem :: cauder_types:system().

rule_deliver(
    #system{mail = Mail0, pool = PMap} = Sys,
    #process{pid = Pid, mail = LocalMail0} = P0,
    Uid
) ->
    {{Msg, _}, LocalMail1} = cauder_utils:queue_take(Uid, LocalMail0),
    Mail1 = cauder_mailbox:add(Msg, Mail0),

    P1 = P0#process{
        mail = LocalMail1
    },
    Sys#system{
        mail = Mail1,
        pool = PMap#{Pid := P1}
    }.

-spec rule_receive(System, Process) -> NewSystem when
    System :: cauder_types:system(),
    Process :: cauder_types:proc(),
    NewSystem :: cauder_types:system().

rule_receive(
    #system{log = Log0, pool = PMap} = Sys,
    #process{pid = Pid, hist = [{rec, Bs0, Es0, Stk0, Msg, _QPos} | Hist0], mail = LocalMail0} = P0
) ->
    % TODO Is QPos necessary??
    #message{uid = Uid, val = Value, dst = Pid} = Msg,
    Log1 = log_prepend(Pid, Log0, {'receive', Uid}),
    LocalMail1 = queue:in_r(Msg, LocalMail0),

    P1 = P0#process{
        hist = Hist0,
        stack = Stk0,
        env = Bs0,
        expr = Es0,
        mail = LocalMail1
    },
    %%    T = #x_trace{
    %%        type = ?RULE_RECEIVE,
    %%        from = Pid,
    %%        val = Value,
    %%        time = Uid
    %%    },
    Sys#system{
        pool = PMap#{Pid := P1},
        log = Log1
    }.

-spec log_prepend(Pid, Log, Action) -> NewLog when
    Pid :: cauder_types:proc_id(),
    Log :: cauder_types:log(),
    Action :: cauder_types:log_action(),
    NewLog :: cauder_types:log().

log_prepend(Pid, Log, Action) ->
    maps:update_with(Pid, fun(Actions) -> [Action | Actions] end, [Action], Log).

%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
    System :: cauder_types:system(),
    Options :: [cauder_types:option()].

options(#system{pool = PMap} = Sys) ->
    maps:fold(
        fun(Pid, Proc, Opts) ->
            case process_option(Sys#system{pool = maps:without([Pid], PMap)}, Proc) of
                ?NULL_OPT -> Opts;
                Opt -> [Opt | Opts]
            end
        end,
        [],
        PMap
    ).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(System, Process) -> Option when
    System :: cauder_types:system(),
    Process :: cauder_types:process(),
    Option :: cauder_types:option() | ?NULL_OPT.

process_option(_, #process{hist = []}) ->
    ?NULL_OPT;
process_option(_, #process{pid = Pid, hist = [{tau, _Bs, _Es, _Stk} | _]}) ->
    #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEQ};
process_option(_, #process{pid = Pid, hist = [{self, _Bs, _Es, _Stk} | _]}) ->
    #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SELF};
process_option(_, #process{pid = Pid, hist = [{node, _Bs, _Es, _Stk} | _]}) ->
    #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODE};
process_option(#system{nodes = SysNodes}, #process{node = Node, pid = Pid, hist = [{nodes, _Bs, _Es, _Stk, Nodes} | _]}) ->
    ProcViewOfNodes = SysNodes -- [Node],
    case ProcViewOfNodes =:= Nodes of
        true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODES};
        false -> ?NULL_OPT
    end;
process_option(#system{pool = PMap}, #process{pid = Pid, hist = [{spawn, _Bs, _Es, _Stk, _Node, SpawnPid} | _]}) ->
    try maps:get(SpawnPid, PMap) of
        Proc ->
            #process{hist = Hist} = Proc,
            case Hist of
                [] -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
                _ -> ?NULL_OPT
            end
    catch
        % this case covers the scenario of a failed spawn
        error:{badkey, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN}
    end;
process_option(#system{pool = PMap}, #process{pid = Pid, hist = [{start, success, _Bs, _Es, _Stk, Node} | _]}) ->
    ProcWithFailedStart = cauder_utils:find_process_with_failed_start(PMap, Node),
    ProcOnNode = cauder_utils:find_process_on_node(PMap, Node),
    ProcWithRead = cauder_utils:find_process_with_read(PMap, Node),
    case {ProcWithFailedStart, ProcOnNode, ProcWithRead} of
        {false, false, false} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
        _ -> ?NULL_OPT
    end;
process_option(_, #process{pid = Pid, hist = [{start, failure, _Bs, _Es, _Stk, _Node} | _]}) ->
    #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
process_option(#system{pool = PMap}, #process{
    pid = Pid,
    hist = [{send, _Bs, _Es, _Stk, #message{dst = Dest} = Msg} | _]
}) ->
    #process{mail = LocalMail} = maps:get(Dest, PMap),
    case queue:member(Msg, LocalMail) of
        true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
        false -> ?NULL_OPT
    end;
process_option(_, #process{pid = Pid, hist = [{rec, _Bs, _Es, _Stk, _Msg, _QPos} | _]}) ->
    #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}.
