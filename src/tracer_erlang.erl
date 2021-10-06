-module(tracer_erlang).

%% API
-export([start/4, apply/3, send_distributed/2, send_centralized/2, nodes/0]).

-ignore_xref([send_distributed/2, send_centralized/2, nodes/0]).

-type dst() ::
    pid()
    | port()
    | (RegName :: atom())
    | {RegName :: atom(), Node :: node()}.

-spec start(Pid, Module, Function, Args) -> term() when
    Pid :: pid(),
    Module :: module(),
    Function :: atom(),
    Args :: [term()].

start(MainPid, Module, Function, Args) ->
    MainPid ! setup_complete,
    receive
        start -> ok
    end,
    tracer_erlang:apply(Module, Function, Args).

-spec apply(Module, Function, Args) -> term() when
    Module :: module(),
    Function :: atom(),
    Args :: [term()].

apply(Module, Function, Args) -> erlang:apply(Module, Function, Args).

-spec send_distributed(Dst, Msg) -> Msg when
    Dst :: dst(),
    Msg :: term().

send_distributed(Dst, Msg) ->
    Dst ! {send, erlang:unique_integer(), Dst, Msg},
    Msg.

-spec send_centralized(Dst, Msg) -> Msg when
    Dst :: dst(),
    Msg :: term().

send_centralized(Dst, Msg) ->
    receive
        {stamp, Stamp} ->
            Dst ! {send, Stamp, Dst, Msg},
            Msg
    end.

-spec nodes() -> Nodes when
    Nodes :: [node()].

nodes() -> erlang:nodes().
