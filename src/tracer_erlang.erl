-module(tracer_erlang).

%% API
-export([send_distributed/2, send_centralized/2, nodes/0]).

-ignore_xref([send_distributed/2, send_centralized/2, nodes/0]).

-type dst() ::
    pid()
    | port()
    | (RegName :: atom())
    | {RegName :: atom(), Node :: node()}.

-spec send_distributed(Dst, Msg) -> Msg when
    Dst :: dst(),
    Msg :: term().

send_distributed(Dst, Msg) ->
    Dst ! {{stamp, erlang:unique_integer()}, Msg},
    Msg.

-spec send_centralized(Dst, Msg) -> Msg when
    Dst :: dst(),
    Msg :: term().

send_centralized(Dst, Msg) ->
    receive
        {stamp, Stamp} ->
            Dst ! {{stamp, Stamp}, Msg},
            Msg
    end.

-spec nodes() -> Nodes when
    Nodes :: [node()].

nodes() -> erlang:nodes().
