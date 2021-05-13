-module(tracer_erlang).

%% API
-export([send/2, nodes/0]).

-type dst() ::
    pid()
    | port()
    | (RegName :: atom())
    | {RegName :: atom(), Node :: node()}.

-spec send(Dst, Msg) -> Msg when
    Dst :: dst(),
    Msg :: term().

send(Dst, Msg) ->
    receive
        {stamp, Stamp} ->
            Dst ! {{stamp, Stamp}, Msg},
            Msg
    end.

-spec nodes() -> Nodes when
    Nodes :: [node()].

nodes() -> erlang:nodes().
