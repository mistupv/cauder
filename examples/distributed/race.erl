-module(race).
-export([start/0, racer/0]).

start() ->
    slave:start(mbp, race),
    R1 = spawn(?MODULE, racer, []),
    R2 = spawn(?MODULE, racer, []),
    spawn('another@mbp', erlang, node, []),
    R1 ! go,
    R2 ! go.

racer() ->
    Result =
        receive
            %slave:start(domain, name)
            go -> slave:start(mbp, another)
        end,
    case Result of
        {ok, _} -> winner;
        {error, _} -> loser
    end.
