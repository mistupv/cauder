-module(prefix_tracer_erlang).

%% API
-export([start/3, spawn/3, send_ack/1]).

-ignore_xref([start/3, spawn/3]).

-spec start(Module, Function, Args) -> term() when
    Module :: module(),
    Function :: atom(),
    Args :: [term()].

start(Module, Function, Args) ->
    receive
        start -> ok
    end,
    sched ! {return, apply(Module, Function, Args)}.

-spec spawn(Module, Function, Args) -> pid() when
    Module :: module(),
    Function :: atom(),
    Args :: [term()].

spawn(Mod, Fun, Args) ->
    Pid = self(),
    SpawnPid = spawn(fun() ->
        sched ! {Pid, spawn, self()},
        apply(Mod, Fun, Args)
    end),
    receive
        {ack} -> ok
    end,
    SpawnPid.

-spec send_ack(Pid) -> any() when
    Pid :: pid().

send_ack(_Pid) ->
    exit(not_transformed).
