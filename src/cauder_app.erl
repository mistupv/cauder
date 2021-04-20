-module(cauder_app).

-behaviour(application).

%% application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason} when
    StartType :: application:start_type(),
    StartArgs :: term(),
    Pid :: pid(),
    State :: term(),
    Reason :: term().

start(normal, []) ->
    start(normal, [wx]);
start(normal, Args) ->
    cauder_sup:start_link(Args).

-spec stop(State) -> ok when
    State :: term().

stop(_State) ->
    ok.
