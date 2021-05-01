-module(cauder_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, init_providers(State)}.

init_providers(State) ->
    lists:foldl(fun init_provider/2, State, [
        cauder_plugin_prv_clean,
        cauder_plugin_prv_escript_link,
        cauder_plugin_prv_reltool
    ]).

init_provider(Module, State) ->
    {ok, NewState} = Module:init(State),
    NewState.
