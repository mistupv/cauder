-module(cauder_bindings).

%% API
-export([new/0, add/3, find/2, is_bound/2, merge/2, to_list/1]).

-export_type([binding/0, bindings/0]).

-type name() :: term().
-type value() :: term().

-type binding() :: {cauder_bindings:name(), cauder_bindings:value()}.
-opaque bindings() :: #{cauder_bindings:name() => cauder_bindings:value()}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec new() -> cauder_bindings:bindings().

new() -> maps:new().

-spec add(Name, Value, Bindings) -> NewBindings when
    Name :: cauder_bindings:name(),
    Value :: cauder_bindings:value(),
    Bindings :: cauder_bindings:bindings(),
    NewBindings :: cauder_bindings:bindings().

add(Name, Value, Bs) ->
    maps:put(Name, Value, Bs).

-spec find(Name, Bindings) -> {ok, Value} | error when
    Name :: cauder_bindings:name(),
    Bindings :: cauder_bindings:bindings(),
    Value :: cauder_bindings:value().

find(Name, Bs) ->
    maps:find(Name, Bs).

-spec is_bound(Name, Bindings) -> boolean() when
    Name :: cauder_bindings:name(),
    Bindings :: cauder_bindings:bindings().

is_bound(Name, Bs) ->
    maps:is_key(Name, Bs).

%%------------------------------------------------------------------------------
%% @doc Merges the two given bindings into a new one.
%% If the same variable is defined in both collections but with different
%% values, an exception is thrown.

-spec merge(Bindings1, Bindings2) -> Bindings3 when
    Bindings1 :: cauder_bindings:bindings(),
    Bindings2 :: cauder_bindings:bindings(),
    Bindings3 :: cauder_bindings:bindings().

merge(Bs1, Bs2) ->
    maps:merge_with(
        fun
            (_K, V, V) -> V;
            (_K, _, V) -> erlang:error({badmatch, V})
        end,
        Bs2,
        Bs1
    ).

-spec to_list(Bindings) -> [Binding] when
    Bindings :: cauder_bindings:bindings(),
    Binding :: cauder_bindings:binding().

to_list(Bindings) -> maps:to_list(Bindings).
