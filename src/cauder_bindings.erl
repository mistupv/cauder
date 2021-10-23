-module(cauder_bindings).

%% API
-export([new/0, add/3, get/2, is_bound/2, merge/2, to_list/1]).

-export_type([binding/0, bindings/0]).

-type name() :: term().
-type value() :: term().

-type binding() :: {cauder_bindings:name(), cauder_bindings:value()}.
-opaque bindings() :: #{cauder_bindings:name() => cauder_bindings:value()}.

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

-spec get(Name, Bindings) -> Value when
    Name :: cauder_bindings:name(),
    Bindings :: cauder_bindings:bindings(),
    Value :: cauder_bindings:value().

get(Name, Bs) ->
    case maps:find(Name, Bs) of
        {ok, Value} -> {value, Value};
        error -> unbound
    end.

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
    % TODO maps:merge_with/3
    maps:fold(
        fun(Name, Value, Bs) ->
            case maps:find(Name, Bs) of
                {ok, Value} -> Bs;
                {ok, V1} -> erlang:error({badmatch, V1});
                error -> maps:put(Name, Value, Bs)
            end
        end,
        Bs1,
        Bs2
    ).

-spec to_list(Bindings) -> [Binding] when
    Bindings :: cauder_bindings:bindings(),
    Binding :: cauder_bindings:binding().

to_list(Bindings) -> maps:to_list(Bindings).
