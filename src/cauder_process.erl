-module(cauder_process).

%% API
-export([new_pid/0, add_binding/3]).
-export([from_pid/1]).

-include("cauder.hrl").
-include("cauder_process.hrl").

-export_type([id/0, process/0]).

-opaque id() :: pos_integer().
-type process() :: #process{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new and unique process identifier.

-spec new_pid() -> Id when
    Id :: cauder_process:id().

new_pid() -> ets:update_counter(?APP_DB, last_pid, 1, {last_pid, -1}).

-spec add_binding(Name, Value, Process) -> NewProcess when
    Name :: cauder_bindings:name(),
    Value :: cauder_bindings:value(),
    Process :: cauder_process:process(),
    NewProcess :: cauder_process:process().

add_binding(Name, Value, #process{env = Bs0} = P) ->
    P#process{env = cauder_bindings:add(Name, Value, Bs0)}.

%%%=============================================================================
%%% Utils
%%%=============================================================================

-spec from_pid(Pid) -> Id when
    Pid :: pid(),
    Id :: cauder_process:id().

from_pid(Pid) when is_pid(Pid) ->
    [_Node, Index, _Serial] = string:lexemes(pid_to_list(Pid), "<.>"),
    list_to_integer(Index).
