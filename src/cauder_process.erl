-module(cauder_process).

%% API
-export([new_pid/0, add_binding/3, is_alive/1]).
-export([from_pid/1]).

-include("cauder.hrl").
-include("cauder_process.hrl").

-export_type([id/0, process/0, scheduler/0]).

-opaque id() :: pos_integer().
-type process() :: #process{}.

-type scheduler() :: ?SCHEDULER_RoundRobin | ?SCHEDULER_FCFS.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new and unique process identifier.

-spec new_pid() -> Id when
    Id :: cauder_process:id().

new_pid() ->
    ets:update_counter(?APP_DB, last_pid, 1, {last_pid, -1}).

-spec add_binding(Name, Value, Process) -> NewProcess when
    Name :: cauder_bindings:name(),
    Value :: cauder_bindings:value(),
    Process :: cauder_process:process(),
    NewProcess :: cauder_process:process().

add_binding(Name, Value, #process{env = Bs0} = P) ->
    P#process{env = cauder_bindings:add(Name, Value, Bs0)}.

-spec is_alive(Process) -> IsDead when
    Process :: cauder_process:process(),
    IsDead :: boolean().

is_alive(#process{expr = [{value, _, _}], stack = Stk}) -> not cauder_stack:is_empty(Stk);
is_alive(#process{}) -> true.

%%%=============================================================================
%%% Utils
%%%=============================================================================

-spec from_pid(Pid) -> Id when
    Pid :: pid(),
    Id :: cauder_process:id().

from_pid(Pid) when is_pid(Pid) ->
    [_Node, Index, _Serial] = string:lexemes(pid_to_list(Pid), "<.>"),
    list_to_integer(Index).
