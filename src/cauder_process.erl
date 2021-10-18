-module(cauder_process).

%% API
-export([]).
-export_type([
    process_map/0,
    proc_id/0,
    process/0,
    history/0,
    history_entry/0,
    stack/0,
    stack_entry/0,
    environment/0,
    binding/0
]).

-include("cauder.hrl").

-type process_map() :: #{proc_id() := process()}.
-type proc_id() :: pos_integer().
-type process() :: #proc{}.

-type history() :: [history_entry()].
-type history_entry() ::
    {tau, environment(), [cauder_syntax:abstract_expr()], stack()}
    | {self, environment(), [cauder_syntax:abstract_expr()], stack()}
    | {node, environment(), [cauder_syntax:abstract_expr()], stack()}
    | {nodes, environment(), [cauder_syntax:abstract_expr()], stack(), [node()]}
    | {spawn, environment(), [cauder_syntax:abstract_expr()], stack(), node(), proc_id()}
    | {start, success, environment(), [cauder_syntax:abstract_expr()], stack(), node()}
    | {start, failure, environment(), [cauder_syntax:abstract_expr()], stack(), node()}
    | {send, environment(), [cauder_syntax:abstract_expr()], stack(), cauder_mailbox:message()}
    | {rec, environment(), [cauder_syntax:abstract_expr()], stack(), cauder_mailbox:message(), QPos :: pos_integer()}.

-type stack() :: [stack_entry()].
-type stack_entry() ::
    {mfa(), environment(), [cauder_syntax:abstract_expr()], cauder_syntax:af_variable()}
    | {atom(), [cauder_syntax:abstract_expr()], cauder_syntax:af_variable()}.

-type environment() :: #{atom() => term()}.
-type binding() :: {atom(), term()}.
