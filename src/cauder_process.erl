-module(cauder_process).

%% API
-export([]).
-export_type([
    process_map/0,
    proc_id/0,
    process/0,
    history/0,
    history_entry/0
]).

-include("cauder.hrl").

-type process_map() :: #{proc_id() := process()}.
-type proc_id() :: pos_integer().
-type process() :: #proc{}.

-type history() :: [history_entry()].
-type history_entry() ::
    {tau, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack()}
    | {self, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack()}
    | {node, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack()}
    | {nodes, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(), [node()]}
    | {spawn, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(), node(), proc_id()}
    | {start, success, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(), node()}
    | {start, failure, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(), node()}
    | {send, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(),
        cauder_mailbox:message()}
    | {rec, cauder_bindings:bindings(), [cauder_syntax:abstract_expr()], cauder_stack:stack(), cauder_mailbox:message(),
        QPos :: pos_integer()}.
