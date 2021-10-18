-module(cauder_types).

-include("cauder.hrl").

-export_type([
    fwd_opts/0,
    option/0,
    semantics/0,
    process_scheduler/0,
    message_scheduler/0,
    rule/0,
    x_trace/0,
    result/0,
    label/0,
    trace_result/0
]).

% Abstract format types

%% Record types

-type option() :: #opt{}.
-type fwd_opts() :: #{atom() => term()}.
-type semantics() :: ?FWD_SEM | ?BWD_SEM.

-type process_scheduler() :: ?SCHEDULER_RoundRobin | ?SCHEDULER_FCFS.
-type message_scheduler() :: ?SCHEDULER_Random | ?SCHEDULER_Manual.

-type rule() ::
    ?RULE_SEQ | ?RULE_SELF | ?RULE_NODE | ?RULE_NODES | ?RULE_SPAWN | ?RULE_START | ?RULE_SEND | ?RULE_RECEIVE.

% TODO What is the purpose of this?
-type x_trace() :: #x_trace{}.

-type result() :: #result{}.
-type label() ::
    tau
    | {spawn, cauder_syntax:af_variable(), cauder_syntax:af_literal()}
    | {spawn, cauder_syntax:af_variable(), node(), cauder_syntax:af_literal()}
    | {spawn, cauder_syntax:af_variable(), module(), atom(), [term()]}
    | {spawn, cauder_syntax:af_variable(), node(), module(), atom(), [term()]}
    | {start, cauder_syntax:af_variable(), node()}
    | {start, cauder_syntax:af_variable(), atom(), atom()}
    | {self, cauder_syntax:af_variable()}
    | {node, cauder_syntax:af_variable()}
    | {nodes, cauder_syntax:af_variable()}
    | {send, cauder_process:proc_id(), term()}
    | {rec, cauder_syntax:af_variable(), cauder_syntax:af_clause_seq()}.

-type trace_result() :: #trace_result{}.
