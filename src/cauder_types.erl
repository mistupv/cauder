-module(cauder_types).

-include("cauder.hrl").

-export_type([
    system/0,
    trace_map/0,
    trace/0,
    trace_entry/0,
    trace_entry_search/0,
    fwd_opts/0,
    process_map/0,
    proc_id/0,
    process/0,
    history/0,
    history_entry/0,
    stack/0,
    stack_entry/0,
    environment/0,
    binding/0,
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
-export_type([
    line/0,
    abstract_expr/0,
    af_args/0,
    af_literal/0,
    af_boolean/0,
    af_variable/0,
    af_remote_call/0,
    af_clause_seq/0,
    af_clause/0,
    af_pattern/0,
    af_guard_seq/0,
    af_guard/0,
    af_guard_test/0,
    af_body/0
]).

%% Record types

-type system() :: #sys{}.

-type trace_map() :: #{proc_id() => trace()}.
-type trace() :: [trace_entry()].
-type trace_entry() ::
    {send, cauder_mailbox:uid()}
    | {deliver, cauder_mailbox:uid()}
    | {'receive', cauder_mailbox:uid()}
    | {nodes, [node()]}
    | {start, node(), success}
    | {start, node(), failure}
    | {spawn, {node(), proc_id()}, success}
    | {spawn, {node(), proc_id()}, failure}.

-type trace_entry_search() ::
    {send, cauder_mailbox:uid()}
    | {'receive', cauder_mailbox:uid()}
    | {start, node(), success}
    | {spawn, {'_', proc_id()}, '_'}
    | {spawn, {node(), '_'}, failure}.

% Not empty
-type process_map() :: #{proc_id() := process()}.
-type proc_id() :: pos_integer().
-type process() :: #proc{}.

-type history() :: [history_entry()].
-type history_entry() ::
    {tau, environment(), [abstract_expr()], stack()}
    | {self, environment(), [abstract_expr()], stack()}
    | {node, environment(), [abstract_expr()], stack()}
    | {nodes, environment(), [abstract_expr()], stack(), [node()]}
    | {spawn, environment(), [abstract_expr()], stack(), node(), proc_id()}
    | {start, success, environment(), [abstract_expr()], stack(), node()}
    | {start, failure, environment(), [abstract_expr()], stack(), node()}
    | {send, environment(), [abstract_expr()], stack(), cauder_mailbox:message()}
    | {rec, environment(), [abstract_expr()], stack(), cauder_mailbox:message(), QPos :: pos_integer()}.

-type stack() :: [stack_entry()].
-type stack_entry() ::
    {mfa(), environment(), [abstract_expr()], af_variable()}
    | {atom(), [abstract_expr()], af_variable()}.

-type environment() :: #{atom() => term()}.
-type binding() :: {atom(), term()}.

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
    | {spawn, af_variable(), af_literal()}
    | {spawn, af_variable(), node(), af_literal()}
    | {spawn, af_variable(), module(), atom(), [term()]}
    | {spawn, af_variable(), node(), module(), atom(), [term()]}
    | {start, af_variable(), node()}
    | {start, af_variable(), atom(), atom()}
    | {self, af_variable()}
    | {node, af_variable()}
    | {nodes, af_variable()}
    | {send, proc_id(), term()}
    | {rec, af_variable(), af_clause_seq()}.

-type trace_result() :: #trace_result{}.

%% Custom of abstract format

-type line() :: non_neg_integer().

-type abstract_expr() ::
    af_literal()
    | af_variable()
    | af_variable()
    | af_cons(abstract_expr())
    | af_tuple(abstract_expr())
    | af_if()
    | af_case()
    | af_receive()
    | af_make_fun()
    | af_bif_call()
    | af_self_call()
    | af_node_call()
    | af_nodes_call()
    | af_start_1_call()
    | af_start_2_call()
    | af_spawn_1_call()
    | af_spawn_2_call()
    | af_spawn_3_call()
    | af_spawn_4_call()
    | af_send_call()
    | af_send_op_call()
    | af_local_call()
    | af_remote_call()
    | af_apply()
    | af_apply_fun()
    | af_match(abstract_expr())
    | af_op(abstract_expr())
    | af_short_circuit_op(abstract_expr()).

-type af_args() :: [abstract_expr()].

-type af_literal() :: {value, line(), term()}.

-type af_boolean() :: {value, line(), true | false}.

-type af_variable() :: {var, line(), atom()}.

-type af_cons(T) :: {cons, line(), T, T}.

-type af_tuple(T) :: {tuple, line(), [T]}.

-type af_if() :: {'if', line(), af_clause_seq()}.

-type af_case() :: {'case', line(), abstract_expr(), af_clause_seq()}.

-type af_receive() :: {'receive', line(), af_clause_seq()}.

-type af_make_fun() :: {make_fun, line(), atom(), af_clause_seq()}.

-type af_bif_call() :: {bif, line(), module(), atom(), af_args()}.

-type af_self_call() :: {self, line()}.

-type af_node_call() :: {node, line()}.

-type af_nodes_call() :: {nodes, line()}.

-type af_start_1_call() :: {start, line(), abstract_expr()}.

-type af_start_2_call() :: {start, line(), abstract_expr(), abstract_expr()}.

-type af_spawn_1_call() :: {spawn, line(), abstract_expr()}.

-type af_spawn_2_call() :: {spawn, line(), abstract_expr(), abstract_expr()}.

-type af_spawn_3_call() :: {spawn, line(), abstract_expr(), abstract_expr(), abstract_expr()}.

-type af_spawn_4_call() :: {spawn, line(), abstract_expr(), abstract_expr(), abstract_expr(), abstract_expr()}.

-type af_send_call() :: {send, line(), abstract_expr(), abstract_expr()}.

-type af_send_op_call() :: {send_op, line(), abstract_expr(), abstract_expr()}.

-type af_local_call() :: {local_call, line(), atom(), af_args()}.

-type af_remote_call() :: {remote_call, line(), module(), atom(), af_args()}.

-type af_apply() :: {apply, line(), abstract_expr(), abstract_expr(), af_args()}.

-type af_apply_fun() :: {apply_fun, line(), abstract_expr(), af_args()}.

-type af_match(T) :: {match, line(), af_pattern(), T}.

-type af_op(T) :: {op, line(), unary_op() | binary_op(), [T]}.

-type af_unary_arith_op(T) :: {op, line(), '+' | '-', [T]}.

-type af_short_circuit_op(T) :: {'andalso' | 'orelse', line(), T, T}.

%% Clauses

-type af_clause_seq() :: [af_clause(), ...].

-type af_clause() :: {'clause', line(), [af_pattern()], af_guard_seq(), af_body()}.

-type af_pattern() ::
    af_literal()
    | af_variable()
    | af_cons(af_pattern())
    | af_tuple(af_pattern())
    | af_match(af_pattern())
    | af_unary_arith_op(af_pattern()).

-type af_guard_seq() :: [af_guard()].

-type af_guard() :: [af_guard_test(), ...].

-type af_guard_test() ::
    af_literal()
    | af_variable()
    | af_cons(af_guard_test())
    | af_tuple(af_guard_test())
    | af_unary_arith_op(af_guard_test())
    | af_short_circuit_op(af_guard_test())
    | af_guard_call()
    | af_self_call()
    | af_node_call()
    | af_nodes_call().

-type af_guard_call() :: {'bif', line(), erlang, atom(), [af_guard_test()]}.

-type af_body() :: [abstract_expr(), ...].

%% Operators

-type binary_op() ::
    '/'
    | '*'
    | 'div'
    | 'rem'
    | 'band'
    | 'and'
    | '+'
    | '-'
    | 'bor'
    | 'bxor'
    | 'bsl'
    | 'bsr'
    | 'or'
    | 'xor'
    | '++'
    | '--'
    | '=='
    | '/='
    | '=<'
    | '<'
    | '>='
    | '>'
    | '=:='
    | '=/='.

-type unary_op() :: '+' | '-' | 'bnot' | 'not'.

%% End of custom abstract format
