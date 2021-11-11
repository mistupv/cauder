%%%=============================================================================
%%% App Information
%%%=============================================================================

-define(APP_NAME, "CauDEr").
-define(APP_URL, "https://github.com/mistupv/cauder").
-define(APP_DB, 'cauder/database').

%%%=============================================================================
%%% Semantics
%%%=============================================================================

% Name of the module that defines the forwards semantics.
-define(FWD_SEM, cauder_semantics_forwards).
% Name of the module that defines the backwards semantics.
-define(BWD_SEM, cauder_semantics_backwards).

%%%=============================================================================
%%% Rules
%%%=============================================================================

-define(RULE_SEQ, seq).
-define(RULE_SELF, self).
-define(RULE_NODE, node).
-define(RULE_NODES, nodes).
-define(RULE_SPAWN, spawn).
-define(RULE_START, start).
-define(RULE_SEND, send).
-define(RULE_RECEIVE, 'receive').

-define(NOT_EXP, not_exp).
-define(NULL_OPT, null_opt).

%%%=============================================================================
%%% Schedulers
%%%=============================================================================

-define(SCHEDULER_RoundRobin, round_robin).
-define(SCHEDULER_FCFS, fcfs).
-define(SCHEDULER_Random, random).
-define(SCHEDULER_Manual, manual).

%%%=============================================================================
%%% Other
%%%=============================================================================

-define(CAUDER_GREEN, {34, 139, 34}).

%%%=============================================================================
%%% Records
%%%=============================================================================

% Option
-record(opt, {
    % integer
    pid :: cauder_process:id(),
    % forward or backward
    sem :: cauder_types:semantics(),
    % seq, spawn, ...
    rule :: cauder_types:rule()
}).

% TODO What is the purpose of this?
-record(x_trace, {
    type :: ?RULE_SPAWN | ?RULE_START | ?RULE_SEND | ?RULE_RECEIVE,
    from :: cauder_process:id(),
    to :: undefined | cauder_process:id(),
    node :: undefined | node(),
    val :: undefined | term(),
    res :: success | failure | undefined,
    time :: undefined | cauder_message:uid()
}).

% Evaluation result
-record(result, {
    env :: cauder_bindings:bindings(),
    exprs :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    label = tau :: cauder_types:label()
}).

% Trace result
-record(trace_result, {
    % Initial node
    node :: node(),
    % Initial pid
    pid :: cauder_process:id(),
    % Initial function call
    call :: {module(), atom(), [term()]},
    % Whether the execution completed or the timeout was reached
    tracing :: success | timeout,
    % The value returned by the function application
    return = none :: none | {value, term()},
    % Compile time in microseconds
    comp :: non_neg_integer(),
    % Execution time in microseconds
    exec :: non_neg_integer(),
    traces = #{} :: cauder_trace:trace()
}).
