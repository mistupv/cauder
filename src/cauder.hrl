%%%=============================================================================
%%% App Information
%%%=============================================================================

-define(APP_NAME, "CauDEr").
-define(APP_URL, "https://github.com/mistupv/cauder").
-define(APP_DB, 'cauder/database').

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p", [X]))).
-else.
-define(LOG(X), ok).
-define(TO_STRING(X), "").
-endif.

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

% System
-record(sys, {
    % Global mailbox
    mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
    % Pool of processes
    procs :: cauder_process:process_map(),
    % System log
    traces = maps:new() :: cauder_trace:trace(),
    % System nodes
    nodes = [] :: [node()],
    x_trace = [] :: [cauder_types:x_trace()],
    roll = []
}).

%% Process
-record(proc, {
    % Process node
    node :: node(),
    % Process identifier
    pid :: cauder_process:proc_id(),
    % History
    hist = [] :: cauder_process:history(),
    % Call stack
    stack = [] :: cauder_process:stack(),
    % Environment
    env = maps:new() :: cauder_process:environment(),
    % List of expressions
    exprs :: [cauder_syntax:abstract_expr()],
    % The entry point function for this process
    spf :: mfa()
}).

%% Message
-record(message, {
    % UID
    uid = cauder_mailbox:uid() :: cauder_mailbox:uid(),
    % Message
    value :: term(),
    % Sender PID
    src :: cauder_process:proc_id(),
    % Receiver PID
    dest :: cauder_process:proc_id()
}).

% Option
-record(opt, {
    % integer
    pid :: cauder_process:proc_id(),
    % forward or backward
    sem :: cauder_types:semantics(),
    % seq, spawn, ...
    rule :: cauder_types:rule()
}).

% TODO What is the purpose of this?
-record(x_trace, {
    type :: ?RULE_SPAWN | ?RULE_START | ?RULE_SEND | ?RULE_RECEIVE,
    from :: cauder_process:proc_id(),
    to :: undefined | cauder_process:proc_id(),
    node :: undefined | node(),
    val :: undefined | term(),
    res :: success | failure | undefined,
    time :: undefined | cauder_mailbox:uid()
}).

% Evaluation result
-record(result, {
    env :: cauder_process:environment(),
    exprs :: [cauder_syntax:abstract_expr()],
    stack :: cauder_process:stack(),
    label = tau :: cauder_types:label()
}).

% Trace result
-record(trace_result, {
    % Initial node
    node :: node(),
    % Initial pid
    pid :: cauder_process:proc_id(),
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
