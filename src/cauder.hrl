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
-define(SEM_FWD, cauder_semantics_forwards).
% Name of the module that defines the backwards semantics.
-define(SEM_BWD, cauder_semantics_backwards).

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
-record(system, {
    % Global mailbox
    mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
    % Pool of processes
    pool :: cauder_types:process_map(),
    % System log
    log = maps:new() :: cauder_types:log(),
    % Trace generated as the program is executed
    trace = maps:new() :: cauder_types:trace(),
    % Race sets for each process and message
    race_sets :: undefined | cauder_types:race_sets(),
    % System nodes
    nodes = [] :: [node()],
    % TODO Remove?
    roll = []
}).

%% Process
-record(process, {
    % Process node
    node :: node(),
    % Process identifier
    pid :: cauder_types:proc_id(),
    % History
    hist = [] :: cauder_types:history(),
    % Call stack
    stack = [] :: cauder_types:stack(),
    % Environment
    env = maps:new() :: cauder_types:environment(),
    % List of expressions
    expr :: [cauder_types:abstract_expr()],
    % Local message queue
    mail = queue:new() :: queue:queue(cauder_mailbox:message()),
    % The entry point function for this process
    mfa :: mfa()
}).

%% Message
-record(message, {
    % UID
    uid :: cauder_mailbox:uid(),
    % Message
    val :: term(),
    % Sender PID
    src :: cauder_types:proc_id(),
    % Receiver PID
    dst :: cauder_types:proc_id()
}).

% Option
-record(opt, {
    % integer
    pid :: cauder_types:proc_id(),
    % forward or backward
    sem :: cauder_types:semantics(),
    % seq, spawn, ...
    rule :: cauder_types:rule()
}).

% Evaluation result
-record(result, {
    env :: cauder_types:environment(),
    expr :: [cauder_types:abstract_expr()],
    stack :: cauder_types:stack(),
    label = tau :: cauder_types:label()
}).

% Trace result
-record(trace_info, {
    % Initial node
    node :: node(),
    % Initial pid
    pid :: cauder_types:proc_id(),
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
    trace = #{} :: cauder_types:trace()
}).
