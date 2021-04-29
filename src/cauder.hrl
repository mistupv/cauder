%%%=============================================================================
%%% App Information
%%%=============================================================================

-define(APP_NAME, "CauDEr").
-define(APP_URL, "https://github.com/mistupv/cauder").
-define(APP_DB, 'cauder/database').


-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p",[X]))).
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

-define(RULE_SEQ,      seq).
-define(RULE_SELF,     self).
-define(RULE_NODE,     node).
-define(RULE_NODES,    nodes).
-define(RULE_SPAWN,    spawn).
-define(RULE_START,    start).
-define(RULE_SEND,     send).
-define(RULE_RECEIVE, 'receive').

-define(NOT_EXP,    not_exp).
-define(NULL_OPT,   null_opt).


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

-define(CAUDER_GREEN, {34,139,34}).


%%%=============================================================================
%%% Records
%%%=============================================================================

% System
-record(sys, {
  % Global mailbox
  mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
  % Pool of processes
  procs :: cauder_types:process_map(),
  % System log
  logs = maps:new() :: cauder_types:log_map(),
  % System nodes
  nodes = [] :: [cauder_types:net_node()],
  trace = [] :: [cauder_types:trace()],
  roll = []
}).

%% Process
-record(proc, {
  % Process node
  node :: cauder_types:net_node(),
  % Process identifier
  pid :: cauder_types:proc_id(),
  % History
  hist = [] :: cauder_types:history(),
  % Call stack
  stack = [] :: cauder_types:stack(),
  % Environment
  env = maps:new() :: cauder_types:environment(),
  % List of expressions
  exprs :: [cauder_types:abstract_expr()],
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
  src :: cauder_types:proc_id(),
  % Receiver PID
  dest :: cauder_types:proc_id()
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

% Trace
-record(trace, {
  type :: ?RULE_SPAWN | ?RULE_START | ?RULE_SEND | ?RULE_RECEIVE,
  from :: cauder_types:proc_id(),
  to :: undefined | cauder_types:proc_id(),
  node :: undefined | cauder_types:net_node(),
  val :: undefined | term(),
  res :: succ | fail | undefined,
  time :: undefined | cauder_mailbox:uid()
}).

% Replay info
-record(replay, {
  log_path :: file:filename(),
  call :: {module(), atom(), cauder_types:af_args()},
  main_pid :: cauder_types:proc_id(),
  main_node :: cauder_types:net_node()
}).

% Evaluation step result
-record(result, {
  env :: cauder_types:environment(),
  exprs :: [cauder_types:abstract_expr()],
  stack :: cauder_types:stack(),
  label = tau :: cauder_types:label()
}).
