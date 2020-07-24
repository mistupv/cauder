-define(APP_STRING, "CauDEr").

-define(ID_GAMMA, 0).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p",[X]))).
-else.
-define(LOG(X), ok).
-define(TO_STRING(X), "").
-endif.

-define(FWD_SEM, fwd_sem).
-define(BWD_SEM, bwd_sem).

-define(RULE_SEQ,      seq).
-define(RULE_SELF,     self).
-define(RULE_SPAWN,    spawn).
-define(RULE_SEND,     send).
-define(RULE_RECEIVE, 'receive').

% ets defs
-define(APP_REF, '_._app').
-define(GUI_REF, '_._gui').

-define(REPLAY_DATA,  200).

-define(FRESH_PID,  fresh_pid).
-define(FRESH_TIME, fresh_time).
-define(FRESH_VAR,  fresh_var).

-define(MULT_FWD,   mult_fwd).
-define(MULT_BWD,   mult_bwd).

-define(NOT_EXP,    not_exp).
-define(NULL_RULE,  null_rule).
-define(NULL_OPT,   null_opt).

-define(PRINT_MAIL,   print_mail).
-define(PRINT_LOG,    print_log).
-define(PRINT_HIST,   print_hist).
-define(PRINT_STACK,  print_stack).
-define(PRINT_ENV,    print_env).
-define(PRINT_EXP,    print_exp).
-define(FULL_HIST,    print_full_hist).
-define(FULL_ENV,     print_full_env).
-define(COMP_OPT,     comp_opt).

-define(SCHED_RANDOM,      random).
-define(SCHED_PRIO_RANDOM, prio_random).

-define(CAUDER_GREEN, {34,139,34}).

-define(LAST_PATH,    last_path).
-define(MODULE_PATH,  module_path).


% System
-record(sys, {
  % Global mailbox
  mail = [] :: [cauder_types:message()],
  % Pool of processes
  procs = [] :: [cauder_types:process()],
  ghosts = [] :: [cauder_types:process()],
  trace = [] :: [cauder_types:trace()],
  roll = []
}).

%% Process
-record(proc, {
  % Process identifier
  pid :: pos_integer(),
  % Log
  log = [] :: cauder_types:log(),
  % History
  hist = [] :: cauder_types:history(),
  % Call stack
  stack = [] :: cauder_types:stack(),
  % Environment
  env = erl_eval:new_bindings() :: cauder_types:environment(),
  % List of expressions
  exprs :: undefined | [cauder_types:abstract_expr()],
  % The entry point function for this process
  spf :: undefined | {atom(), atom(), arity()}
}).

%% Message
-record(msg, {
  % Target process identifier
  dest :: pos_integer(),
  % Message
  val :: term(),
  % Timestamp
  time :: non_neg_integer()
}).

% Option
-record(opt, {
  % forward or backward
  sem :: ?FWD_SEM | ?BWD_SEM,
  % integer
  id :: undefined | non_neg_integer(),
  % seq, spawn, ...
  rule :: ?RULE_SEQ | ?RULE_SELF | ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE
}).

% Trace
-record(trace, {
  type :: ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE,
  from :: pos_integer(),
  to :: undefined | pos_integer(),
  val :: undefined | term(),
  time :: undefined | pos_integer()
}).

% Replay info
-record(replay, {
  log_path :: file:filename(),
  call :: {atom(), atom(), [cauder_types:abstract_expr()]},
  main_pid :: pos_integer()
}).

% Evaluation step result
-record(result, {
  env :: cauder_types:environment(),
  exprs :: [cauder_types:abstract_expr()],
  stack :: cauder_types:stack(),
  label = tau :: cauder_types:label()
}).
