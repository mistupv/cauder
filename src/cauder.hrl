-define(APP_STRING, "CauDEr").

-define(ID_GAMMA, 0).

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE, ?LINE, X])).
-define(TO_STRING(X), lists:flatten(io_lib:format("~p",[X]))).
-else.
-define(LOG(X), true).
-define(TO_STRING(X), true).
-endif.

-define(FWD_SEM, fwd_sem).
-define(BWD_SEM, bwd_sem).

-define(TYPE_MSG,  msg).
-define(TYPE_PROC, proc).

-define(RULE_SEQ,      seq).
-define(RULE_CHECK,    check).
-define(RULE_SEND,     send).
-define(RULE_RECEIVE, 'receive').
-define(RULE_SPAWN,    spawn).
-define(RULE_SELF,     self).
-define(RULE_SCHED,    sched).

% ets defs
-define(APP_REF, '_._app').
-define(GUI_REF, '_._gui').

-define(FILE_PATH,   200).
-define(REPLAY_DATA, 201).

-define(FUN_DEFS,   300).
-define(FRESH_PID,  301).
-define(FRESH_TIME, 302).
-define(FRESH_VAR,  303).

-define(MULT_FWD, mult_fwd).
-define(MULT_BWD, mult_bwd).

-define(NOT_EXP,   not_exp).
-define(NULL_RULE, null_rule).
-define(NULL_OPT,  null_opt).

-define(PRINT_MAIL, print_mail).
-define(PRINT_HIST, print_hist).
-define(PRINT_ENV,  print_env).
-define(PRINT_EXP,  print_exp).
-define(PRINT_FULL, print_full).
-define(COMP_OPT,   comp_opt).
-define(PRINT_FULL_ENV, print_full_env).

-define(SCHED_RANDOM,      random).
-define(SCHED_PRIO_RANDOM, prio_random).

-define(CAUDER_GREEN, {34,139,34}).

-define(LAST_PATH, last_path).


% System
-record(sys, {
  sched = ?SCHED_PRIO_RANDOM :: ?SCHED_PRIO_RANDOM | ?SCHED_RANDOM,
  % Global mailbox
  msgs = [] :: [cauder_types:message()],
  % Pool of processes
  procs = [] :: [cauder_types:process()],
  trace = [] :: [cauder_types:trace()],
  roll = []
}).

%% Process
-record(proc, {
  % Process identifier
  pid :: cauder_types:af_integer(),
  % History
  hist = [] :: cauder_types:history(),
  % Log
  log = [],
  % Environment
  env = erl_eval:new_bindings() :: cauder_types:environment(),
  % List of expressions
  exprs :: [cauder_types:abstract_expr()],
  % Process mailbox
  mail = [] :: [cauder_types:process_message()],
  % The entry point function for this process
  spf :: undefined | {atom(), arity()}
}).

%% Message
-record(msg, {
  % Target process identifier
  dest :: cauder_types:af_integer(),
  % Message
  val :: cauder_types:abstract_expr(),
  % Timestamp
  time :: non_neg_integer()
}).

% Option
-record(opt, {
  % forward or backward
  sem :: ?FWD_SEM | ?BWD_SEM,
  % proc or msg
  type :: ?TYPE_PROC | ?TYPE_MSG,
  % integer
  id :: undefined | non_neg_integer(),
  % seq, spawn, ...
  rule :: ?RULE_SEQ | ?RULE_CHECK | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF | ?RULE_SCHED
}).

% Trace
-record(trace, {
  type :: ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN,
  from :: cauder_types:af_integer(),
  to :: undefined | cauder_types:af_integer(),
  val :: undefined | cauder_types:abstract_expr(),
  time :: undefined | non_neg_integer()
}).

-record(replay, {
  call,
  main_pid,
  log_path
}).
