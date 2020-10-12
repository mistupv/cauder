-define(APPNAME, "CauDEr").
-define(WEBPAGE, "https://github.com/mistupv/cauder-v2").

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


% System
-record(sys, {
  % Global mailbox
  mail = [] :: [cauder_types:message()],
  % Pool of processes
  procs = [] :: cauder_types:process_dict(),
  % System log
  logs = [] :: cauder_types:log_dict(),
  trace = [] :: [cauder_types:trace()],
  roll = []
}).

%% Process
-record(proc, {
  % Process identifier
  pid :: cauder_types:proc_id(),
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
  dest :: cauder_types:proc_id(),
  % Message
  val :: term(),
  % UID
  uid :: cauder_types:msg_id()
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
  type :: ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE,
  from :: cauder_types:proc_id(),
  to :: undefined | cauder_types:proc_id(),
  val :: undefined | term(),
  time :: undefined | cauder_types:msg_id()
}).

% Replay info
-record(replay, {
  log_path :: file:filename(),
  call :: {atom(), atom(), [cauder_types:abstract_expr()]},
  main_pid :: cauder_types:proc_id()
}).

% Evaluation step result
-record(result, {
  env :: cauder_types:environment(),
  exprs :: [cauder_types:abstract_expr()],
  stack :: cauder_types:stack(),
  label = tau :: cauder_types:label()
}).



-define(EVAL_MANUAL(Type, Args), {eval, {manual, Type, Args}}).
-define(EVAL_AUTOMATIC(Args), {eval, {automatic, Args}}).
-define(EVAL_REPLAY(Type, Args), {eval, {replay, Type, Args}}).
-define(EVAL_ROLLBACK(Type, Args), {eval, {rollback, Type, Args}}).
