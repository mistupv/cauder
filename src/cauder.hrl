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

-record(msg, {dest,
              val,
              time}).

%% Process
-record(proc, {
  % Process PID
  pid :: erl_syntax:syntaxTree(), % TODO Less generic type
  % History
  hist = [],
  % Log
  log = [],
  % Environment
  env = [],
  % List of expressions
  exp :: [erl_syntax:syntaxTree()], % TODO Less generic type
  % Process mailbox
  mail = [] :: [#msg{}],
  % The entry point function for this process
  spf = undef :: undef | {atom(), arity()}
}).

-record(sys, {sched = ?SCHED_PRIO_RANDOM,
              msgs  = [],
              procs = [],
              trace = [],
              roll  = []}).

-record(opt, {sem,    % forward or backward
              type,   % proc or msg
              id,     % integer
              rule}). % seq, spawn, ...

-record(trace, {type,
                from,
                to,
                val,
                time}).

-record(replay, {call,
                 main_pid,
                 log_path}).

