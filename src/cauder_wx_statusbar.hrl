-define(NO_PROCESS, "Cannot perform any action because no process is selected").
-define(NO_MATCH, "Cannot perform any forward action because there is no matching message to be received").

-define(LOAD_START, "Loading file ~s").
-define(LOAD_FINISH, "Loaded module ~p in ~s").
-define(LOAD_FAIL, "Failed to load file, there are compilation errors").

-define(INIT_START, "Starting system...").
-define(INIT_MANUAL_FINISH, "Started system manual mode in ~s").
-define(INIT_REPLAY_FINISH, "Started system in replay mode in ~s").

-define(STOP_FINISH, "Stopped system!").

-define(STEP_START, "Performing ~s steps...").
-define(STEP_FINISH, "Performed ~b (of ~b) ~s steps in ~s").
-define(STEP_SUSPEND, "Suspended execution").

-define(STEP_MULTIPLE_FINISH, "Performed ~b (of ~b) ~s steps in ~s").

-define(REPLAY_STEPS_START, "Replaying steps...").
-define(REPLAY_STEPS_FINISH, "Replayed ~b of ~b steps in ~s").

-define(REPLAY_START_START, "Replaying the starting of node ~p...").
-define(REPLAY_START_FINISH, "Replayed the starting of node ~p in ~s").
-define(REPLAY_START_FAIL, "Could not replay the starting of that node").

-define(REPLAY_SPAWN_START, "Replaying the spawning of the process with PID ~p...").
-define(REPLAY_SPAWN_FINISH, "Replayed the spawning of the process with PID ~p in ~s").
-define(REPLAY_SPAWN_FAIL, "Could not replay the spawning of that process").

-define(REPLAY_SEND_START, "Replaying the sending of the message with UID ~p...").
-define(REPLAY_SEND_FINISH, "Replayed the sending of the message with UID ~p in ~s").
-define(REPLAY_SEND_FAIL, "Could not replay the sending of that message").

-define(REPLAY_RECEIVE_START, "Replaying the reception of the message with UID ~p...").
-define(REPLAY_RECEIVE_FINISH, "Replayed the reception of the message with UID ~p in ~s").
-define(REPLAY_RECEIVE_FAIL, "Could not replay the reception of that message").

-define(REPLAY_FULL_LOG_START, "Replaying the full log...").
-define(REPLAY_FULL_LOG_FINISH, "Replayed the full log in ~s").

-define(ROLLBACK_STEPS_START, "Rolling back steps...").
-define(ROLLBACK_STEPS_FINISH, "Rolled back ~b of ~b steps in ~s").

-define(ROLLBACK_START_BEGIN, "Rolling back the start of the node with name ~p...").
-define(ROLLBACK_START_FINISH, "Rolled back the starting of the node with name ~p in ~s").
-define(ROLLBACK_START_FAIL, "Could not rollback the start of that node").

-define(ROLLBACK_SPAWN_START, "Rolling back the spawning of the process with PID ~p...").
-define(ROLLBACK_SPAWN_FINISH, "Rolled back the spawning of the process with PID ~p in ~s").
-define(ROLLBACK_SPAWN_FAIL, "Could not rollback the spawning of that process").

-define(ROLLBACK_SEND_START, "Rolling back the sending of the message with UID ~p...").
-define(ROLLBACK_SEND_FINISH, "Rolled back the sending of the message with UID ~p in ~s").
-define(ROLLBACK_SEND_FAIL, "Could not rollback the sending of that message").

-define(ROLLBACK_SENDA_START, "Rolling back the sending of the message with UID ~p...").
-define(ROLLBACK_SENDA_FINISH, "Rolled back the sending of the message with UID ~p in ~s").
-define(ROLLBACK_SENDA_FAIL, "Could not rollback the sending of that message").

-define(ROLLBACK_RECEIVE_START, "Rolling back the reception of the message with UID ~p...").
-define(ROLLBACK_RECEIVE_FINISH, "Rolled back the reception of the message with UID ~p in ~s").
-define(ROLLBACK_RECEIVE_FAIL, "Could not rollback the reception of that message").

-define(ROLLBACK_REG_START, "Rolling back the register of the element ~p...").
-define(ROLLBACK_REG_FINISH, "Rolled back the register of the element ~p in ~s").
-define(ROLLBACK_REG_FAIL, "Could not rollback the register").

-define(ROLLBACK_DEL_START, "Rolling back the delete of the element ~p...").
-define(ROLLBACK_DEL_FINISH, "Rolled back the delete of the element ~p in ~s").
-define(ROLLBACK_DEL_FAIL, "Could not rollback the delete").

-define(ROLLBACK_VARIABLE_START, "Rolling back the binding of the variable with name ~p...").
-define(ROLLBACK_VARIABLE_FINISH, "Rolled back the binding of the variable with name ~p in ~s").
-define(ROLLBACK_VARIABLE_FAIL, "Could not rollback the binding of that variable").
