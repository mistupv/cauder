-record(status, {
  loaded = false :: boolean(),
  running = false :: boolean()
}).

-define(FRAME, 500).

-define(FRAME_SIZE_INIT, {800, 600}).
-define(FRAME_SIZE_MIN, {800, 600}).
-define(FRAME_SIZE_MAX, {800, 600}).

-define(DEFAULT_FONT_SIZE, 9).
-define(FONT_SIZES, [8, 9, 10, 11, 12, 14, 16, 18, 20, 22, 24, 26, 28, 36, 48, 72]).

%% =====================================================================

%% ========== Menu Bar ========== %%

%% File Menu

-define(OPEN,             ?wxID_OPEN).
-define(REPLAY,           1000).
-define(EXIT,             ?wxID_EXIT).

%% View Menu

-define(MENU_VIEW,        1100).

-define(ZOOM_IN,          ?wxID_ZOOM_IN).
-define(ZOOM_OUT,         ?wxID_ZOOM_OUT).
-define(ZOOM_100,         ?wxID_ZOOM_100).

-define(TOGGLE_MAIL,      1110).
-define(TOGGLE_LOG,       1111).
-define(TOGGLE_HIST,      1112).
-define(TOGGLE_STACK,     1113).
-define(TOGGLE_ENV,       1114).
-define(TOGGLE_EXP,       1115).

-define(RADIO_CONC_HIST,  1120).
-define(RADIO_FULL_HIST,  1121).

-define(RADIO_REL_ENV,    1130).
-define(RADIO_FULL_ENV,   1131).

%% Compiler Menu

-define(MENU_COMP,        1200).

-define(TOGGLE_COMP,      1201).

%% Help Menu

-define(ABOUT,            ?wxID_ABOUT).


%% ==================== Left notebook ==================== %%

-define(LEFT_NOTEBOOK,    2000).

%% ========== Code Panel ========== %%

-define(CODE_TEXT,        2001).

-define(FUN_SIZER,        2002).
-define(FUN_CHOICE,       2003).
-define(ARGS_TEXT,        2004).
-define(START_BUTTON,     2005).

%% ========== State Panel ========== %%

-define(STATE_TEXT,       2006).


%% ==================== Right top notebook ==================== %%

-define(RIGHT_TOP_NOTEBOOK,     2100).

%% ========== Manual Panel ========== %%

-define(MANUAL_PID_TEXT,        2101).

-define(FWD_INT_BUTTON,         2102).
-define(BWD_INT_BUTTON,         2104).

%% ========== Automatic Panel ========== %%

-define(AUTO_STEP_TEXT,         2106).

-define(FORWARD_BUTTON,         2107).
-define(BACKWARD_BUTTON,        2108).
-define(NORMALIZE_BUTTON,       2109).

%% ========== Replay Panel ========== %%

-define(REPLAY_PID_TEXT,        2110).
-define(REPLAY_STEPS_TEXT,      2111).
-define(REPLAY_SEND_ID_TEXT,    2112).
-define(REPLAY_SPAWN_PID_TEXT,  2113).
-define(REPLAY_REC_ID_TEXT,     2114).

-define(REPLAY_BUTTON,          2115).
-define(REPLAY_SEND_BUTTON,     2116).
-define(REPLAY_SPAWN_BUTTON,    2117).
-define(REPLAY_REC_BUTTON,      2118).

%% ========== Rollback Panel ========== %%

-define(ROLL_PID_TEXT,          2119).
-define(ROLL_STEP_TEXT,         2120).
-define(ROLL_SEND_ID_TEXT,      2121).
-define(ROLL_SPAWN_PID_TEXT,    2122).
-define(ROLL_REC_ID_TEXT,       2123).
-define(ROLL_VAR_NAME_TEXT,     2124).

-define(ROLL_BUTTON,            2125).
-define(ROLL_SEND_BUTTON,       2126).
-define(ROLL_SPAWN_BUTTON,      2127).
-define(ROLL_REC_BUTTON,        2128).
-define(ROLL_VAR_BUTTON,        2129).


%% ==================== Right bottom notebook ==================== %%

-define(RIGHT_BOTTOM_NOTEBOOK,  2200).

%% ========== Trace panel ========== %%

-define(TRACE_TEXT,             2201).

%% ========== Roll log panel ========== %%

-define(ROLL_LOG_TEXT,          2202).


%% ==================== Status bar ==================== %%

-define(STATUS_BAR,             2300).


%% =====================================================================

-define(ALL_BUTTONS, [
  ?FWD_INT_BUTTON,
  ?BWD_INT_BUTTON,
  ?FORWARD_BUTTON,
  ?BACKWARD_BUTTON,
  ?NORMALIZE_BUTTON,
  ?REPLAY_BUTTON,
  ?REPLAY_SEND_BUTTON,
  ?REPLAY_SPAWN_BUTTON,
  ?REPLAY_REC_BUTTON,
  ?ROLL_BUTTON,
  ?ROLL_SEND_BUTTON,
  ?ROLL_SPAWN_BUTTON,
  ?ROLL_REC_BUTTON,
  ?ROLL_VAR_BUTTON
]).


%% =====================================================================

-define(SYSTEM, 601).
-define(STATUS, 602).


%% =====================================================================

-define(PAGEPOS_CODE,  0).
-define(PAGEPOS_STATE, 1).
-define(PAGEPOS_MANU,  0).
-define(PAGEPOS_SEMI,  1).
-define(PAGEPOS_AUTO,  2).
-define(PAGEPOS_TRACE, 0).
-define(PAGEPOS_ROLL,  1).

-define(INFO_TEXT, "A Causal-consistent Debugger for Erlang. More info at: https://github.com/mistupv/cauder").

-define(ERROR_NUM_STEP, "The number of steps is not correct.").
-define(ERROR_NUM_ARGS, "The number of arguments is not correct.").

-define(HELP_OPEN_ITEM,      "Open and compile an Erlang file").
-define(HELP_REPLAY_ITEM,    "Replay an execution from a log file").
-define(HELP_QUIT_ITEM,      "Quit this program").
-define(HELP_ZOOM_100_ITEM,  "Reset text font size").
-define(HELP_ZOOM_IN_ITEM,   "Increase text font size").
-define(HELP_ZOOM_OUT_ITEM,  "Decrease text font size").
-define(HELP_TOGGLE_MAIL,    "Show or hide process mailbox").
-define(HELP_TOGGLE_HIST,    "Show or hide process history").
-define(HELP_TOGGLE_LOG,     "Show or hide process logs").
-define(HELP_TOGGLE_STACK,   "Show or hide process stack").
-define(HELP_TOGGLE_ENV,     "Show or hide process environment").
-define(HELP_TOGGLE_EXP,     "Show or hide process expressions").
-define(HELP_RADIO_CONC,     "Show only concurrent history").
-define(HELP_RADIO_FULL,     "Show complete history").
-define(HELP_RADIO_REN_ENV,  "Show relevant bindings from environment").
-define(HELP_RADIO_FULL_ENV, "Show all bindings from environment").
-define(HELP_TOGGLE_COMP,    "Allow compiler optimizations when loading files").
-define(HELP_RADIO_RAND,     "Set scheduler to random choice among options").
-define(HELP_RADIO_PRIO,     "Set scheduler to random choice among options (priority to process options)").
