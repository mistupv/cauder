-record(status, {
  loaded = false :: boolean(),
  running = false :: boolean()
}).

-define(FRAME, 500).

-define(FRAME_SIZE_INIT, {900, 750}).

-define(ZOOM_MIN, -10).
-define(ZOOM_MAX, 20).
-define(ZOOM_DEFAULT, ?ZOOM_MIN + ?FONT_SIZE_APPARENT_DEFAULT - ?FONT_SIZE_MIN).

-define(FONT_SIZE_MIN, 7).
% Change if you want a different initial font size
-define(FONT_SIZE_APPARENT_DEFAULT, 9).
% DO NOT CHANGE. This is the actual default font size taking into account zoom levels
-define(FONT_SIZE_ACTUAL_DEFAULT, ?FONT_SIZE_APPARENT_DEFAULT - (?ZOOM_DEFAULT)).


%% ========== Menu Bar ========== %%

%% File Menu

-define(OPEN,             ?wxID_OPEN).
-define(LOAD_TRACE,       1000).
-define(EXIT,             ?wxID_EXIT).

%% View Menu

-define(MENU_VIEW,        1100).

-define(BUTTON_ZOOM_IN,   ?wxID_ZOOM_IN).
-define(BUTTON_ZOOM_OUT,  ?wxID_ZOOM_OUT).
-define(BUTTON_ZOOM_100,  ?wxID_ZOOM_100).

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


%% ========== Main Panel ========== %%

%% ===== Code Panel ===== %%

-define(CODE_TEXT,                2001).

%% ===== Action Panel ===== %%

-define(ACTION_NOTEBOOK,          2100).

%% ----- Manual Panel ----- %%

-define(PROC_CHOICE,              2101).

-define(SINGLE_FORWARD_BUTTON,    2102).
-define(SINGLE_BACKWARD_BUTTON,   2104).

%% ----- Automatic Panel ----- %%

-define(STEPS_SPIN, 2106).

-define(MULTIPLE_FORWARD_BUTTON,  2107).
-define(MULTIPLE_BACKWARD_BUTTON, 2108).

%% ----- Replay Panel ----- %%

-define(REPLAY_STEPS_SPIN,        2111).
-define(REPLAY_SPAWN_TEXT,        2113).
-define(REPLAY_SEND_TEXT,         2112).
-define(REPLAY_REC_TEXT,          2114).

-define(REPLAY_INPUTS, [
  ?REPLAY_STEPS_SPIN,
  ?REPLAY_SPAWN_TEXT,
  ?REPLAY_SEND_TEXT,
  ?REPLAY_REC_TEXT
]).

-define(REPLAY_STEPS_BUTTON,      2115).
-define(REPLAY_SPAWN_BUTTON,      2117).
-define(REPLAY_SEND_BUTTON,       2116).
-define(REPLAY_REC_BUTTON,        2118).

-define(REPLAY_BUTTONS, [
  ?REPLAY_STEPS_BUTTON,
  ?REPLAY_SPAWN_BUTTON,
  ?REPLAY_SEND_BUTTON,
  ?REPLAY_REC_BUTTON
]).

%% ----- Rollback Panel ----- %%

-define(ROLL_STEPS_SPIN,          2120).
-define(ROLL_SPAWN_TEXT,          2122).
-define(ROLL_SEND_TEXT,           2121).
-define(ROLL_REC_TEXT,            2123).
-define(ROLL_VAR_TEXT,            2124).

-define(ROLL_INPUTS, [
  ?ROLL_STEPS_SPIN,
  ?ROLL_SPAWN_TEXT,
  ?ROLL_SEND_TEXT,
  ?ROLL_REC_TEXT,
  ?ROLL_VAR_TEXT
]).

-define(ROLL_STEPS_BUTTON,        2125).
-define(ROLL_SPAWN_BUTTON,        2127).
-define(ROLL_SEND_BUTTON,         2126).
-define(ROLL_REC_BUTTON,          2128).
-define(ROLL_VAR_BUTTON,          2129).

-define(ROLL_BUTTONS, [
  ?ROLL_STEPS_BUTTON,
  ?ROLL_SPAWN_BUTTON,
  ?ROLL_SEND_BUTTON,
  ?ROLL_REC_BUTTON,
  ?ROLL_VAR_BUTTON
]).


%% ===== %%


-define(ALL_INPUTS, [
  ?PROC_CHOICE,
  ?STEPS_SPIN |
  ?REPLAY_INPUTS ++ ?ROLL_INPUTS
]).

-define(ALL_BUTTONS, [
  ?SINGLE_FORWARD_BUTTON, ?SINGLE_BACKWARD_BUTTON,
  ?MULTIPLE_FORWARD_BUTTON, ?MULTIPLE_BACKWARD_BUTTON |
  ?REPLAY_BUTTONS ++ ?ROLL_BUTTONS
]).


%% ===== System Info Panel ===== %%

-define(MAIL_LIST,            2199).

-define(SYSTEM_INFO_NOTEBOOK, 2200).
-define(PAGEPOS_TRACE,        0).
-define(PAGEPOS_ROLL,         1).

-define(TRACE_LIST,           2201).
-define(ROLL_LOG_LIST,        2202).


%% ===== Process Info Panel ===== %%

-define(BINDINGS_LIST,        2222).
-define(STACK_LIST,           2223).
-define(LOG_TEXT,             2224).
-define(HISTORY_TEXT,         2225).


%% ========== Status bar ========== %%

-define(STATUS_BAR,           2300).


%% =====================================================================


%% =====================================================================


%%-define(LEFT_NOTEBOOK, 2000).
%%
%%-define(FUN_SIZER, 2002).
%%-define(FUN_CHOICE, 2003).
%%-define(ARGS_TEXT, 2004).
%%-define(START_BUTTON, 2005).
%%
%%%% ========== State Panel ========== %%
%%
%%-define(STATE_TEXT, 2006).


%% =====================================================================


-define(SYSTEM, 601).
-define(STATUS, 602).


%% =====================================================================


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
