-define(FRAME, 500).

-define(FRAME_SIZE_INIT, {900, 800}).

-define(ZOOM_MIN, -10).
-define(ZOOM_MAX, 20).
-define(ZOOM_DEFAULT, ?ZOOM_MIN + ?FONT_SIZE_APPARENT_DEFAULT - ?FONT_SIZE_MIN).

-define(FONT_SIZE_MIN, 7).
% Change if you want a different initial font size
-define(FONT_SIZE_APPARENT_DEFAULT, 9).
% DO NOT CHANGE. This is the actual default font size taking into account zoom levels
-define(FONT_SIZE_ACTUAL_DEFAULT, ?FONT_SIZE_APPARENT_DEFAULT - (?ZOOM_DEFAULT)).


-define(SPACER_SMALL, 5).
-define(SPACER_MEDIUM, 7).
-define(SPACER_LARGE, 10).


%% ========== Menu Bar ========== %%

%% File Menu

-define(MENU_File_Open, ?wxID_OPEN).
-define(MENU_File_Exit, ?wxID_EXIT).

%% Edit Menu

-define(MENU_Edit_Undo, ?wxID_UNDO).
-define(MENU_Edit_Redo, ?wxID_REDO).

%% View Menu

-define(MENU_View_ZoomIn, ?wxID_ZOOM_IN).
-define(MENU_View_ZoomOut, ?wxID_ZOOM_OUT).
-define(MENU_View_Zoom100, ?wxID_ZOOM_100).

-define(MENU_View_Mailbox, 1000).
-define(MENU_View_Log, 1001).
-define(MENU_View_History, 1002).
-define(MENU_View_Stack, 1003).
-define(MENU_View_Environment, 1004).
-define(MENU_View_CurrentExpression, 1005).

-define(IS_HISTORY_MODE(HistoryMode), HistoryMode =:= ?MENU_View_ConcurrentHistory orelse HistoryMode =:= ?MENU_View_FullHistory).

-define(MENU_View_ConcurrentHistory, 1006).
-define(MENU_View_FullHistory, 1007).

-define(IS_ENVIRONMENT_MODE(EnvironmentMode), EnvironmentMode =:= ?MENU_View_RelevantEnvironment orelse EnvironmentMode =:= ?MENU_View_FullEnvironment).

-define(MENU_View_RelevantEnvironment, 1008).
-define(MENU_View_FullEnvironment, 1009).

-define(MENU_View_StatusBar, 1010).

%% Run Menu

-define(MENU_Run_Start, 1011).
-define(MENU_Run_Stop, 1012).

%% Help Menu

-define(MENU_Help_ViewHelp, 1013).

-define(MENU_Help_About, ?wxID_ABOUT).


%% ========== Main Panel ========== %%

%% ===== Code Panel ===== %%

-define(CODE_TEXT, 2001).
-define(EXPR_TEXT, 2002).

%% ===== Action Panel ===== %%

-define(MAX_STEPS, 10000).

%% ----- Manual Panel ----- %%

-define(ACTION_Manual, 2100).

-define(PROC_CHOICE, 2101).

-define(STEP_FORWARD_BUTTON, 2102).
-define(STEP_BACKWARD_BUTTON, 2103).

-define(IS_STEP_BUTTON(Button), (Button =:= ?STEP_FORWARD_BUTTON) orelse (Button =:= ?STEP_BACKWARD_BUTTON)).

-define(STEP_OVER_FORWARD_BUTTON, 2104).
-define(STEP_OVER_BACKWARD_BUTTON, 2105).

-define(IS_STEP_OVER_BUTTON(Button), (Button =:= ?STEP_OVER_FORWARD_BUTTON) orelse (Button =:= ?STEP_OVER_BACKWARD_BUTTON)).

-define(STEP_INTO_FORWARD_BUTTON, 2106).
-define(STEP_INTO_BACKWARD_BUTTON, 2107).

-define(IS_STEP_INTO_BUTTON(Button), (Button =:= ?STEP_INTO_FORWARD_BUTTON) orelse (Button =:= ?STEP_INTO_BACKWARD_BUTTON)).

%% ----- Automatic Panel ----- %%

-define(ACTION_Automatic, 2110).

-define(STEPS_SPIN, 2111).

-define(MULTIPLE_FORWARD_BUTTON, 2112).
-define(MULTIPLE_BACKWARD_BUTTON, 2113).

-define(IS_MULT_BUTTON(Button), (Button =:= ?MULTIPLE_FORWARD_BUTTON) orelse (Button =:= ?MULTIPLE_BACKWARD_BUTTON)).

%% ----- Replay Panel ----- %%

-define(ACTION_Replay, 2120).

-define(REPLAY_STEPS_SPIN, 2121).
-define(REPLAY_SPAWN_TEXT, 2122).
-define(REPLAY_SEND_TEXT, 2123).
-define(REPLAY_REC_TEXT, 2124).

%%-define(REPLAY_INPUTS, [
%%  ?REPLAY_STEPS_SPIN,
%%  ?REPLAY_SPAWN_TEXT,
%%  ?REPLAY_SEND_TEXT,
%%  ?REPLAY_REC_TEXT
%%]).

-define(REPLAY_STEPS_BUTTON, 2125).
-define(REPLAY_SPAWN_BUTTON, 2126).
-define(REPLAY_SEND_BUTTON, 2127).
-define(REPLAY_REC_BUTTON, 2128).

%%-define(REPLAY_BUTTONS, [
%%  ?REPLAY_STEPS_BUTTON,
%%  ?REPLAY_SPAWN_BUTTON,
%%  ?REPLAY_SEND_BUTTON,
%%  ?REPLAY_REC_BUTTON
%%]).

%% ----- Rollback Panel ----- %%

-define(ACTION_Rollback, 2140).

-define(ROLL_STEPS_SPIN, 2141).
-define(ROLL_SPAWN_TEXT, 2142).
-define(ROLL_SEND_TEXT, 2143).
-define(ROLL_REC_TEXT, 2144).
-define(ROLL_VAR_TEXT, 2145).

%%-define(ROLL_INPUTS, [
%%  ?ROLL_STEPS_SPIN,
%%  ?ROLL_SPAWN_TEXT,
%%  ?ROLL_SEND_TEXT,
%%  ?ROLL_REC_TEXT,
%%  ?ROLL_VAR_TEXT
%%]).

-define(ROLL_STEPS_BUTTON, 2146).
-define(ROLL_SPAWN_BUTTON, 2147).
-define(ROLL_SEND_BUTTON, 2148).
-define(ROLL_REC_BUTTON, 2149).
-define(ROLL_VAR_BUTTON, 2150).

%%-define(ROLL_BUTTONS, [
%%  ?ROLL_STEPS_BUTTON,
%%  ?ROLL_SPAWN_BUTTON,
%%  ?ROLL_SEND_BUTTON,
%%  ?ROLL_REC_BUTTON,
%%  ?ROLL_VAR_BUTTON
%%]).


%% ===== %%


%%-define(ALL_INPUTS, [
%%  ?PROC_CHOICE,
%%  ?STEPS_SPIN |
%%  ?REPLAY_INPUTS ++ ?ROLL_INPUTS
%%]).

%%-define(ALL_BUTTONS, [
%%  ?STEP_FORWARD_BUTTON, ?STEP_BACKWARD_BUTTON,
%%  ?STEP_OVER_FORWARD_BUTTON, ?STEP_OVER_BACKWARD_BUTTON,
%%  ?STEP_INTO_FORWARD_BUTTON, ?STEP_INTO_BACKWARD_BUTTON,
%%  ?MULTIPLE_FORWARD_BUTTON, ?MULTIPLE_BACKWARD_BUTTON |
%%  ?REPLAY_BUTTONS ++ ?ROLL_BUTTONS
%%]).


%% ===== System Info Panel ===== %%

-define(MAIL_LIST, 2199).

-define(SYSTEM_INFO_NOTEBOOK, 2200).
-define(PAGEPOS_TRACE, 0).
-define(PAGEPOS_ROLL, 1).

-define(TRACE_LIST, 2201).
-define(ROLL_LOG_LIST, 2202).


%% ===== Process Info Panel ===== %%

-define(BINDINGS_LIST, 2222).
-define(STACK_LIST, 2223).
-define(LOG_TEXT, 2224).
-define(HISTORY_TEXT, 2225).


%% ========== Status bar ========== %%

-define(STATUS_BAR, 2300).


%% =====================================================================


-define(INFO_TEXT, "A Causal-consistent Debugger for Erlang. More info at: https://github.com/mistupv/cauder").

-define(ERROR_NUM_STEP, "The number of steps is not correct.").
-define(ERROR_NUM_ARGS, "The number of arguments is not correct.").

-define(HELP_OPEN_ITEM, "Open and compile an Erlang file").
-define(HELP_REPLAY_ITEM, "Replay an execution from a log file").
-define(HELP_QUIT_ITEM, "Quit this program").
-define(HELP_View_Zoom100, "Reset text font size").
-define(HELP_View_ZoomIn, "Increase text font size").
-define(HELP_View_ZoomOut, "Decrease text font size").
-define(HELP_View_Mailbox, "Show or hide process mailbox").
-define(HELP_View_History, "Show or hide process history").
-define(HELP_View_Log, "Show or hide process logs").
-define(HELP_View_Stack, "Show or hide process stack").
-define(HELP_View_Environment, "Show or hide process environment").
-define(HELP_View_CurrentExpression, "Show or hide process expressions").
-define(HELP_View_ConcurrentHistory, "Show only concurrent history").
-define(HELP_View_FullHistory, "Show complete history").
-define(HELP_View_RelevantEnvironment, "Show relevant bindings from environment").
-define(HELP_View_FullEnvironment, "Show all bindings from environment").
-define(HELP_TOGGLE_COMP, "Allow compiler optimizations when loading files").
-define(HELP_RADIO_RAND, "Set scheduler to random choice among options").
-define(HELP_RADIO_PRIO, "Set scheduler to random choice among options (priority to process options)").


-define(MESSAGE_Session_Stop, "You are about to stop the current debugging session.\nThe current system state will be lost.\nAre you sure you want to continue?").
