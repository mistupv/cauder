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

-define(CODE_Code, 2001).
-define(CODE_Expression, 2002).

%% ===== Action Panel ===== %%

-define(MAX_STEPS, 10000).

-define(ACTION_Process, 2100).

%% ----- Manual Panel ----- %%

-define(ACTION_Manual, 2110).

-define(ACTION_Manual_Step, 2111).
-define(ACTION_Manual_Step_Forward_Button, 2112).
-define(ACTION_Manual_Step_Backward_Button, 2113).

-define(Is_Step_Button(Button), (Button =:= ?ACTION_Manual_Step_Forward_Button) orelse (Button =:= ?ACTION_Manual_Step_Backward_Button)).

-define(ACTION_Manual_StepOver, 2114).
-define(ACTION_Manual_StepOver_Forward_Button, 2115).
-define(ACTION_Manual_StepOver_Backward_Button, 2116).

-define(Is_StepOver_Button(Button), (Button =:= ?ACTION_Manual_StepOver_Forward_Button) orelse (Button =:= ?ACTION_Manual_StepOver_Backward_Button)).

-define(ACTION_Manual_StepInto, 2117).
-define(ACTION_Manual_StepInto_Forward_Button, 2118).
-define(ACTION_Manual_StepInto_Backward_Button, 2119).

-define(Is_StepInto_Button(Button), (Button =:= ?ACTION_Manual_StepInto_Forward_Button) orelse (Button =:= ?ACTION_Manual_StepInto_Backward_Button)).

%% ----- Automatic Panel ----- %%

-define(ACTION_Automatic, 2120).

-define(ACTION_Automatic_Steps, 2121).
-define(ACTION_Automatic_Forward_Button, 2122).
-define(ACTION_Automatic_Backward_Button, 2123).

-define(Is_Automatic_Button(Button), (Button =:= ?ACTION_Automatic_Forward_Button) orelse (Button =:= ?ACTION_Automatic_Backward_Button)).

%% ----- Replay Panel ----- %%

-define(ACTION_Replay, 2130).

-define(ACTION_Replay_Steps, 2131).
-define(ACTION_Replay_Spawn, 2132).
-define(ACTION_Replay_Send, 2133).
-define(ACTION_Replay_Receive, 2134).

-define(ACTION_Replay_Steps_Button, 2135).
-define(ACTION_Replay_Spawn_Button, 2136).
-define(ACTION_Replay_Send_Button, 2137).
-define(ACTION_Replay_Receive_Button, 2138).

%% ----- Rollback Panel ----- %%

-define(ACTION_Rollback, 2140).

-define(ACTION_Rollback_Steps, 2141).
-define(ACTION_Rollback_Spawn, 2142).
-define(ACTION_Rollback_Send, 2143).
-define(ACTION_Rollback_Receive, 2144).
-define(ACTION_Rollback_Variable, 2145).

-define(ACTION_Rollback_Steps_Button, 2146).
-define(ACTION_Rollback_Spawn_Button, 2147).
-define(ACTION_Rollback_Send_Button, 2148).
-define(ACTION_Rollback_Receive_Button, 2149).
-define(ACTION_Rollback_Variable_Button, 2150).

%% ===== System Info Panel ===== %%

-define(SYSTEM_Mail, 2200).

-define(SYSTEM_Notebook, 2210).
-define(SYSTEM_Notebook_Trace, 0).
-define(SYSTEM_Notebook_RollLog, 1).

-define(SYSTEM_Trace, 2211).
-define(SYSTEM_RollLog, 2212).


%% ===== Process Info Panel ===== %%

-define(PROCESS_Bindings, 2300).
-define(PROCESS_Stack, 2301).
-define(PROCESS_Log, 2302).
-define(PROCESS_History, 2303).


%% ========== Status bar ========== %%

-define(STATUS_BAR, 2400).


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
