-record(config, {
  current_expression = true :: boolean(),
  bindings = true :: boolean(),
  stack = true :: boolean(),
  log = true :: boolean(),
  history = true :: boolean(),
  mailbox = true :: boolean(),
  bindings_mode = relevant :: all | relevant,
  history_mode = concurrent :: full | concurrent,
  mailbox_mode = process :: all | process,
  status_bar = true :: boolean()
}).

-record(wx_state, {
  frame :: wxFrame:wxFrame(),
  menubar :: wxMenuBar:wxMenuBar(),
  content :: wxWindow:wxWindow(),
  statusbar :: wxStatusBar:wxStatusBar(),

  config = #config{} :: #config{},

  module :: atom() | undefined,
  position = -1 :: integer(),
  task :: atom() | undefined,
  system :: cauder_types:system() | undefined,
  pid :: cauder_types:proc_id() | undefined
}).

-define(GUI_DB, 'cauder_wx/database').

-define(BINDINGS_IDX_TO_KEY, 'bindings_index_to_key').

-define(FRAME, 500).

-define(FRAME_SIZE_INIT, {1250, 775}).

-define(ZOOM_MIN, -10).
-define(ZOOM_MAX, 20).
-define(ZOOM_DEFAULT, (?ZOOM_MIN + ?FONT_SIZE_APPARENT_DEFAULT - ?FONT_SIZE_MIN)).

-define(FONT_SIZE_MIN, 10).
% Change if you want a different initial font size
-define(FONT_SIZE_APPARENT_DEFAULT, 9).
% DO NOT CHANGE. This is the actual default font size taking into account zoom levels
-define(FONT_SIZE_ACTUAL_DEFAULT, (?FONT_SIZE_APPARENT_DEFAULT - ?ZOOM_DEFAULT)).

-define(MAX_STEPS, 10000).

-define(ICON_ALIVE, "🏃").
-define(ICON_DEAD, "💀").

%% -----

-define(SPACER_SMALL, 5).
-define(SPACER_MEDIUM, 7).
-define(SPACER_LARGE, 10).

%% -------------------- Component IDs -------------------- %%

%% ---------- Menu Bar ---------- %%

-define(MENU_File_Open, ?wxID_OPEN).
-define(MENU_File_Exit, ?wxID_EXIT).

%% -----

-define(MENU_Edit_Undo, ?wxID_UNDO).
-define(MENU_Edit_Redo, ?wxID_REDO).

%% -----

-define(MENU_View_ZoomIn, ?wxID_ZOOM_IN).
-define(MENU_View_ZoomOut, ?wxID_ZOOM_OUT).
-define(MENU_View_Zoom100, ?wxID_ZOOM_100).

-define(MENU_View_CurrentExpression, 1000).

-define(Is_Visibility_Item(Item), (Item =:= ?MENU_View_Bindings orelse Item =:= ?MENU_View_Stack orelse Item =:= ?MENU_View_Log orelse Item =:= ?MENU_View_History orelse Item =:= ?MENU_View_Mailbox)).

-define(MENU_View_Bindings, 1001).
-define(MENU_View_Stack, 1002).
-define(MENU_View_Log, 1003).
-define(MENU_View_History, 1004).

-define(MENU_View_Mailbox, 1005).

-define(Is_Bindings_Mode(Item), (Item =:= ?MENU_View_RelevantBindings orelse Item =:= ?MENU_View_AllBindings)).

-define(MENU_View_RelevantBindings, 1006).
-define(MENU_View_AllBindings, 1007).

-define(Is_History_Mode(Item), (Item =:= ?MENU_View_ConcurrentHistory orelse Item =:= ?MENU_View_FullHistory)).

-define(MENU_View_ConcurrentHistory, 1008).
-define(MENU_View_FullHistory, 1009).

-define(Is_Message_Mode(Item), (Item =:= ?MENU_View_ProcessMessages orelse Item =:= ?MENU_View_AllMessages)).

-define(MENU_View_ProcessMessages, 1010).
-define(MENU_View_AllMessages, 1011).

-define(MENU_View_StatusBar, 1012).

%% -----

-define(MENU_Run_Start, 1100).
-define(MENU_Run_Stop, 1101).

%% -----

-define(MENU_Help_ViewHelp, 1200).

-define(MENU_Help_About, ?wxID_ABOUT).


%% ---------- Main Panel ---------- %%

-define(CODE_Code_Control, 2001).
-define(CODE_Expression_Control, 2002).

%% -----

-define(ACTION_Process, 2100).

%% -----

-define(ACTION_Manual, 2110).

-define(ACTION_Manual_Steps, 2111).
-define(ACTION_Manual_Scheduler, 2112).
-define(ACTION_Manual_Forward_Button, 2114).
-define(ACTION_Manual_Backward_Button, 2115).

-define(Is_Step_Button(Button), (Button =:= ?ACTION_Manual_Forward_Button orelse Button =:= ?ACTION_Manual_Backward_Button)).

%% -----

-define(ACTION_Automatic, 2130).

-define(ACTION_Automatic_Steps, 2131).
-define(ACTION_Automatic_Scheduler, 2132).
-define(ACTION_Automatic_Forward_Button, 2133).
-define(ACTION_Automatic_Backward_Button, 2134).

-define(Is_Automatic_Button(Button), (Button =:= ?ACTION_Automatic_Forward_Button orelse Button =:= ?ACTION_Automatic_Backward_Button)).

%% -----

-define(ACTION_Replay, 21300).

-define(ACTION_Replay_Steps, 21301).
-define(ACTION_Replay_Spawn, 21302).
-define(ACTION_Replay_Send, 21303).
-define(ACTION_Replay_Receive, 21304).
-define(ACTION_Replay_Start, 21305).


-define(ACTION_Replay_Steps_Button, 21306).
-define(ACTION_Replay_Spawn_Button, 21307).
-define(ACTION_Replay_Send_Button, 21308).
-define(ACTION_Replay_Receive_Button, 21309).
-define(ACTION_Replay_Start_Button, 2134).
-define(ACTION_Replay_FullLog_Button, 21311).

%% -----

-define(ACTION_Rollback, 2170).

-define(ACTION_Rollback_Steps, 2141).
-define(ACTION_Rollback_Spawn, 2142).
-define(ACTION_Rollback_Send, 2143).
-define(ACTION_Rollback_Receive, 2144).
-define(ACTION_Rollback_Variable, 2145).
-define(ACTION_Rollback_Start, 2146).

-define(ACTION_Rollback_Steps_Button, 2147).
-define(ACTION_Rollback_Spawn_Button, 2148).
-define(ACTION_Rollback_Send_Button, 2149).
-define(ACTION_Rollback_Receive_Button, 2150).
-define(ACTION_Rollback_Variable_Button, 2151).
-define(ACTION_Rollback_Start_Button, 2152).

%% ----- System Info Panel ----- %%

-define(SYSTEM_NodesAndMail, 2200).
-define(SYSTEM_NodesAndMail_Nodes, 0).
-define(SYSTEM_NodesAndMail_Mail, 1).


-define(SYSTEM_Notebook, 2210).
-define(SYSTEM_Notebook_Trace, 2).
-define(SYSTEM_Notebook_RollLog, 3).

-define(SYSTEM_Trace, 2211).
-define(SYSTEM_RollLog, 2212).


%% ----- Process Info Panel ----- %%

-define(PROCESS_Panel, 2300).

-define(PROCESS_Bindings_Panel, 2301).
-define(PROCESS_Bindings_Control, 2302).

-define(PROCESS_Stack_Panel, 2303).
-define(PROCESS_Stack_Control, 2304).

-define(PROCESS_Log_Panel, 2305).
-define(PROCESS_Log_Control, 2306).

-define(PROCESS_History_Panel, 2307).
-define(PROCESS_History_Control, 2308).


%% ---------- Status bar ---------- %%

-define(STATUS_BAR, 2400).


%% -------------------- Process Scheduler strings -------------------- %%

-define(SCHEDULER_RoundRobin_Name, "Round-robin").
-define(SCHEDULER_FCFS_Name, "First come, first served").
-define(SCHEDULER_Random_Name, "Random").
-define(SCHEDULER_Manual_Name, "Manual").

%% -------------------- Help strings -------------------- %%

-define(HELP_File_Open, "Open an Erlang file").
-define(HELP_File_Exit, "Quit this program").

-define(HELP_View_Zoom100, "Reset text font size").
-define(HELP_View_ZoomIn, "Increase text font size").
-define(HELP_View_ZoomOut, "Decrease text font size").

-define(HELP_View_CurrentExpression, "Show current expressions").

-define(HELP_View_Bindings, "Show process bindings").
-define(HELP_View_Stack, "Show process call stack").
-define(HELP_View_Log, "Show process logs").
-define(HELP_View_History, "Show process history").

-define(HELP_View_Mailbox, "Show system mailbox").

-define(HELP_View_RelevantBindings, "Show only bindings relevant in the current scope").
-define(HELP_View_AllBindings, "Show all bindings").

-define(HELP_View_ConcurrentHistory, "Show only concurrent history").
-define(HELP_View_FullHistory, "Show complete history").

-define(HELP_View_ProcessMessages, "Show only current process messages").
-define(HELP_View_AllMessages, "Show all messages").

-define(HELP_View_StatusBar, "Show or hide status bar").

-define(HELP_Run_Start, "Start a new debugging session").
-define(HELP_Run_Stop, "Stop the current debugging session").


%% -------------------- Dialog strings -------------------- %%

-define(DIALOG_StartSession_Title, "Start debugging session").

-define(DIALOG_StartSession_NodeName_Title, "Warning: Node name").
-define(DIALOG_StartSession_NodeName_Message, "Node name not correct, the node name must be of the form 'name@host'").

-define(DIALOG_StartSession_ArgCount_Title, "Warming: Argument count").
-define(DIALOG_StartSession_ArgCount_Message, "Wrong number of argumments!\nExpected ~b but got ~b.").

-define(DIALOG_BadArgs_Title, "Error: Invalid arguments").
-define(DIALOG_BadArgs_Message, "Invalid arguments!").

-define(DIALOG_StopSession_Title, "Stop debugging session").
-define(DIALOG_StopSession_Message, "You are about to stop the current debugging session.\nThe current system state will be lost.\nAre you sure you want to continue?").

-define(DIALOG_DropFiles_Unsupported_Title, "Unsupported files").
-define(DIALOG_DropFiles_Unsupported_Message, "Some of the dropped files are not supported.\nOnly files with the extension .erl are supported.").

-define(DIALOG_DropFiles_Multiple_Title, "Multiple files").
-define(DIALOG_DropFiles_Multiple_Message, "Currently only one file can be opened at a time.\nPlease, choose one of the files:").

-define(DIALOG_About, "A Causal-Consistent Reversible Debugger for Erlang.").
