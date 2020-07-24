-module(cauder_gui).
-export([setup_gui/0]).

-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


setup_gui() ->
  Server = wx:new(),
  Frame = wxFrame:new(Server, -1, ?APP_STRING, [{size, ?FRAME_SIZE_INIT}]),
  ref_start(),
  ref_add(?STATUS, #status{}),
  ref_add(?FRAME, Frame),
  setupMenu(),
  wxFrame:createStatusBar(Frame, [{id, ?STATUS_BAR}]),
  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  wxEvtHandler:connect(Frame, command_text_updated),
  setupMainPanel(Frame),
  wxFrame:show(Frame),
  loop(),
  utils_gui:stop_refs(),
  ref_stop().


setupMenu() ->
  MenuBar = wxMenuBar:new(),
  wxFrame:setMenuBar(ref_lookup(?FRAME), MenuBar),

  %% --------- File menu ---------- %%
  File = wxMenu:new(),
  wxMenuBar:append(MenuBar, File, "&File"),

  File_Open = wxMenu:append(File, ?OPEN, "&Open\tCtrl+O"),
  wxMenuItem:setHelp(File_Open, ?HELP_OPEN_ITEM),
  File_Replay = wxMenu:append(File, ?REPLAY, "Load &Replay Data\tCtrl+R"),
  wxMenuItem:setHelp(File_Replay, ?HELP_REPLAY_ITEM),
  wxMenuItem:enable(File_Replay, [{enable, false}]),
  ref_add(?REPLAY, File_Replay),

  wxMenu:appendSeparator(File),

  File_Quit = wxMenu:append(File, ?EXIT, "&Quit\tCtrl+Q"),
  wxMenuItem:setHelp(File_Quit, ?HELP_QUIT_ITEM),

  %% --------- View menu ---------- %%
  View = wxMenu:new(),
  wxMenuBar:append(MenuBar, View, "&View"),
  ref_add(?MENU_VIEW, View),

  View_ZoomIn = wxMenu:append(View, ?ZOOM_IN, "Zoom &In\tCtrl++"),
  wxMenuItem:setHelp(View_ZoomIn, ?HELP_ZOOM_IN_ITEM),
  wxMenuItem:enable(View_ZoomIn, [{enable, utils_gui:can_zoom_in(?DEFAULT_FONT_SIZE)}]),
  ref_add(?ZOOM_IN, View_ZoomIn),
  View_ZoomOut = wxMenu:append(View, ?ZOOM_OUT, "Zoom &Out\tCtrl+-"),
  wxMenuItem:setHelp(View_ZoomOut, ?HELP_ZOOM_OUT_ITEM),
  wxMenuItem:enable(View_ZoomOut, [{enable, utils_gui:can_zoom_out(?DEFAULT_FONT_SIZE)}]),
  ref_add(?ZOOM_OUT, View_ZoomOut),
  View_Zoom100 = wxMenu:append(View, ?ZOOM_100, "Zoom &100%\tCtrl+0"),
  wxMenuItem:setHelp(View_Zoom100, ?HELP_ZOOM_100_ITEM),
  ref_add(?ZOOM_100, View_Zoom100),

  wxMenu:appendSeparator(View),

  View_ToggleMail = wxMenu:appendCheckItem(View, ?TOGGLE_MAIL, "Show &Mailbox"),
  wxMenuItem:setHelp(View_ToggleMail, ?HELP_TOGGLE_MAIL),
  View_ToggleHist = wxMenu:appendCheckItem(View, ?TOGGLE_HIST, "Show &History"),
  wxMenuItem:setHelp(View_ToggleHist, ?HELP_TOGGLE_HIST),
  View_ToggleLog = wxMenu:appendCheckItem(View, ?TOGGLE_LOG, "Show &Log"),
  wxMenuItem:setHelp(View_ToggleLog, ?HELP_TOGGLE_LOG),
  View_ToggleStack = wxMenu:appendCheckItem(View, ?TOGGLE_STACK, "Show &Stack"),
  wxMenuItem:setHelp(View_ToggleStack, ?HELP_TOGGLE_STACK),
  View_ToggleEnv = wxMenu:appendCheckItem(View, ?TOGGLE_ENV, "Show &Environment"),
  wxMenuItem:setHelp(View_ToggleEnv, ?HELP_TOGGLE_ENV),
  View_ToggleExp = wxMenu:appendCheckItem(View, ?TOGGLE_EXP, "Show E&xpressions"),
  wxMenuItem:setHelp(View_ToggleExp, ?HELP_TOGGLE_EXP),

  wxMenu:appendSeparator(View),

  View_RadioConc = wxMenu:appendRadioItem(View, ?RADIO_CONC_HIST, "Concurrent History"),
  wxMenuItem:setHelp(View_RadioConc, ?HELP_RADIO_CONC),
  View_RadioFull = wxMenu:appendRadioItem(View, ?RADIO_FULL_HIST, "Full History"),
  wxMenuItem:setHelp(View_RadioFull, ?HELP_RADIO_FULL),

  wxMenu:appendSeparator(View),

  View_RadioRelEnv = wxMenu:appendRadioItem(View, ?RADIO_REL_ENV, "Relevant Environment"),
  wxMenuItem:setHelp(View_RadioRelEnv, ?HELP_RADIO_REN_ENV),
  View_RadioFullEnv = wxMenu:appendRadioItem(View, ?RADIO_FULL_ENV, "Full Environment"),
  wxMenuItem:setHelp(View_RadioFullEnv, ?HELP_RADIO_FULL_ENV),

  % Set default values
  wxMenuItem:check(View_ToggleMail),
  wxMenuItem:check(View_ToggleHist),
  wxMenuItem:check(View_ToggleLog),
  wxMenuItem:check(View_ToggleStack),
  wxMenuItem:check(View_ToggleEnv),
  wxMenuItem:check(View_ToggleExp),
  wxMenuItem:check(View_RadioConc),
  wxMenuItem:check(View_RadioRelEnv),

  %% --------- Compile menu ---------- %%
  Compile = wxMenu:new(),
  wxMenuBar:append(MenuBar, Compile, "&Compiler"),
  ref_add(?MENU_COMP, Compile),

  Compile_ToggleComp = wxMenu:appendCheckItem(Compile, ?TOGGLE_COMP, "Compiler &Optimizations"),
  wxMenuItem:setHelp(Compile_ToggleComp, ?HELP_TOGGLE_COMP),
  wxMenuItem:check(Compile_ToggleComp),

  %% --------- Help menu ---------- %%
  Help = wxMenu:new(),
  wxMenuBar:append(MenuBar, Help, "&Help"),

  wxMenu:append(Help, ?ABOUT, "&About").


setupMainPanel(Parent) ->
  MainPanel = wxPanel:new(Parent),
  MainSizer = wxBoxSizer:new(?wxHORIZONTAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],

  LeftPanel = wxPanel:new(MainPanel),
  LeftSizer = setupLeftSizer(LeftPanel),
  wxWindow:setSizerAndFit(LeftPanel, LeftSizer),

  RightPanel = wxPanel:new(MainPanel),
  RightSizer = setupRightSizer(RightPanel),
  wxWindow:setSizerAndFit(RightPanel, RightSizer),

  wxSizer:add(MainSizer, LeftPanel, SizerFlags),
  wxSizer:add(MainSizer, RightPanel, SizerFlags),
  wxWindow:setSizer(MainPanel, MainSizer),
  MainPanel.


%% =====================================================================

setupLeftSizer(Parent) ->
  Notebook = wxNotebook:new(Parent, ?LEFT_NOTEBOOK),
  ref_add(?LEFT_NOTEBOOK, Notebook),
  CodePanel = setupCodePanel(Notebook),
  StatePanel = setupStatePanel(Notebook),
  wxNotebook:addPage(Notebook, CodePanel, "Code"),
  wxNotebook:addPage(Notebook, StatePanel, "State"),
  % wxNotebook:layout(Notebook),
  LeftSizer = wxBoxSizer:new(?wxVERTICAL),
  SizerFlags = [{proportion, 1}, {flag, ?wxEXPAND}],
  wxSizer:add(LeftSizer, Notebook, SizerFlags),
  LeftSizer.


%% ========== Left sizer ========== %%

setupCodePanel(Parent) ->
  CodePanel = wxPanel:new(Parent),

  % Code text
  CodeTextCtrl = wxTextCtrl:new(CodePanel, ?CODE_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  wxWindow:setFont(CodeTextCtrl, wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)),
  ref_add(?CODE_TEXT, CodeTextCtrl),

  % Function selection
  FunStaticText = wxStaticText:new(CodePanel, ?wxID_ANY, "Fun: "),
  FunChoice = wxChoice:new(CodePanel, ?FUN_CHOICE),
  ref_add(?FUN_CHOICE, FunChoice),

  % Function arguments
  ArgsStaticText = wxStaticText:new(CodePanel, ?wxID_ANY, "Args: "),
  ArgsTextCtrl = wxTextCtrl:new(CodePanel, ?ARGS_TEXT),
  ref_add(?ARGS_TEXT, ArgsTextCtrl),

  % Start button
  StartButton = wxButton:new(CodePanel, ?START_BUTTON, [{label, "START"}]),
  wxButton:disable(StartButton),
  ref_add(?START_BUTTON, StartButton),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  CodeSizer = wxBoxSizer:new(?wxVERTICAL),
  FunSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ref_add(?FUN_SIZER, FunSizer),

  % Populate sizers
  wxWindow:setSizer(CodePanel, BorderSizer),
  wxSizer:add(BorderSizer, CodeSizer, [{proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}, {border, 10}]),

  wxSizer:add(CodeSizer, CodeTextCtrl, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(CodeSizer, 10),
  wxSizer:add(CodeSizer, FunSizer, [{flag, ?wxEXPAND}]),

  wxSizer:add(FunSizer, FunStaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:add(FunSizer, FunChoice, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:addSpacer(FunSizer, 10),
  wxSizer:add(FunSizer, ArgsStaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:add(FunSizer, ArgsTextCtrl, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:addSpacer(FunSizer, 10),
  wxSizer:add(FunSizer, StartButton, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

  CodePanel.

setupStatePanel(Parent) ->
  StatePanel = wxPanel:new(Parent),

  % State text
  StateTextCtrl = wxTextCtrl:new(StatePanel, ?STATE_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH}]),
  wxWindow:setFont(StateTextCtrl, wxFont:new(?DEFAULT_FONT_SIZE, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)),
  ref_add(?STATE_TEXT, StateTextCtrl),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  StateSizer = wxBoxSizer:new(?wxVERTICAL),

  % Populate sizers
  wxWindow:setSizer(StatePanel, BorderSizer),
  wxSizer:add(BorderSizer, StateSizer, [{proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}, {border, 10}]),

  wxSizer:add(StateSizer, StateTextCtrl, [{proportion, 1}, {flag, ?wxEXPAND}]),

  StatePanel.


%% =====================================================================

setupRightSizer(Parent) ->
  % Right top notebook
  RightTopNotebook = wxNotebook:new(Parent, ?RIGHT_TOP_NOTEBOOK),
  ref_add(?RIGHT_TOP_NOTEBOOK, RightTopNotebook),

  ManualPanel = setupManualPanel(RightTopNotebook),
  AutoPanel = setupAutoPanel(RightTopNotebook),
  ReplayPanel = setupReplayPanel(RightTopNotebook),
  RollPanel = setupRollPanel(RightTopNotebook),

  wxNotebook:addPage(RightTopNotebook, ManualPanel, "Manual"),
  wxNotebook:addPage(RightTopNotebook, AutoPanel, "Automatic"),
  wxNotebook:addPage(RightTopNotebook, ReplayPanel, "Replay"),
  wxNotebook:addPage(RightTopNotebook, RollPanel, "Rollback"),

  % Right bottom notebook
  RightBottomNotebook = wxNotebook:new(Parent, ?RIGHT_BOTTOM_NOTEBOOK),
  ref_add(?RIGHT_BOTTOM_NOTEBOOK, RightBottomNotebook),

  TracePanel = setupTracePanel(RightBottomNotebook),
  RollLogPanel = setupRollLogPanel(RightBottomNotebook),

  wxNotebook:addPage(RightBottomNotebook, TracePanel, "Trace"),
  wxNotebook:addPage(RightBottomNotebook, RollLogPanel, "Roll Log"),

  % Sizers
  RightSizer = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(RightSizer, RightTopNotebook, [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:add(RightSizer, RightBottomNotebook, [{proportion, 1}, {flag, ?wxEXPAND}]),

  RightSizer.


%% ========== Right top sizer ========== %%

setupManualPanel(Parent) ->
  ManualPanel = wxPanel:new(Parent),

  % Pid text
  PidStaticText = wxStaticText:new(ManualPanel, ?wxID_ANY, "Pid/MsgId: "),
  PidTextCtrl = wxTextCtrl:new(ManualPanel, ?MANUAL_PID_TEXT),
  ref_add(?MANUAL_PID_TEXT, PidTextCtrl),

  % Forward buttons
  FwdIntButton = wxButton:new(ManualPanel, ?FWD_INT_BUTTON, [{label, "Seq"}]),
  wxButton:disable(FwdIntButton),
  ref_add(?FWD_INT_BUTTON, FwdIntButton),

  % Backward buttons
  BwdIntButton = wxButton:new(ManualPanel, ?BWD_INT_BUTTON, [{label, "Seq"}]),
  wxButton:disable(BwdIntButton),
  ref_add(?BWD_INT_BUTTON, BwdIntButton),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  ManualSizer = wxBoxSizer:new(?wxVERTICAL),
  PidSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ButtonSizer = wxBoxSizer:new(?wxVERTICAL),
  ForwardSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, ManualPanel, [{label, "Forward rules"}]),
  BackwardSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, ManualPanel, [{label, "Backward rules"}]),

  % Populate sizers
  wxSizer:add(BorderSizer, ManualSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),
  wxWindow:setSizer(ManualPanel, BorderSizer),

  wxSizer:add(ManualSizer, PidSizer),
  wxSizer:addSpacer(ManualSizer, 10),
  wxSizer:add(ManualSizer, ButtonSizer),

  wxSizer:add(PidSizer, PidStaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:add(PidSizer, PidTextCtrl, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

  wxSizer:add(ButtonSizer, ForwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(ButtonSizer, 5),
  wxSizer:add(ButtonSizer, BackwardSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(ForwardSizer, FwdIntButton),

  wxSizer:add(BackwardSizer, BwdIntButton),

  ManualPanel.

setupAutoPanel(Parent) ->
  AutoPanel = wxPanel:new(Parent),

  % Steps text
  StepStaticText = wxStaticText:new(AutoPanel, ?wxID_ANY, "Steps: "),
  StepTextCtrl = wxTextCtrl:new(AutoPanel, ?AUTO_STEP_TEXT),
  ref_add(?AUTO_STEP_TEXT, StepTextCtrl),

  % Forward/Backward buttons
  ForwardButton = wxButton:new(AutoPanel, ?FORWARD_BUTTON, [{label, "Forward"}]),
  BackwardButton = wxButton:new(AutoPanel, ?BACKWARD_BUTTON, [{label, "Backward"}]),

  wxButton:disable(ForwardButton),
  wxButton:disable(BackwardButton),

  ref_add(?FORWARD_BUTTON, ForwardButton),
  ref_add(?BACKWARD_BUTTON, BackwardButton),

  % Horizontal line
  HorizontalLine = wxStaticLine:new(AutoPanel, [{style, ?wxLI_HORIZONTAL}, {size, {200, -1}}]),

  % Normalize button
  NormalizeButton = wxButton:new(AutoPanel, ?NORMALIZE_BUTTON, [{label, "Normalize"}]),

  wxButton:disable(NormalizeButton),

  ref_add(?NORMALIZE_BUTTON, NormalizeButton),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  AutoSizer = wxBoxSizer:new(?wxVERTICAL),
  StepSizer = wxBoxSizer:new(?wxHORIZONTAL),
  DirectionButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),
  NormalizeButtonSizer = wxBoxSizer:new(?wxHORIZONTAL),

  % Populate sizers
  wxWindow:setSizer(AutoPanel, BorderSizer),
  wxSizer:add(BorderSizer, AutoSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),

  wxSizer:add(AutoSizer, StepSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:addSpacer(AutoSizer, 15),
  wxSizer:add(AutoSizer, DirectionButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),
  wxSizer:add(AutoSizer, HorizontalLine, [{flag, ?wxTOP bor ?wxBOTTOM}, {border, 10}]),
  wxSizer:add(AutoSizer, NormalizeButtonSizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

  wxSizer:add(StepSizer, StepStaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
  wxSizer:add(StepSizer, StepTextCtrl, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

  wxSizer:add(DirectionButtonSizer, ForwardButton),
  wxSizer:addSpacer(DirectionButtonSizer, 5),
  wxSizer:add(DirectionButtonSizer, BackwardButton),

  wxSizer:add(NormalizeButtonSizer, NormalizeButton),

  AutoPanel.

setupReplayPanel(Parent) ->
  ReplayPanel = wxPanel:new(Parent),

  TextCtrlSize = {size, {50, -1}},
  ButtonSize = {size, {100, -1}},

  % Replay pid and steps
  ReplayPidStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Pid: "),
  ReplayPidTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_PID_TEXT, [TextCtrlSize]),
  ref_add(?REPLAY_PID_TEXT, ReplayPidTextCtrl),
  ReplayStepsStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Steps: "),
  ReplayStepsTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_STEPS_TEXT, [TextCtrlSize]),
  ref_add(?REPLAY_STEPS_TEXT, ReplayStepsTextCtrl),

  ReplayButton = wxButton:new(ReplayPanel, ?REPLAY_BUTTON, [{label, "Replay"}, ButtonSize]),
  wxButton:disable(ReplayButton),
  ref_add(?REPLAY_BUTTON, ReplayButton),

  % Replay spawn
  ReplaySpawnPidStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "Pid: "),
  ReplaySpawnPidTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_SPAWN_PID_TEXT, [TextCtrlSize]),
  ref_add(?REPLAY_SPAWN_PID_TEXT, ReplaySpawnPidTextCtrl),

  ReplaySpawnButton = wxButton:new(ReplayPanel, ?REPLAY_SPAWN_BUTTON, [{label, "Replay spawn"}, ButtonSize]),
  wxButton:disable(ReplaySpawnButton),
  ref_add(?REPLAY_SPAWN_BUTTON, ReplaySpawnButton),

  % Replay send
  ReplaySendIdStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "MsgId: "),
  ReplaySendIdTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_SEND_ID_TEXT, [TextCtrlSize]),
  ref_add(?REPLAY_SEND_ID_TEXT, ReplaySendIdTextCtrl),

  ReplaySendButton = wxButton:new(ReplayPanel, ?REPLAY_SEND_BUTTON, [{label, "Replay send"}, ButtonSize]),
  wxButton:disable(ReplaySendButton),
  ref_add(?REPLAY_SEND_BUTTON, ReplaySendButton),

  % Replay receive
  ReplayRecIdStaticText = wxStaticText:new(ReplayPanel, ?wxID_ANY, "MsgId: "),
  ReplayRecIdTextCtrl = wxTextCtrl:new(ReplayPanel, ?REPLAY_REC_ID_TEXT, [TextCtrlSize]),
  ref_add(?REPLAY_REC_ID_TEXT, ReplayRecIdTextCtrl),

  ReplayRecButton = wxButton:new(ReplayPanel, ?REPLAY_REC_BUTTON, [{label, "Replay rec"}, ButtonSize]),
  wxButton:disable(ReplayRecButton),
  ref_add(?REPLAY_REC_BUTTON, ReplayRecButton),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  ReplaySizer = wxBoxSizer:new(?wxVERTICAL),
  ReplayStepsSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplaySpawnSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplaySendSizer = wxBoxSizer:new(?wxHORIZONTAL),
  ReplayRecSizer = wxBoxSizer:new(?wxHORIZONTAL),

  % Populate sizers
  wxWindow:setSizer(ReplayPanel, BorderSizer),
  wxSizer:add(BorderSizer, ReplaySizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),

  AlignRightFlag = [{flag, ?wxALIGN_RIGHT}],

  wxSizer:add(ReplaySizer, ReplayStepsSizer, AlignRightFlag),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplaySpawnSizer, AlignRightFlag),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplaySendSizer, AlignRightFlag),
  wxSizer:addSpacer(ReplaySizer, 10),
  wxSizer:add(ReplaySizer, ReplayRecSizer, AlignRightFlag),

  AlignCenterVerticalFlag = [{flag, ?wxALIGN_CENTER_VERTICAL}],

  wxSizer:add(ReplayStepsSizer, ReplayPidStaticText, AlignCenterVerticalFlag),
  wxSizer:add(ReplayStepsSizer, ReplayPidTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(ReplayStepsSizer, 10),
  wxSizer:add(ReplayStepsSizer, ReplayStepsStaticText, AlignCenterVerticalFlag),
  wxSizer:add(ReplayStepsSizer, ReplayStepsTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(ReplayStepsSizer, 5),
  wxSizer:add(ReplayStepsSizer, ReplayButton, AlignCenterVerticalFlag),

  wxSizer:add(ReplaySpawnSizer, ReplaySpawnPidStaticText, AlignCenterVerticalFlag),
  wxSizer:add(ReplaySpawnSizer, ReplaySpawnPidTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(ReplaySpawnSizer, 5),
  wxSizer:add(ReplaySpawnSizer, ReplaySpawnButton, AlignCenterVerticalFlag),

  wxSizer:add(ReplaySendSizer, ReplaySendIdStaticText, AlignCenterVerticalFlag),
  wxSizer:add(ReplaySendSizer, ReplaySendIdTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(ReplaySendSizer, 5),
  wxSizer:add(ReplaySendSizer, ReplaySendButton, AlignCenterVerticalFlag),

  wxSizer:add(ReplayRecSizer, ReplayRecIdStaticText, AlignCenterVerticalFlag),
  wxSizer:add(ReplayRecSizer, ReplayRecIdTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(ReplayRecSizer, 5),
  wxSizer:add(ReplayRecSizer, ReplayRecButton, AlignCenterVerticalFlag),

  ReplayPanel.

setupRollPanel(Parent) ->
  RollPanel = wxPanel:new(Parent),

  TextCtrlSize = {size, {50, -1}},
  ButtonSize = {size, {100, -1}},

  % Rollback pid and steps
  RollPidStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Pid: "),
  RollPidTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_PID_TEXT, [TextCtrlSize]),
  ref_add(?ROLL_PID_TEXT, RollPidTextCtrl),
  RollStepStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Steps: "),
  RollStepTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_STEP_TEXT, [TextCtrlSize]),
  ref_add(?ROLL_STEP_TEXT, RollStepTextCtrl),

  RollButton = wxButton:new(RollPanel, ?ROLL_BUTTON, [{label, "Roll"}, ButtonSize]),
  wxButton:disable(RollButton),
  ref_add(?ROLL_BUTTON, RollButton),

  % Rollback spawn
  RollSpawnIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Pid: "),
  RollSpawnIdText = wxTextCtrl:new(RollPanel, ?ROLL_SPAWN_PID_TEXT, [TextCtrlSize]),
  ref_add(?ROLL_SPAWN_PID_TEXT, RollSpawnIdText),

  RollSpawnButton = wxButton:new(RollPanel, ?ROLL_SPAWN_BUTTON, [{label, "Roll spawn"}, ButtonSize]),
  wxButton:disable(RollSpawnButton),
  ref_add(?ROLL_SPAWN_BUTTON, RollSpawnButton),

  % Rollback send
  RollSendIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId: "),
  RollSendIdText = wxTextCtrl:new(RollPanel, ?ROLL_SEND_ID_TEXT, [TextCtrlSize]),
  ref_add(?ROLL_SEND_ID_TEXT, RollSendIdText),

  RollSendButton = wxButton:new(RollPanel, ?ROLL_SEND_BUTTON, [{label, "Roll send"}, ButtonSize]),
  wxButton:disable(RollSendButton),
  ref_add(?ROLL_SEND_BUTTON, RollSendButton),

  % Rollback receive
  RollRecIdStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "MsgId: "),
  RollRecIdTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_REC_ID_TEXT, [TextCtrlSize]),
  ref_add(?ROLL_REC_ID_TEXT, RollRecIdTextCtrl),

  RollRecButton = wxButton:new(RollPanel, ?ROLL_REC_BUTTON, [{label, "Roll rec"}, ButtonSize]),
  wxButton:disable(RollRecButton),
  ref_add(?ROLL_REC_BUTTON, RollRecButton),

  % Rollback variable
  RollVarNameStaticText = wxStaticText:new(RollPanel, ?wxID_ANY, "Name: "),
  RollVarNameTextCtrl = wxTextCtrl:new(RollPanel, ?ROLL_VAR_NAME_TEXT, [{size, {80, -1}}]),
  ref_add(?ROLL_VAR_NAME_TEXT, RollVarNameTextCtrl),

  RollVarButton = wxButton:new(RollPanel, ?ROLL_VAR_BUTTON, [{label, "Roll var"}, ButtonSize]),
  wxButton:disable(RollVarButton),
  ref_add(?ROLL_VAR_BUTTON, RollVarButton),

  % Create sizers
  RollSizer = wxBoxSizer:new(?wxVERTICAL),
  RollStepsSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSpawnSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollSendSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollRecSizer = wxBoxSizer:new(?wxHORIZONTAL),
  RollVarSizer = wxBoxSizer:new(?wxHORIZONTAL),
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),

  % Populate sizers
  wxWindow:setSizer(RollPanel, BorderSizer),
  wxSizer:add(BorderSizer, RollSizer, [{flag, ?wxALL bor ?wxALIGN_CENTER_HORIZONTAL}, {border, 10}]),

  AlignRightFlag = [{flag, ?wxALIGN_RIGHT}],

  wxSizer:add(RollSizer, RollStepsSizer, AlignRightFlag),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollSpawnSizer, AlignRightFlag),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollSendSizer, AlignRightFlag),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollRecSizer, AlignRightFlag),
  wxSizer:addSpacer(RollSizer, 10),
  wxSizer:add(RollSizer, RollVarSizer, AlignRightFlag),

  AlignCenterVerticalFlag = [{flag, ?wxALIGN_CENTER_VERTICAL}],

  wxSizer:add(RollStepsSizer, RollPidStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollStepsSizer, RollPidTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollStepsSizer, 10),
  wxSizer:add(RollStepsSizer, RollStepStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollStepsSizer, RollStepTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollStepsSizer, 5),
  wxSizer:add(RollStepsSizer, RollButton, AlignCenterVerticalFlag),

  wxSizer:add(RollSpawnSizer, RollSpawnIdStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollSpawnSizer, RollSpawnIdText, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollSpawnSizer, 5),
  wxSizer:add(RollSpawnSizer, RollSpawnButton, AlignCenterVerticalFlag),

  wxSizer:add(RollSendSizer, RollSendIdStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollSendSizer, RollSendIdText, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollSendSizer, 5),
  wxSizer:add(RollSendSizer, RollSendButton, AlignCenterVerticalFlag),

  wxSizer:add(RollRecSizer, RollRecIdStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollRecSizer, RollRecIdTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollRecSizer, 5),
  wxSizer:add(RollRecSizer, RollRecButton, AlignCenterVerticalFlag),

  wxSizer:add(RollVarSizer, RollVarNameStaticText, AlignCenterVerticalFlag),
  wxSizer:add(RollVarSizer, RollVarNameTextCtrl, AlignCenterVerticalFlag),
  wxSizer:addSpacer(RollVarSizer, 5),
  wxSizer:add(RollVarSizer, RollVarButton, AlignCenterVerticalFlag),

  RollPanel.


%% ========== Right bottom sizer ========== %%

setupTracePanel(Parent) ->
  TracePanel = wxPanel:new(Parent),

  % Trace text
  TraceText = wxTextCtrl:new(TracePanel, ?TRACE_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  wxWindow:setFont(TraceText, wxFont:new(9, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)),
  ref_add(?TRACE_TEXT, TraceText),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  TraceSizer = wxBoxSizer:new(?wxVERTICAL),

  % Populate sizers
  wxWindow:setSizer(TracePanel, BorderSizer),
  wxSizer:add(BorderSizer, TraceSizer, [{proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}, {border, 10}]),

  wxSizer:add(TraceSizer, TraceText, [{proportion, 1}, {flag, ?wxEXPAND}]),

  TracePanel.

setupRollLogPanel(Parent) ->
  RollLogPanel = wxPanel:new(Parent),

  % Roll log text
  RollLogText = wxTextCtrl:new(RollLogPanel, ?ROLL_LOG_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY}]),
  wxWindow:setFont(RollLogText, wxFont:new(9, ?wxFONTFAMILY_TELETYPE, ?wxFONTSTYLE_NORMAL, ?wxFONTWEIGHT_NORMAL)),
  ref_add(?ROLL_LOG_TEXT, RollLogText),

  % Create sizers
  BorderSizer = wxBoxSizer:new(?wxVERTICAL),
  RollLogSizer = wxBoxSizer:new(?wxVERTICAL),

  % Populate sizers
  wxWindow:setSizer(RollLogPanel, BorderSizer),
  wxSizer:add(RollLogSizer, RollLogText, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxSizer:add(BorderSizer, RollLogSizer, [{proportion, 1}, {flag, ?wxALL bor ?wxEXPAND}, {border, 10}]),

  RollLogPanel.


%% =====================================================================


%% Loads the specified '.erl' source file
loadFile(File) ->
  utils_gui:stop_refs(),
  cauder:start_refs(),
  cauder:ref_add(?MODULE_PATH, filename:dirname(File)),

  {ok, Src, _} = erl_prim_loader:get_file(File),
  wxTextCtrl:setValue(ref_lookup(?CODE_TEXT), Src),

  FunNames = cauder:load_file(File),

  Status = ref_lookup(?STATUS),
  ref_add(?STATUS, Status#status{loaded = true}),
  wxMenuItem:enable(ref_lookup(?REPLAY), [{enable, true}]),

  % Open the 'Code' tab
  wxNotebook:setSelection(ref_lookup(?LEFT_NOTEBOOK), ?PAGEPOS_CODE),

  utils_gui:set_choices(FunNames),
  utils_gui:disable_all_buttons(),
  utils_gui:clear_texts(),

  wxSizer:layout(ref_lookup(?FUN_SIZER)),
  wxButton:enable(ref_lookup(?START_BUTTON)),

  utils_gui:update_status_text("Loaded file " ++ File).


-spec loadReplayData(file:filename()) -> ok.

loadReplayData(Path) ->
%%  try
  utils:load_replay_data(Path),
  #replay{log_path = Path, call = {Mod, Fun, Args}, main_pid = Pid} = get(replay_data),
  Log = utils:get_log_data(Path, Pid),
  io:format("Log: ~p\n", [Log]),
  start(Mod, Fun, Args, Pid, Log).
%%  catch
%%    _:_ ->
%%      Frame = ref_lookup(?FRAME),
%%      wxFrame:setStatusText(Frame, "Error loading replay data")
%%  end.

openDialog(Parent) ->
  Message = "Select an Erlang file",
  Wildcard = "Erlang (*.erl)|*.erl|All files (*.*)|*.*",
  Dialog = wxFileDialog:new(Parent, [{message, Message},
                                     {wildCard, Wildcard},
                                     {style, ?wxFD_OPEN bor
                                             ?wxFD_FILE_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Dir = wxFileDialog:getDirectory(Dialog),
      Path = wxFileDialog:getPath(Dialog),
      ref_add(?LAST_PATH, Dir),
      loadFile(Path);
    _Other -> continue
  end,
  wxDialog:destroy(Dialog).

openReplayDialog(Parent) ->
  Title = "Select a log folder",
  DefaultPath = ref_lookup(?LAST_PATH),
  Dialog = wxDirDialog:new(Parent, [{title, Title},
                                    {defaultPath, DefaultPath},
                                    {style, ?wxDD_DIR_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Path = wxDirDialog:getPath(Dialog),
      loadReplayData(Path);
    _Other -> continue
  end,
  wxDialog:destroy(Dialog).

zoomIn() ->
  CodeText = ref_lookup(?CODE_TEXT),
  StateText = ref_lookup(?STATE_TEXT),

  Font = wxTextCtrl:getFont(CodeText),
  FontSize = wxFont:getPointSize(Font),
  NewFontSize = utils_gui:next_font_size(FontSize),
  wxFont:setPointSize(Font, NewFontSize),

  wxTextCtrl:setFont(CodeText, Font),
  wxTextCtrl:setFont(StateText, Font),

  wxMenuItem:enable(ref_lookup(?ZOOM_IN), [{enable, utils_gui:can_zoom_in(NewFontSize)}]),
  wxMenuItem:enable(ref_lookup(?ZOOM_OUT), [{enable, utils_gui:can_zoom_out(NewFontSize)}]).

zoomOut() ->
  CodeText = ref_lookup(?CODE_TEXT),
  StateText = ref_lookup(?STATE_TEXT),

  Font = wxTextCtrl:getFont(CodeText),
  FontSize = wxFont:getPointSize(Font),
  NewFontSize = utils_gui:prev_font_size(FontSize),
  wxFont:setPointSize(Font, NewFontSize),

  wxTextCtrl:setFont(CodeText, Font),
  wxTextCtrl:setFont(StateText, Font),

  wxMenuItem:enable(ref_lookup(?ZOOM_IN), [{enable, utils_gui:can_zoom_in(NewFontSize)}]),
  wxMenuItem:enable(ref_lookup(?ZOOM_OUT), [{enable, utils_gui:can_zoom_out(NewFontSize)}]).

zoomReset() ->
  CodeText = ref_lookup(?CODE_TEXT),
  StateText = ref_lookup(?STATE_TEXT),

  Font = wxTextCtrl:getFont(CodeText),
  NewFontSize = ?DEFAULT_FONT_SIZE,
  wxFont:setPointSize(Font, NewFontSize),

  wxTextCtrl:setFont(CodeText, Font),
  wxTextCtrl:setFont(StateText, Font),

  wxMenuItem:enable(ref_lookup(?ZOOM_IN), [{enable, utils_gui:can_zoom_in(NewFontSize)}]),
  wxMenuItem:enable(ref_lookup(?ZOOM_OUT), [{enable, utils_gui:can_zoom_out(NewFontSize)}]).

%%--------------------------------------------------------------------
%% @doc Initializes the system.
%%
%% @param Fun Entry point of the system.
%% @param Args Arguments of the entry point.
%% @param Pid Pid for the new system.
%% @param Log Initial system log.
%% @end
%%--------------------------------------------------------------------
-spec init_system(Module, Function, Args, Pid, Log) -> no_return() when
  Module :: atom(),
  Function :: atom(),
  Args :: [cauder_types:abstract_expr()],
  Pid :: pos_integer(),
  Log :: list(). % TODO

init_system(M, F, As, Pid, Log) ->
  Proc = #proc{
    pid   = Pid,
    log   = Log,
    exprs = [{remote_call, 0, M, F, As}],
    spf   = {M, F, length(As)}
  },
  System = #sys{
    procs  = [Proc],
    ghosts = load_ghosts(Pid)
  },
  ref_add(?SYSTEM, System),

  % Update system status
  Status = ref_lookup(?STATUS),
  NewStatus = Status#status{running = true},
  ref_add(?STATUS, NewStatus).


%% ---------------------------------------------------------------------
%% @doc Loads the replay data for all the processes in the current replay
%% data, except for the one with the MainPid, which has already been loaded.

-spec load_ghosts(MainPid :: pos_integer()) -> [cauder_types:process()].

load_ghosts(MainPid) ->
  #replay{log_path = Path} = get(replay_data),
  {ok, Filenames} = file:list_dir(Path),
  lists:filtermap(
    fun(Filename) ->
      case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
        {match, [StrPid]} ->
          case list_to_integer(StrPid) of
            MainPid -> false;
            Pid ->
              {true, #proc{
                pid = Pid,
                log = utils:get_log_data(Path, Pid)
              }}
          end;
        nomatch -> false
      end
    end, Filenames).

%%--------------------------------------------------------------------
%% @doc Starts a new system.
%%
%% @param Fun Entry point of the system.
%% @param Args Arguments of the entry point.
%% @end
%%--------------------------------------------------------------------

-spec start(Module, Function, Args) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()].

start(M, F, As) -> start(M, F, As, 1, []).


%%--------------------------------------------------------------------
%% @doc Starts a new system.
%%
%% @param Fun Entry point function of the system.
%% @param Args Arguments of the entry point.
%% @param Pid Initial pid for the new system.
%% @param Log Initial system log.
%% @end
%%--------------------------------------------------------------------

-spec start(Module, Function, Args, Pid, Log) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()],
  Pid :: pos_integer(),
  Log :: list(). % TODO

start(M, F, As, Pid, Log) ->
  cauder:reset_fresh_refs(Pid),
  init_system(M, F, As, Pid, Log),
  refresh(true),

  % Open the 'State' tab
  wxNotebook:setSelection(ref_lookup(?LEFT_NOTEBOOK), ?PAGEPOS_STATE),

  % Update status bar message
  StatusString = "Started system with " ++ atom_to_list(F) ++ "/" ++ integer_to_list(length(As)) ++ " fun application!",
  utils_gui:update_status_text(StatusString).


refresh_buttons(Opts) ->
  PidText = wxTextCtrl:getValue(ref_lookup(?MANUAL_PID_TEXT)),
  ?LOG("full options: " ++ ?TO_STRING(utils_gui:sort_opts(Opts))),
  case string:to_integer(PidText) of
    {error, _} ->
      utils_gui:disable_button(?FWD_INT_BUTTON);
    {Pid, _} ->
      Buttons = lists:map(fun utils_gui:option_to_button_label/1, utils:filter_options(Opts, Pid)),
      utils_gui:set_button_label_from(?FWD_INT_BUTTON, Buttons)
  end,
  utils_gui:enable_button_if(?FORWARD_BUTTON, utils:has_fwd(Opts)),
  utils_gui:enable_button_if(?BACKWARD_BUTTON, utils:has_bwd(Opts)),
  utils_gui:enable_button_if(?NORMALIZE_BUTTON, utils:has_norm(Opts)).


-spec refresh(boolean()) -> ok.

refresh(RefState) ->
  case utils_gui:is_app_running() of
    false -> ok;
    true ->
      System = ref_lookup(?SYSTEM),
      Options = cauder:eval_opts(System),
      case RefState of
        false -> ok;
        true ->
          ToggleOpts = utils_gui:toggle_opts(),
          StateText = ref_lookup(?STATE_TEXT),
          TraceText = ref_lookup(?TRACE_TEXT),
          RollLogText = ref_lookup(?ROLL_LOG_TEXT),
          MarkedText = pretty_print:system(System, ToggleOpts),
          utils_gui:pp_marked_text(StateText, MarkedText),
          wxTextCtrl:setValue(TraceText, pretty_print:system_trace(System)),
          wxTextCtrl:setValue(RollLogText, pretty_print:roll_log(System))
      end,
      refresh_buttons(Options),
      utils_gui:enable_replay_buttons(), % TODO Enable only if it is possible to replay
      utils_gui:enable_roll_buttons() % TODO Enable only if it is possible to rollback
  end.

start() ->
  InputText = wxTextCtrl:getValue(ref_lookup(?ARGS_TEXT)),
  SelectedFun = wxChoice:getStringSelection(ref_lookup(?FUN_CHOICE)),
  {M, F, A} = utils:stringToMFA(SelectedFun),
  As = utils:stringToArgs(InputText),
  case A == length(As) of
    true ->
      start(M, F, As),
      ?LOG("start fun " ++ SelectedFun ++ " with args " ++ InputText);
    false ->
      utils_gui:update_status_text(?ERROR_NUM_ARGS),
      error
  end.


%% ==================== Manual evaluation ==================== %%


-spec exec_with(?FWD_INT_BUTTON | ?BWD_INT_BUTTON) -> ok.

exec_with(Button) ->
  PidText = wxTextCtrl:getValue(ref_lookup(?MANUAL_PID_TEXT)),
  case string:to_integer(PidText) of
    {error, _} -> ok;
    {Pid, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      Opt0 = utils_gui:button_to_option(Button),
      Opt1 = Opt0#opt{id = Pid},
      Sys1 = cauder:eval_step(Sys0, Opt1),
      ref_add(?SYSTEM, Sys1)
  end.


%% ==================== Automatic evaluation ==================== %%


-spec eval_mult(Button) -> error | {StepsDone, Steps} when
  Button :: ?FORWARD_BUTTON | ?BACKWARD_BUTTON,
  StepsDone :: non_neg_integer(),
  Steps :: pos_integer().

eval_mult(Button) ->
  StepText = wxTextCtrl:getValue(ref_lookup(?AUTO_STEP_TEXT)),
  case string:to_integer(StepText) of
    {error, _} -> error;
    {Steps, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      Dir =
      case Button of
        ?FORWARD_BUTTON -> ?MULT_FWD;
        ?BACKWARD_BUTTON -> ?MULT_BWD
      end,
      {Sys1, StepsDone} = cauder:eval_mult(Sys0, Dir, Steps),
      ref_add(?SYSTEM, Sys1),
      {StepsDone, Steps}
  end.


-spec eval_norm() -> StepsDone :: non_neg_integer().

eval_norm() ->
  System = ref_lookup(?SYSTEM),
  {NewSystem, StepsDone} = cauder:eval_norm(System),
  ref_add(?SYSTEM, NewSystem),
  StepsDone.


%% ==================== Replay evaluation ==================== %%


-spec eval_replay() -> {StepsDone, Steps} when
  StepsDone :: non_neg_integer(),
  Steps :: pos_integer().

eval_replay() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?REPLAY_PID_TEXT)),
  StepText = wxTextCtrl:getValue(ref_lookup(?REPLAY_STEPS_TEXT)),
  {Pid, _} = string:to_integer(PidText),
  {Steps, _} = string:to_integer(StepText),
  if
    Pid =:= error orelse Steps =:= error ->
      {0, 0};
    true ->
      Sys0 = ref_lookup(?SYSTEM),
      {Sys1, StepsDone} = cauder:eval_replay(Sys0, Pid, Steps),
      ref_add(?SYSTEM, Sys1),
      {StepsDone, Steps}
  end.


-spec eval_replay_spawn() -> {CanReplay, SpawnPid} when
  CanReplay :: boolean(),
  SpawnPid :: string() | none.

eval_replay_spawn() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?REPLAY_SPAWN_PID_TEXT)),
  case string:to_integer(PidText) of
    % What if error?
    {error, _} -> {false, none};
    {SpawnPid, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      Sys1 = cauder:eval_replay_spawn(Sys0, SpawnPid),
      ref_add(?SYSTEM, Sys1)
  end.


-spec eval_replay_send() -> {CanReplay, Id} when
  CanReplay :: boolean(),
  Id :: string() | none.

eval_replay_send() ->
  IdText = wxTextCtrl:getValue(ref_lookup(?REPLAY_SEND_ID_TEXT)),
  case string:to_integer(IdText) of
    % What if error?
    {error, _} -> {false, none};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      Sys1 = cauder:eval_replay_send(Sys0, Id),
      ref_add(?SYSTEM, Sys1)
  end.


-spec eval_replay_rec() -> {CanReplay, Id} when
  CanReplay :: boolean(),
  Id :: string() | none.

eval_replay_rec() ->
  IdText = wxTextCtrl:getValue(ref_lookup(?REPLAY_REC_ID_TEXT)),
  case string:to_integer(IdText) of
    % What if error?
    {error, _} -> {false, none};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      Sys1 = cauder:eval_replay_rec(Sys0, Id),
      ref_add(?SYSTEM, Sys1)
  end.


%% ==================== Rollback evaluation ==================== %%


-spec eval_roll() -> {FocusLog, StepsDone, Steps} when
  FocusLog :: boolean(),
  StepsDone :: non_neg_integer(),
  Steps :: pos_integer().

eval_roll() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?ROLL_PID_TEXT)),
  StepText = wxTextCtrl:getValue(ref_lookup(?ROLL_STEP_TEXT)),
  {Pid, _} = string:to_integer(PidText),
  {Steps, _} = string:to_integer(StepText),
  case {Pid, Steps} of
    {error, _} -> {false, 0, 0};
    {_, error} -> {false, 0, 0};
    _ ->
      Sys0 = ref_lookup(?SYSTEM),
      {FocusLog, Sys1, StepsDone} = cauder:eval_roll(Sys0, Pid, Steps),
      ref_add(?SYSTEM, Sys1),
      {FocusLog, StepsDone, Steps}
  end.


-spec eval_roll_spawn() -> {CanRoll, SpawnPid, FocusLog} when
  CanRoll :: boolean(),
  SpawnPid :: string() | none,
  FocusLog :: boolean().

eval_roll_spawn() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?ROLL_SPAWN_PID_TEXT)),
  case string:to_integer(PidText) of
    {error, _} -> {false, none, false};
    {Pid, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_spawn(Sys0, Pid),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, PidText, FocusLog}
  end.


-spec eval_roll_send() -> {CanRoll, Id, FocusLog} when
  CanRoll :: boolean(),
  Id :: string() | none,
  FocusLog :: boolean().

eval_roll_send() ->
  IdTextCtrl = ref_lookup(?ROLL_SEND_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case string:to_integer(IdText) of
    {error, _} -> {false, none, false};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_send(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, IdText, FocusLog}
  end.


-spec eval_roll_rec() -> {CanRoll, Id, FocusLog} when
  CanRoll :: boolean(),
  Id :: string() | none,
  FocusLog :: boolean().

eval_roll_rec() ->
  IdTextCtrl = ref_lookup(?ROLL_REC_ID_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case string:to_integer(IdText) of
    {error, _} -> {false, none, false};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_rec(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, IdText, FocusLog}
  end.


-spec eval_roll_var() -> {CanRoll, Name, FocusLog} when
  CanRoll :: boolean(),
  Name :: string() | none,
  FocusLog :: boolean().

eval_roll_var() ->
  TextCtrl = ref_lookup(?ROLL_VAR_NAME_TEXT),
  case wxTextCtrl:getValue(TextCtrl) of
    "" -> {false, none, false};
    Text ->
      System = ref_lookup(?SYSTEM),
      VarName = list_to_atom(Text),
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_var(System, VarName),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, Text, FocusLog}
  end.

focus_roll_log(false) -> ok;
focus_roll_log(true) ->
  RBotNotebook = ref_lookup(?RIGHT_BOTTOM_NOTEBOOK),
  wxNotebook:setSelection(RBotNotebook, ?PAGEPOS_ROLL).


loop() ->
  Result =
  receive
  %% ---------- File menu ---------- %%
    #wx{id = ?OPEN, event = #wxCommand{type = command_menu_selected}} -> openDialog(ref_lookup(?FRAME));
    #wx{id = ?REPLAY, event = #wxCommand{type = command_menu_selected}} -> openReplayDialog(ref_lookup(?FRAME));
    #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} -> exit;

  %% ---------- View menu ---------- %%
    #wx{id = ?ZOOM_IN, event = #wxCommand{type = command_menu_selected}} -> zoomIn();
    #wx{id = ?ZOOM_OUT, event = #wxCommand{type = command_menu_selected}} -> zoomOut();
    #wx{id = ?ZOOM_100, event = #wxCommand{type = command_menu_selected}} -> zoomReset();
    #wx{id = ?TOGGLE_MAIL, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?TOGGLE_LOG, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?TOGGLE_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?TOGGLE_STACK, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?TOGGLE_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?TOGGLE_EXP, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?RADIO_CONC_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?RADIO_FULL_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?RADIO_REL_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
    #wx{id = ?RADIO_FULL_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);

  %% ---------- Compiler menu ---------- %%
    #wx{id = ?TOGGLE_COMP, event = #wxCommand{type = command_menu_selected}} -> utils_gui:sttext_comp();

  %% ---------- Help menu ---------- %%
    #wx{id = ?ABOUT, event = #wxCommand{type = command_menu_selected}} ->
      Caption = "About " ++ ?APP_STRING,
      Dialog = wxMessageDialog:new(ref_lookup(?FRAME), ?INFO_TEXT, [{style, ?wxOK}, {caption, Caption}]),
      wxDialog:showModal(Dialog),
      wxWindow:destroy(Dialog);

  %% ---------- Start button ---------- %%
    #wx{id = ?START_BUTTON, event = #wxCommand{type = command_button_clicked}} -> start();

  %% ---------- Manual panel buttons ---------- %%
    #wx{id = Button, event = #wxCommand{type = command_button_clicked}}
      when (Button =:= ?FWD_INT_BUTTON) orelse (Button =:= ?BWD_INT_BUTTON) ->
      utils_gui:disable_all_buttons(),
      exec_with(Button),
      utils_gui:sttext_single(Button),
      refresh(true);

  %% ---------- Automatic panel buttons ---------- %%
    #wx{id = Button, event = #wxCommand{type = command_button_clicked}}
      when (Button =:= ?FORWARD_BUTTON) orelse (Button =:= ?BACKWARD_BUTTON) ->
      utils_gui:disable_all_buttons(),
      case eval_mult(Button) of
        error ->
          utils_gui:update_status_text(?ERROR_NUM_STEP);
        {StepsDone, TotalSteps} ->
          utils_gui:sttext_mult(StepsDone, TotalSteps)
      end,
      refresh(true);
    #wx{id = ?NORMALIZE_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      StepsDone = eval_norm(),
      utils_gui:sttext_norm(StepsDone),
      refresh(true);

  %% ---------- Replay panel buttons ---------- %%
    #wx{id = ?REPLAY_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {StepsDone, TotalSteps} = eval_replay(),
      utils_gui:sttext_replay(StepsDone, TotalSteps),
      refresh(true);
    #wx{id = ?REPLAY_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, SpawnPid} = eval_replay_spawn(),
      utils_gui:sttext_replay_spawn(HasRolled, SpawnPid),
      refresh(true);
    #wx{id = ?REPLAY_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, SendId} = eval_replay_send(),
      utils_gui:sttext_replay_send(HasRolled, SendId),
      refresh(true);
    #wx{id = ?REPLAY_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, RecId} = eval_replay_rec(),
      utils_gui:sttext_replay_rec(HasRolled, RecId),
      refresh(true);

  %% ---------- Rollback panel buttons ---------- %%
    #wx{id = ?ROLL_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {MustFocus, StepsDone, TotalSteps} = eval_roll(),
      utils_gui:sttext_roll(StepsDone, TotalSteps),
      focus_roll_log(MustFocus),
      refresh(true);
    #wx{id = ?ROLL_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, SpawnPid, MustFocus} = eval_roll_spawn(),
      utils_gui:sttext_roll_spawn(HasRolled, SpawnPid),
      focus_roll_log(MustFocus),
      refresh(HasRolled);
    #wx{id = ?ROLL_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, SendId, MustFocus} = eval_roll_send(),
      utils_gui:sttext_roll_send(HasRolled, SendId),
      focus_roll_log(MustFocus),
      refresh(HasRolled);
    #wx{id = ?ROLL_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, RecId, MustFocus} = eval_roll_rec(),
      utils_gui:sttext_roll_rec(HasRolled, RecId),
      focus_roll_log(MustFocus),
      refresh(HasRolled);
    #wx{id = ?ROLL_VAR_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
      utils_gui:disable_all_buttons(),
      {HasRolled, VarId, MustFocus} = eval_roll_var(),
      utils_gui:sttext_roll_var(HasRolled, VarId),
      focus_roll_log(MustFocus),
      refresh(HasRolled);

  %% ---------- Text handlers ---------- %%
    #wx{id = ?MANUAL_PID_TEXT, event = #wxCommand{type = command_text_updated}} -> refresh(false);
    #wx{id = ?AUTO_STEP_TEXT, event = #wxCommand{type = command_text_updated}} -> refresh(false);
    #wx{id = _RestIds, event = #wxCommand{type = command_text_updated}} -> ok;

  %% ---------- Other handlers ---------- %%
    #wx{event = #wxClose{type = close_window}} -> exit;

  %% ---------- Non-supported events ---------- %%
    Other -> io:format("main loop does not implement ~p~n", [Other])
  end,
  case Result of
    ok -> loop();
    exit -> wxFrame:destroy(ref_lookup(?FRAME));
    _ -> error(Result)
  end.

ref_start() ->
  ?GUI_REF = ets:new(?GUI_REF, [set, public, named_table]),
  ok.

ref_stop() ->
  true = ets:delete(?GUI_REF),
  ok.

ref_add(Id, Ref) ->
  true = ets:insert(?GUI_REF, {Id, Ref}),
  ok.

ref_lookup(Id) -> ets:lookup_element(?GUI_REF, Id, 2).
