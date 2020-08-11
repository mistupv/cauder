-module(cauder_wx_actions).

-export([actions_area/1]).


-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


%% ===== Actions Area ===== %%

actions_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Actions"}]),
  wxWindow:setSizer(Win, Sizer),

  % Process

  Process = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Process"}]),
  wxStaticBoxSizer:add(Sizer, Process, [{flag, ?wxEXPAND}]),

  ProcessChoice = wxChoice:new(Win, ?PROC_CHOICE, []),
  ref_add(?PROC_CHOICE, ProcessChoice),
  wxStaticBoxSizer:add(Process, ProcessChoice, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxEvtHandler:connect(ProcessChoice, command_choice_selected),

  % -----

  wxStaticBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

  % Notebook

  Notebook = wxNotebook:new(Win, ?ACTION_NOTEBOOK),
  wxStaticBoxSizer:add(Sizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxNotebook:addPage(Notebook, manual_actions(Notebook), "Manual"),
  wxNotebook:addPage(Notebook, automatic_actions(Notebook), "Automatic"),
  wxNotebook:addPage(Notebook, replay_actions(Notebook), "Replay"),
  wxNotebook:addPage(Notebook, rollback_actions(Notebook), "Rollback"),

  Win.


manual_actions(Parent) ->
  Win = wxPanel:new(Parent),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  SizerV = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, SizerV, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  ExpandCenterHorizontal = [{flag, ?wxEXPAND bor ?wxALIGN_CENTER_HORIZONTAL}],
  CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

  % ----- Buttons -----

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(SizerV, Buttons, ExpandCenterHorizontal),

  % Backward

  BwdTitle = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Backward"}]),
  wxBoxSizer:add(Buttons, BwdTitle, ExpandCenterHorizontal),

  BwdButton = wxButton:new(Win, ?SINGLE_BACKWARD_BUTTON, [{label, "Seq"}]),
  ref_add(?SINGLE_BACKWARD_BUTTON, BwdButton),
  wxBoxSizer:add(BwdTitle, BwdButton),

  % -----

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  % Forward

  FwdTitle = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Forward"}]),
  wxBoxSizer:add(Buttons, FwdTitle, ExpandCenterHorizontal),

  FwdButton = wxButton:new(Win, ?SINGLE_FORWARD_BUTTON, [{label, "Seq"}]),
  ref_add(?SINGLE_FORWARD_BUTTON, FwdButton),
  wxBoxSizer:add(FwdTitle, FwdButton),

  Win.


automatic_actions(Parent) ->
  Win = wxPanel:new(Parent),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, Content, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  ExpandCenterHorizontal = [{flag, ?wxEXPAND bor ?wxALIGN_CENTER_HORIZONTAL}],
  CenterVertical = [{flag, ?wxALIGN_CENTER}],

  % Steps

  Steps = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Steps, ExpandCenterHorizontal),

  StepsStaticText = wxStaticText:new(Win, ?wxID_ANY, "Steps:"),
  wxBoxSizer:add(Steps, StepsStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

  StepsSpin = wxSpinCtrl:new(Win, [{id, ?STEPS_SPIN}, {min, 1}, {max, 100}, {initial, 1}]),
  ref_add(?STEPS_SPIN, StepsSpin),
  wxBoxSizer:add(Steps, StepsSpin, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALIGN_CENTER}]),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Buttons

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Buttons, ExpandCenterHorizontal),

  BwdButton = wxButton:new(Win, ?MULTIPLE_BACKWARD_BUTTON, [{label, "Backward"}]),
  ref_add(?MULTIPLE_BACKWARD_BUTTON, BwdButton),
  wxBoxSizer:add(Buttons, BwdButton, CenterVertical),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  FwdButton = wxButton:new(Win, ?MULTIPLE_FORWARD_BUTTON, [{label, "Forward"}]),
  ref_add(?MULTIPLE_FORWARD_BUTTON, FwdButton),
  wxBoxSizer:add(Buttons, FwdButton, CenterVertical),

  Win.


replay_actions(Parent) ->
  Win = wxPanel:new(Parent),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, Content, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  InputSize = {size, {100, -1}},
  ButtonSize = {size, {100, -1}},

  StaticAlignRight = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {60, -1}}],
  CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

  % Steps

  Steps = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Steps),

  StepsStaticText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
  wxBoxSizer:add(Steps, StepsStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

  StepsText = wxSpinCtrl:new(Win, [{id, ?REPLAY_STEPS_SPIN}, {min, 1}, {max, 100}, {initial, 1}, InputSize]),
  ref_add(?REPLAY_STEPS_SPIN, StepsText),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?REPLAY_STEPS_BUTTON, [{label, "Replay steps"}, ButtonSize]),
  ref_add(?REPLAY_STEPS_BUTTON, StepsButton),
  wxBoxSizer:add(Steps, StepsButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Spawn

  Spawn = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Spawn),

  SpawnStaticText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
  wxBoxSizer:add(Spawn, SpawnStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

  SpawnText = wxTextCtrl:new(Win, ?REPLAY_SPAWN_TEXT, [InputSize]),
  ref_add(?REPLAY_SPAWN_TEXT, SpawnText),
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?REPLAY_SPAWN_BUTTON, [{label, "Replay spawn"}, ButtonSize]),
  ref_add(?REPLAY_SPAWN_BUTTON, SpawnButton),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. ID:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?REPLAY_SEND_TEXT, [InputSize]),
  ref_add(?REPLAY_SEND_TEXT, SendText),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?REPLAY_SEND_BUTTON, [{label, "Replay send"}, ButtonSize]),
  ref_add(?REPLAY_SEND_BUTTON, SendButton),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. ID:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?REPLAY_REC_TEXT, [InputSize]),
  ref_add(?REPLAY_REC_TEXT, ReceiveText),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?REPLAY_REC_BUTTON, [{label, "Replay receive"}, ButtonSize]),
  ref_add(?REPLAY_REC_BUTTON, ReceiveButton),
  wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

  Win.


rollback_actions(Parent) ->
  Win = wxPanel:new(Parent),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, Content, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  InputSize = {size, {100, -1}},
  ButtonSize = {size, {100, -1}},

  StaticAlignRight = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {60, -1}}],
  CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

  % Steps

  Steps = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Steps),

  StepsStaticText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
  wxBoxSizer:add(Steps, StepsStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

  StepsText = wxSpinCtrl:new(Win, [{id, ?ROLL_STEPS_SPIN}, {min, 1}, {max, 100}, {initial, 1}, InputSize]),
  ref_add(?ROLL_STEPS_SPIN, StepsText),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?ROLL_STEPS_BUTTON, [{label, "Roll steps"}, ButtonSize]),
  ref_add(?ROLL_STEPS_BUTTON, StepsButton),
  wxBoxSizer:add(Steps, StepsButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Spawn

  Spawn = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Spawn),

  SpawnStaticText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
  wxBoxSizer:add(Spawn, SpawnStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

  SpawnText = wxTextCtrl:new(Win, ?ROLL_SPAWN_TEXT, [InputSize]),
  ref_add(?ROLL_SPAWN_TEXT, SpawnText),
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?ROLL_SPAWN_BUTTON, [{label, "Roll spawn"}, ButtonSize]),
  ref_add(?ROLL_SPAWN_BUTTON, SpawnButton),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. ID:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?ROLL_SEND_TEXT, [InputSize]),
  ref_add(?ROLL_SEND_TEXT, SendText),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?ROLL_SEND_BUTTON, [{label, "Roll send"}, ButtonSize]),
  ref_add(?ROLL_SEND_BUTTON, SendButton),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. ID:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?ROLL_REC_TEXT, [InputSize]),
  ref_add(?ROLL_REC_TEXT, ReceiveText),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?ROLL_REC_BUTTON, [{label, "Roll receive"}, ButtonSize]),
  ref_add(?ROLL_REC_BUTTON, ReceiveButton),
  wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Variable

  Variable = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Variable),

  VariableStaticText = wxStaticText:new(Win, ?wxID_ANY, "Var. Name:", StaticAlignRight),
  wxBoxSizer:add(Variable, VariableStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Variable, ?SPACER_SMALL),

  VariableText = wxTextCtrl:new(Win, ?ROLL_VAR_TEXT, [InputSize]),
  ref_add(?ROLL_VAR_TEXT, VariableText),
  wxBoxSizer:add(Variable, VariableText, CenterVertical),

  wxBoxSizer:addSpacer(Variable, ?SPACER_MEDIUM),

  VariableButton = wxButton:new(Win, ?ROLL_VAR_BUTTON, [{label, "Roll variable"}, ButtonSize]),
  ref_add(?ROLL_VAR_BUTTON, VariableButton),
  wxBoxSizer:add(Variable, VariableButton, CenterVertical),

  Win.


%% ===== Utils ===== %%


ref_add(Id, Ref) -> cauder_gui:ref_add(Id, Ref).
ref_lookup(Id) -> cauder_gui:ref_lookup(Id).
