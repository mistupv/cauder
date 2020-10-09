-module(cauder_wx_actions).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/1]).
-export([selected_pid/0]).


-spec create(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create(Frame) ->
  Win = wxPanel:new(Frame),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Actions"}]),
  wxWindow:setSizer(Win, Sizer),

  % Process

  Process = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Process"}]),
  wxStaticBoxSizer:add(Sizer, Process, [{flag, ?wxEXPAND}]),

  ProcessChoice = wxChoice:new(Win, ?PROC_CHOICE, []),
  wxStaticBoxSizer:add(Process, ProcessChoice, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxEvtHandler:connect(ProcessChoice, command_choice_selected),

  % -----

  wxStaticBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

  % Notebook

  Notebook = wxNotebook:new(Win, ?wxID_ANY),
  wxStaticBoxSizer:add(Sizer, Notebook, [{proportion, 1}, {flag, ?wxEXPAND}]),

  wxNotebook:addPage(Notebook, create_manual(Notebook), "Manual"),
  wxNotebook:addPage(Notebook, create_automatic(Notebook), "Automatic"),
  wxNotebook:addPage(Notebook, create_replay(Notebook), "Replay"),
  wxNotebook:addPage(Notebook, create_rollback(Notebook), "Rollback"),

  Win.


%%--------------------------------------------------------------------
%% @doc Updates the process selector and the action panels

-spec update(System :: cauder_types:system() | 'undefined') -> ok.

update(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  % Disable and clear process selector
  Choice = utils_gui:find(?PROC_CHOICE, wxChoice),
  wxChoice:disable(Choice),
  wxChoice:clear(Choice),

  % Disable all actions
  wxPanel:disable(utils_gui:find(?ACTION_Manual, wxPanel)),
  wxPanel:disable(utils_gui:find(?ACTION_Automatic, wxPanel)),
  wxPanel:disable(utils_gui:find(?ACTION_Replay, wxPanel)),
  wxPanel:disable(utils_gui:find(?ACTION_Rollback, wxPanel)),

  ok;

update(#sys{procs = ProcDict, logs = Logs} = System) ->
  Choice = utils_gui:find(?PROC_CHOICE, wxChoice),
  {_, Procs} = lists:unzip(orddict:to_list(ProcDict)),

  % Enable and populate process selector
  case get({?MODULE, system}) of
    System -> ok;
    _ ->
      wxChoice:freeze(Choice),
      wxChoice:enable(Choice),
      PrevSel = wxChoice:getStringSelection(Choice),
      wxChoice:clear(Choice),
      lists:foreach(
        fun(Proc) ->
          Label = pretty_print:process(Proc),
          wxChoice:append(Choice, Label, Proc#proc.pid)
        end,
        Procs
      ),
      wxChoice:setStringSelection(Choice, PrevSel),
      case wxChoice:getSelection(Choice) of
        ?wxNOT_FOUND -> wxChoice:setSelection(Choice, 0);
        _ -> ok
      end,
      wxChoice:thaw(Choice),

      put({?MODULE, system}, System)
  end,

  Options = cauder:eval_opts(System),
  Pid = selected_pid(),

  % Enable/disable manual actions
  wxPanel:enable(utils_gui:find(?ACTION_Manual, wxPanel)),

  case Pid of
    none ->
      wxButton:disable(utils_gui:find(?STEP_FORWARD_BUTTON, wxButton)),
      wxButton:disable(utils_gui:find(?STEP_BACKWARD_BUTTON, wxButton)),
      wxButton:disable(utils_gui:find(?STEP_OVER_FORWARD_BUTTON, wxButton)),
      wxButton:disable(utils_gui:find(?STEP_OVER_BACKWARD_BUTTON, wxButton)),
      wxButton:disable(utils_gui:find(?STEP_INTO_FORWARD_BUTTON, wxButton)),
      wxButton:disable(utils_gui:find(?STEP_INTO_BACKWARD_BUTTON, wxButton));
    _ ->
      ProcOpts = lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, Options),
      CanGoFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, ProcOpts),
      CanGoBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, ProcOpts),

      wxButton:enable(utils_gui:find(?STEP_FORWARD_BUTTON, wxButton), [{enable, CanGoFwd}]),
      wxButton:enable(utils_gui:find(?STEP_BACKWARD_BUTTON, wxButton), [{enable, CanGoBwd}]),
      wxButton:enable(utils_gui:find(?STEP_OVER_FORWARD_BUTTON, wxButton), [{enable, CanGoFwd}]),
      wxButton:enable(utils_gui:find(?STEP_OVER_BACKWARD_BUTTON, wxButton), [{enable, CanGoBwd}]),
      wxButton:enable(utils_gui:find(?STEP_INTO_FORWARD_BUTTON, wxButton), [{enable, CanGoFwd}]),
      wxButton:enable(utils_gui:find(?STEP_INTO_BACKWARD_BUTTON, wxButton), [{enable, CanGoBwd}])
  end,

  % Enable/disable automatic actions
  wxPanel:enable(utils_gui:find(?ACTION_Automatic, wxPanel)),

  HasFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, Options),
  HasBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, Options),

  wxSpinCtrl:enable(utils_gui:find(?STEPS_SPIN, wxSpinCtrl), [{enable, HasFwd orelse HasBwd}]),
  wxButton:enable(utils_gui:find(?MULTIPLE_FORWARD_BUTTON, wxSpinCtrl), [{enable, HasFwd}]),
  wxButton:enable(utils_gui:find(?MULTIPLE_BACKWARD_BUTTON, wxSpinCtrl), [{enable, HasBwd}]),

  % Enable/disable replay actions
  case Pid =:= none orelse lists:all(fun(Log) -> Log =:= [] end, orddict:to_list(Logs)) of
    true ->
      wxPanel:disable(utils_gui:find(?ACTION_Replay, wxPanel));
    false ->
      wxPanel:enable(utils_gui:find(?ACTION_Replay, wxPanel)),

      CanReplaySteps =
        case orddict:find(Pid, Logs) of
          {ok, Log} -> length(Log) > 0;
          error -> false
        end,

      wxSpinCtrl:enable(utils_gui:find(?REPLAY_STEPS_SPIN, wxSpinCtrl), [{enable, CanReplaySteps}]),
      wxButton:enable(utils_gui:find(?REPLAY_STEPS_BUTTON, wxButton), [{enable, CanReplaySteps}])
  end,

  % Enable/disable rollback actions
  CanRollBack = lists:any(
    fun(#proc{hist = Hist}) ->
      lists:any(
        fun(Entry) ->
          Key = element(1, Entry),
          Key =/= tau andalso Key =/= self
        end, Hist)
    end, Procs),
  case Pid =:= none orelse not CanRollBack of
    true ->
      wxPanel:disable(utils_gui:find(?ACTION_Rollback, wxPanel));
    false ->
      wxPanel:enable(utils_gui:find(?ACTION_Rollback, wxPanel)),

      #proc{hist = Hist} = lists:nth(wxChoice:getSelection(Choice) + 1, Procs),
      CanRollbackSteps = length(Hist) > 0,

      wxSpinCtrl:enable(utils_gui:find(?ROLL_STEPS_SPIN, wxSpinCtrl), [{enable, CanRollbackSteps}]),
      wxButton:enable(utils_gui:find(?ROLL_STEPS_BUTTON, wxButton), [{enable, CanRollbackSteps}])
  end,

  ok.


create_manual(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Manual}]),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  SizerV = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, SizerV, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  ExpandCenterHorizontal = [{flag, ?wxEXPAND bor ?wxALIGN_CENTER_HORIZONTAL}],

  % ----- Buttons -----

  Buttons = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerV, Buttons, ExpandCenterHorizontal),

  % Step

  Step = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Step"}]),
  wxBoxSizer:add(Buttons, Step, ExpandCenterHorizontal),

  StepBwd = wxButton:new(Win, ?STEP_BACKWARD_BUTTON, [{label, "Backward"}]),
  wxBoxSizer:add(Step, StepBwd),

  wxBoxSizer:addSpacer(Step, ?SPACER_SMALL),

  StepFwd = wxButton:new(Win, ?STEP_FORWARD_BUTTON, [{label, "Forward"}]),
  wxBoxSizer:add(Step, StepFwd),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  % Step Over

  StepOver = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Step Over"}]),
  wxBoxSizer:add(Buttons, StepOver, ExpandCenterHorizontal),

  StepOverBwd = wxButton:new(Win, ?STEP_OVER_BACKWARD_BUTTON, [{label, "Backward"}]),
  wxBoxSizer:add(StepOver, StepOverBwd),

  wxBoxSizer:addSpacer(StepOver, ?SPACER_SMALL),

  StepOverFwd = wxButton:new(Win, ?STEP_OVER_FORWARD_BUTTON, [{label, "Forward"}]),
  wxBoxSizer:add(StepOver, StepOverFwd),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  % Step Into

  StepInto = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Step Into"}]),
  wxBoxSizer:add(Buttons, StepInto, ExpandCenterHorizontal),

  StepIntoBwd = wxButton:new(Win, ?STEP_INTO_BACKWARD_BUTTON, [{label, "Backward"}]),
  wxBoxSizer:add(StepInto, StepIntoBwd),

  wxBoxSizer:addSpacer(StepInto, ?SPACER_SMALL),

  StepIntoFwd = wxButton:new(Win, ?STEP_INTO_FORWARD_BUTTON, [{label, "Forward"}]),
  wxBoxSizer:add(StepInto, StepIntoFwd),

  Win.


create_automatic(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Automatic}]),

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

  StepsSpin = wxSpinCtrl:new(Win, [{id, ?STEPS_SPIN}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}]),
  wxBoxSizer:add(Steps, StepsSpin, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALIGN_CENTER}]),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Buttons

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Buttons, ExpandCenterHorizontal),

  BwdButton = wxButton:new(Win, ?MULTIPLE_BACKWARD_BUTTON, [{label, "Backward"}]),
  wxBoxSizer:add(Buttons, BwdButton, CenterVertical),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  FwdButton = wxButton:new(Win, ?MULTIPLE_FORWARD_BUTTON, [{label, "Forward"}]),
  wxBoxSizer:add(Buttons, FwdButton, CenterVertical),

  Win.


create_replay(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Replay}]),

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

  StepsText = wxSpinCtrl:new(Win, [{id, ?REPLAY_STEPS_SPIN}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}, InputSize]),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?REPLAY_STEPS_BUTTON, [{label, "Replay steps"}, ButtonSize]),
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
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?REPLAY_SPAWN_BUTTON, [{label, "Replay spawn"}, ButtonSize]),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. UID:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?REPLAY_SEND_TEXT, [InputSize]),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?REPLAY_SEND_BUTTON, [{label, "Replay send"}, ButtonSize]),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. UID:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?REPLAY_REC_TEXT, [InputSize]),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?REPLAY_REC_BUTTON, [{label, "Replay receive"}, ButtonSize]),
  wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

  Win.


create_rollback(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Rollback}]),

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

  StepsText = wxSpinCtrl:new(Win, [{id, ?ROLL_STEPS_SPIN}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}, InputSize]),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?ROLL_STEPS_BUTTON, [{label, "Roll steps"}, ButtonSize]),
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
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?ROLL_SPAWN_BUTTON, [{label, "Roll spawn"}, ButtonSize]),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. UID:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?ROLL_SEND_TEXT, [InputSize]),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?ROLL_SEND_BUTTON, [{label, "Roll send"}, ButtonSize]),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. UID:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?ROLL_REC_TEXT, [InputSize]),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?ROLL_REC_BUTTON, [{label, "Roll receive"}, ButtonSize]),
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
  wxBoxSizer:add(Variable, VariableText, CenterVertical),

  wxBoxSizer:addSpacer(Variable, ?SPACER_MEDIUM),

  VariableButton = wxButton:new(Win, ?ROLL_VAR_BUTTON, [{label, "Roll variable"}, ButtonSize]),
  wxBoxSizer:add(Variable, VariableButton, CenterVertical),

  Win.


%% ===================================================================


-spec selected_pid() -> cauder_types:proc_id() | none.

selected_pid() ->
  Choice = utils_gui:find(?PROC_CHOICE, wxChoice),
  case wxChoice:getSelection(Choice) of
    ?wxNOT_FOUND -> none;
    Idx -> wxChoice:getClientData(Choice, Idx)
  end.
