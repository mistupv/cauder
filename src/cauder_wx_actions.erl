-module(cauder_wx_actions).

%% API
-export([create/1, update/1]).
-export([selected_pid/0]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates the <i>actions</i> panel and populates it.

-spec create(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create(Frame) ->
  Win = wxPanel:new(Frame),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Actions"}]),
  wxWindow:setSizer(Win, Sizer),

  % Process

  Process = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Process"}]),
  wxStaticBoxSizer:add(Sizer, Process, [{flag, ?wxEXPAND}]),

  ProcessChoice = wxChoice:new(Win, ?ACTION_Process, []),
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


%%------------------------------------------------------------------------------
%% @doc Updates the <i>actions</i> panel according to the given system.

-spec update(System) -> ok when
  System :: cauder_types:system() | undefined.

update(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  % Disable and clear process selector
  Choice = cauder_wx_utils:find(?ACTION_Process, wxChoice),
  wxChoice:disable(Choice),
  wxChoice:clear(Choice),

  % Update actions
  update_manual(System),
  update_automatic(System),
  update_replay(System),
  update_rollback(System),

  ok;

update(#sys{procs = PDict} = System) ->
  Choice = cauder_wx_utils:find(?ACTION_Process, wxChoice),
  {_, Procs} = lists:unzip(orddict:to_list(PDict)),

  % Enable and populate process selector
  wxChoice:freeze(Choice),
  wxChoice:enable(Choice),
  PrevPid = selected_pid(),
  wxChoice:clear(Choice),
  {_, NewIdx} =
    lists:foldl(
      fun(Proc, {Idx, Match}) ->
        Label = cauder_pp:process(Proc),
        Pid = Proc#proc.pid,
        wxChoice:append(Choice, Label, Pid),
        case PrevPid =:= Pid of
          true -> {Idx + 1, Idx};
          false -> {Idx + 1, Match}
        end
      end,
      {0, 0},
      Procs
    ),
  wxChoice:setSelection(Choice, NewIdx),
  wxChoice:thaw(Choice),

  % Update actions
  update_manual(System),
  update_automatic(System),
  update_replay(System),
  update_rollback(System),

  ok.


%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the PID of the process currently selected in the "Process"
%% dropdown.

-spec selected_pid() -> Pid | none when
  Pid :: cauder_types:proc_id().

selected_pid() ->
  Choice = cauder_wx_utils:find(?ACTION_Process, wxChoice),
  case wxChoice:getSelection(Choice) of
    ?wxNOT_FOUND -> none;
    Idx -> wxChoice:getClientData(Choice, Idx)
  end.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec create_manual(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_manual(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Manual}]),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  SizerV = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, SizerV, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  % ----- Buttons -----

  Buttons = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerV, Buttons, [{flag, ?wxEXPAND}]),

  % Step

  Step = wxPanel:new(Win, [{winid, ?ACTION_Manual_Step}]),
  wxBoxSizer:add(Buttons, Step),

  StepSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Step, [{label, "Step"}]),
  wxPanel:setSizer(Step, StepSizer),

  StepBwd = wxButton:new(Step, ?ACTION_Manual_Step_Backward_Button, [{label, "Backward"}]),
  wxBoxSizer:add(StepSizer, StepBwd),

  wxBoxSizer:addSpacer(StepSizer, ?SPACER_SMALL),

  StepFwd = wxButton:new(Step, ?ACTION_Manual_Step_Forward_Button, [{label, "Forward"}]),
  wxBoxSizer:add(StepSizer, StepFwd),

  % -----

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  % Step Over

  StepOver = wxPanel:new(Win, [{winid, ?ACTION_Manual_StepOver}]),
  wxBoxSizer:add(Buttons, StepOver),

  StepOverSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, StepOver, [{label, "Step Over"}]),
  wxPanel:setSizer(StepOver, StepOverSizer),

  StepOverBwd = wxButton:new(StepOver, ?ACTION_Manual_StepOver_Backward_Button, [{label, "Backward"}]),
  wxBoxSizer:add(StepOverSizer, StepOverBwd),

  wxBoxSizer:addSpacer(StepOverSizer, ?SPACER_SMALL),

  StepOverFwd = wxButton:new(StepOver, ?ACTION_Manual_StepOver_Forward_Button, [{label, "Forward"}]),
  wxBoxSizer:add(StepOverSizer, StepOverFwd),

  wxPanel:hide(StepOver), % Temporarily hidden

  % -----

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  % Step Into

  StepInto = wxPanel:new(Win, [{winid, ?ACTION_Manual_StepInto}]),
  wxBoxSizer:add(Buttons, StepInto),

  StepIntoSizer = wxStaticBoxSizer:new(?wxHORIZONTAL, StepInto, [{label, "Step Into"}]),
  wxPanel:setSizer(StepInto, StepIntoSizer),

  StepIntoBwd = wxButton:new(StepInto, ?ACTION_Manual_StepInto_Backward_Button, [{label, "Backward"}]),
  wxBoxSizer:add(StepIntoSizer, StepIntoBwd),

  wxBoxSizer:addSpacer(StepIntoSizer, ?SPACER_SMALL),

  StepIntoFwd = wxButton:new(StepInto, ?ACTION_Manual_StepInto_Forward_Button, [{label, "Forward"}]),
  wxBoxSizer:add(StepIntoSizer, StepIntoFwd),

  wxPanel:hide(StepInto), % Temporarily hidden

  Win.


-spec update_manual(System) -> ok when
  System :: cauder_types:system() | undefined.

update_manual(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  wxPanel:disable(cauder_wx_utils:find(?ACTION_Manual, wxPanel)),
  ok;

update_manual(System) ->
  wxPanel:enable(cauder_wx_utils:find(?ACTION_Manual, wxPanel)),

  case selected_pid() of
    none ->
      wxPanel:disable(cauder_wx_utils:find(?ACTION_Manual_Step, wxPanel));
    Pid ->
      ProcOpts = lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, cauder:eval_opts(System)),
      CanStep = ProcOpts =/= [],

      wxPanel:enable(cauder_wx_utils:find(?ACTION_Manual_Step, wxPanel), [{enable, CanStep}]),

      case CanStep of
        false -> ok;
        true ->
          CanStepFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, ProcOpts),
          CanStepBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, ProcOpts),

          wxButton:enable(cauder_wx_utils:find(?ACTION_Manual_Step_Forward_Button, wxButton), [{enable, CanStepFwd}]),
          wxButton:enable(cauder_wx_utils:find(?ACTION_Manual_Step_Backward_Button, wxButton), [{enable, CanStepBwd}])
      end
  end,
  ok.


%%%=============================================================================


-spec create_automatic(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_automatic(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?ACTION_Automatic}]),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Win, Border),

  SizerH = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Border, SizerH, [{proportion, 1}, {flag, ?wxALL bor ?wxALIGN_CENTER}, {border, ?SPACER_LARGE}]),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(SizerH, Content, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

  CenterVertical = [{flag, ?wxALIGN_CENTER}],

  % Steps

  Steps = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Steps, [{flag, ?wxEXPAND}]),

  StepsStaticText = wxStaticText:new(Win, ?wxID_ANY, "Steps:"),
  wxBoxSizer:add(Steps, StepsStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

  StepsSpin = wxSpinCtrl:new(Win, [{id, ?ACTION_Automatic_Steps}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}]),
  wxBoxSizer:add(Steps, StepsSpin, [{proportion, 1}, {flag, ?wxEXPAND}]),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Buttons

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Buttons, [{flag, ?wxEXPAND}]),

  BwdButton = wxButton:new(Win, ?ACTION_Automatic_Backward_Button, [{label, "Backward"}]),
  wxBoxSizer:add(Buttons, BwdButton, CenterVertical),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  FwdButton = wxButton:new(Win, ?ACTION_Automatic_Forward_Button, [{label, "Forward"}]),
  wxBoxSizer:add(Buttons, FwdButton, CenterVertical),

  Win.


-spec update_automatic(System) -> ok when
  System :: cauder_types:system() | undefined.

update_automatic(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  wxPanel:disable(cauder_wx_utils:find(?ACTION_Automatic, wxPanel)),
  ok;
update_automatic(System) ->
  wxPanel:enable(cauder_wx_utils:find(?ACTION_Automatic, wxPanel)),

  Options = cauder:eval_opts(System),
  HasFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, Options),
  HasBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, Options),

  wxSpinCtrl:enable(cauder_wx_utils:find(?ACTION_Automatic_Steps, wxSpinCtrl), [{enable, HasFwd orelse HasBwd}]),
  wxButton:enable(cauder_wx_utils:find(?ACTION_Automatic_Forward_Button, wxSpinCtrl), [{enable, HasFwd}]),
  wxButton:enable(cauder_wx_utils:find(?ACTION_Automatic_Backward_Button, wxSpinCtrl), [{enable, HasBwd}]),
  ok.


%%%=============================================================================


-spec create_replay(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

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

  StepsText = wxSpinCtrl:new(Win, [{id, ?ACTION_Replay_Steps}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}, InputSize]),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?ACTION_Replay_Steps_Button, [{label, "Replay steps"}, ButtonSize]),
  wxBoxSizer:add(Steps, StepsButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Spawn

  Spawn = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Spawn),

  SpawnStaticText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
  wxBoxSizer:add(Spawn, SpawnStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

  SpawnText = wxTextCtrl:new(Win, ?ACTION_Replay_Spawn, [InputSize]),
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?ACTION_Replay_Spawn_Button, [{label, "Replay spawn"}, ButtonSize]),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?ACTION_Replay_Send, [InputSize]),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?ACTION_Replay_Send_Button, [{label, "Replay send"}, ButtonSize]),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?ACTION_Replay_Receive, [InputSize]),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?ACTION_Replay_Receive_Button, [{label, "Replay receive"}, ButtonSize]),
  wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

  Win.


-spec update_replay(System) -> ok when
  System :: cauder_types:system() | undefined.

update_replay(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  wxPanel:disable(cauder_wx_utils:find(?ACTION_Replay, wxPanel)),
  ok;
update_replay(#sys{logs = Logs}) ->
  Pid = selected_pid(),
  case Pid =:= none orelse lists:all(fun(Log) -> Log =:= [] end, orddict:to_list(Logs)) of
    true ->
      wxPanel:disable(cauder_wx_utils:find(?ACTION_Replay, wxPanel));
    false ->
      wxPanel:enable(cauder_wx_utils:find(?ACTION_Replay, wxPanel)),

      CanReplaySteps =
        case orddict:find(Pid, Logs) of
          {ok, Log} -> length(Log) > 0;
          error -> false
        end,

      wxSpinCtrl:enable(cauder_wx_utils:find(?ACTION_Replay_Steps, wxSpinCtrl), [{enable, CanReplaySteps}]),
      wxButton:enable(cauder_wx_utils:find(?ACTION_Replay_Steps_Button, wxButton), [{enable, CanReplaySteps}])
  end,
  ok.


%%%=============================================================================


-spec create_rollback(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

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

  StepsText = wxSpinCtrl:new(Win, [{id, ?ACTION_Rollback_Steps}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}, InputSize]),
  wxBoxSizer:add(Steps, StepsText, CenterVertical),

  wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

  StepsButton = wxButton:new(Win, ?ACTION_Rollback_Steps_Button, [{label, "Roll steps"}, ButtonSize]),
  wxBoxSizer:add(Steps, StepsButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Spawn

  Spawn = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Spawn),

  SpawnStaticText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
  wxBoxSizer:add(Spawn, SpawnStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

  SpawnText = wxTextCtrl:new(Win, ?ACTION_Rollback_Spawn, [InputSize]),
  wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

  wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

  SpawnButton = wxButton:new(Win, ?ACTION_Rollback_Spawn_Button, [{label, "Roll spawn"}, ButtonSize]),
  wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Send

  Send = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Send),

  SendStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
  wxBoxSizer:add(Send, SendStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

  SendText = wxTextCtrl:new(Win, ?ACTION_Rollback_Send, [InputSize]),
  wxBoxSizer:add(Send, SendText, CenterVertical),

  wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

  SendButton = wxButton:new(Win, ?ACTION_Rollback_Send_Button, [{label, "Roll send"}, ButtonSize]),
  wxBoxSizer:add(Send, SendButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Receive

  Receive = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Receive),

  ReceiveStaticText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
  wxBoxSizer:add(Receive, ReceiveStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

  ReceiveText = wxTextCtrl:new(Win, ?ACTION_Rollback_Receive, [InputSize]),
  wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

  wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

  ReceiveButton = wxButton:new(Win, ?ACTION_Rollback_Receive_Button, [{label, "Roll receive"}, ButtonSize]),
  wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

  % -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  % Variable

  Variable = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Variable),

  VariableStaticText = wxStaticText:new(Win, ?wxID_ANY, "Var. Name:", StaticAlignRight),
  wxBoxSizer:add(Variable, VariableStaticText, CenterVertical),

  wxBoxSizer:addSpacer(Variable, ?SPACER_SMALL),

  VariableText = wxTextCtrl:new(Win, ?ACTION_Rollback_Variable, [InputSize]),
  wxBoxSizer:add(Variable, VariableText, CenterVertical),

  wxBoxSizer:addSpacer(Variable, ?SPACER_MEDIUM),

  VariableButton = wxButton:new(Win, ?ACTION_Rollback_Variable_Button, [{label, "Roll variable"}, ButtonSize]),
  wxBoxSizer:add(Variable, VariableButton, CenterVertical),

  Win.


-spec update_rollback(System) -> ok when
  System :: cauder_types:system() | undefined.

update_rollback(System) when System =:= undefined orelse System#sys.procs =:= [] ->
  wxPanel:disable(cauder_wx_utils:find(?ACTION_Rollback, wxPanel)),
  ok;
update_rollback(#sys{procs = PDict}) ->
  {_, Procs} = lists:unzip(orddict:to_list(PDict)),
  Pid = selected_pid(),

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
      wxPanel:disable(cauder_wx_utils:find(?ACTION_Rollback, wxPanel));
    false ->
      wxPanel:enable(cauder_wx_utils:find(?ACTION_Rollback, wxPanel)),

      Choice = cauder_wx_utils:find(?ACTION_Process, wxChoice),
      #proc{hist = Hist} = lists:nth(wxChoice:getSelection(Choice) + 1, Procs),
      CanRollbackSteps = length(Hist) > 0,

      wxSpinCtrl:enable(cauder_wx_utils:find(?ACTION_Rollback_Steps, wxSpinCtrl), [{enable, CanRollbackSteps}]),
      wxButton:enable(cauder_wx_utils:find(?ACTION_Rollback_Steps_Button, wxButton), [{enable, CanRollbackSteps}])
  end,
  ok.
