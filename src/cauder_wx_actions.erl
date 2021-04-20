-module(cauder_wx_actions).

%% API
-export([create/1, update/2, update_process/2]).
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
%% @doc Updates the <i>actions</i> panel according to the given new state, by
%% comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update(OldState, NewState) ->
    update_manual(OldState, NewState),
    update_automatic(OldState, NewState),
    update_replay(OldState, NewState),
    update_rollback(OldState, NewState),
    ok.

%%%=============================================================================

-spec update_process(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_process(#wx_state{system = System, pid = Pid}, #wx_state{system = System, pid = Pid}) ->
    ok;
update_process(_, #wx_state{system = undefined}) ->
    Choice = cauder_wx:find(?ACTION_Process, wxChoice),
    wxChoice:disable(Choice),
    wxChoice:clear(Choice),
    ok;
update_process(#wx_state{pid = OldPid}, #wx_state{system = #sys{procs = PMap}}) ->
    Choice = cauder_wx:find(?ACTION_Process, wxChoice),
    wxChoice:freeze(Choice),
    wxChoice:enable(Choice),
    wxChoice:clear(Choice),
    {_, NewIdx} =
        lists:foldl(
            fun(Proc, {Idx, Match}) ->
                Label = cauder_pp:process(Proc),
                Pid = Proc#proc.pid,
                wxChoice:append(Choice, Label, Pid),
                case Pid of
                    OldPid -> {Idx + 1, Idx};
                    Pid -> {Idx + 1, Match}
                end
            end,
            {0, 0},
            maps:values(PMap)
        ),
    wxChoice:setSelection(Choice, NewIdx),
    wxChoice:thaw(Choice),
    ok.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the PID of the process currently selected in the "Process"
%% dropdown.

-spec selected_pid() -> Pid | undefined when
    Pid :: cauder_types:proc_id().

selected_pid() ->
    Choice = cauder_wx:find(?ACTION_Process, wxChoice),
    case wxChoice:getSelection(Choice) of
        ?wxNOT_FOUND -> undefined;
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

    Content = wxBoxSizer:new(?wxVERTICAL),
    wxBoxSizer:add(SizerH, Content, [{proportion, 1}, {flag, ?wxALIGN_CENTER}]),

    InputSize = {size, {150, -1}},

    StaticAlignRight = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {75, -1}}],
    CenterHorizontal = [{flag, ?wxALIGN_CENTER_HORIZONTAL}],
    CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

    % Steps

    Steps = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Steps, CenterHorizontal),

    StepsText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
    wxBoxSizer:add(Steps, StepsText, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

    StepsSpin = wxSpinCtrl:new(Win, [{id, ?ACTION_Manual_Steps}, {min, 1}, {max, ?MAX_STEPS}, {initial, 1}, InputSize]),
    wxBoxSizer:add(Steps, StepsSpin, [{proportion, 1}, {flag, ?wxEXPAND}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Scheduler

    Scheduler = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Scheduler, CenterHorizontal),

    SchedulerText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Sched.:", StaticAlignRight),
    wxBoxSizer:add(Scheduler, SchedulerText, CenterVertical),

    wxBoxSizer:addSpacer(Scheduler, ?SPACER_SMALL),

    SchedulerChoice = wxChoice:new(Win, ?ACTION_Manual_Scheduler, [InputSize]),
    wxBoxSizer:add(Scheduler, SchedulerChoice, CenterVertical),

    SchedulerItems =
        [
            {?SCHEDULER_Random_Name, ?SCHEDULER_Random},
            {?SCHEDULER_Manual_Name, ?SCHEDULER_Manual}
        ],

    populate_choice(SchedulerChoice, SchedulerItems),

    wxChoice:setSelection(SchedulerChoice, 0),
    wxChoice:enable(SchedulerChoice, [{enable, length(SchedulerItems) > 1}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Buttons

    Buttons = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Buttons, CenterHorizontal),

    BwdButton = wxButton:new(Win, ?ACTION_Manual_Backward_Button, [{label, "Backward"}]),
    wxBoxSizer:add(Buttons, BwdButton, CenterVertical),

    wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

    FwdButton = wxButton:new(Win, ?ACTION_Manual_Forward_Button, [{label, "Forward"}]),
    wxBoxSizer:add(Buttons, FwdButton, CenterVertical),

    Win.

-spec update_manual(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_manual(#wx_state{task = T, system = S, pid = Pid}, #wx_state{task = T, system = S, pid = Pid}) ->
    ok;
update_manual(_, #wx_state{task = Action}) when Action =/= undefined ->
    wxPanel:disable(cauder_wx:find(?ACTION_Manual, wxPanel)),
    ok;
update_manual(_, #wx_state{system = undefined}) ->
    wxPanel:disable(cauder_wx:find(?ACTION_Manual, wxPanel)),
    ok;
update_manual(_, #wx_state{pid = undefined}) ->
    wxPanel:disable(cauder_wx:find(?ACTION_Manual, wxPanel)),
    ok;
update_manual(_, #wx_state{system = System, pid = Pid}) ->
    wxPanel:enable(cauder_wx:find(?ACTION_Manual, wxPanel)),

    Options = lists:filter(fun(Opt) -> Opt#opt.pid =:= Pid end, cauder:eval_opts(System)),
    CanFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, Options),
    CanBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, Options),

    wxSpinCtrl:enable(cauder_wx:find(?ACTION_Manual_Steps, wxSpinCtrl), [{enable, CanFwd orelse CanBwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Manual_Forward_Button, wxSpinCtrl), [{enable, CanFwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Manual_Backward_Button, wxSpinCtrl), [{enable, CanBwd}]),
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

    InputSize = {size, {150, -1}},

    StaticAlignRight = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {75, -1}}],
    CenterHorizontal = [{flag, ?wxALIGN_CENTER_HORIZONTAL}],
    CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

    % Steps

    Steps = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Steps, CenterHorizontal),

    StepsText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
    wxBoxSizer:add(Steps, StepsText, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

    StepsSpin = wxSpinCtrl:new(Win, [
        {id, ?ACTION_Automatic_Steps},
        {min, 1},
        {max, ?MAX_STEPS},
        {initial, 1},
        InputSize
    ]),
    wxBoxSizer:add(Steps, StepsSpin, [{proportion, 1}, {flag, ?wxEXPAND}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Scheduler

    Scheduler = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Scheduler, CenterHorizontal),

    SchedulerText = wxStaticText:new(Win, ?wxID_ANY, "Proc. Sched.:", StaticAlignRight),
    wxBoxSizer:add(Scheduler, SchedulerText, CenterVertical),

    wxBoxSizer:addSpacer(Scheduler, ?SPACER_SMALL),

    SchedulerChoice = wxChoice:new(Win, ?ACTION_Automatic_Scheduler, [InputSize]),
    wxBoxSizer:add(Scheduler, SchedulerChoice, CenterVertical),

    SchedulerItems =
        [
            {?SCHEDULER_RoundRobin_Name, ?SCHEDULER_RoundRobin},
            {?SCHEDULER_FCFS_Name, ?SCHEDULER_FCFS}
        ],

    populate_choice(SchedulerChoice, SchedulerItems),

    wxChoice:setSelection(SchedulerChoice, 0),
    wxChoice:enable(SchedulerChoice, [{enable, length(SchedulerItems) > 1}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Buttons

    Buttons = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Buttons, CenterHorizontal),

    BwdButton = wxButton:new(Win, ?ACTION_Automatic_Backward_Button, [{label, "Backward"}]),
    wxBoxSizer:add(Buttons, BwdButton, CenterVertical),

    wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

    FwdButton = wxButton:new(Win, ?ACTION_Automatic_Forward_Button, [{label, "Forward"}]),
    wxBoxSizer:add(Buttons, FwdButton, CenterVertical),

    Win.

-spec update_automatic(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_automatic(#wx_state{task = Action, system = System}, #wx_state{task = Action, system = System}) ->
    ok;
update_automatic(_, #wx_state{task = Action}) when Action =/= undefined ->
    wxPanel:disable(cauder_wx:find(?ACTION_Automatic, wxPanel)),
    ok;
update_automatic(_, #wx_state{system = undefined}) ->
    wxPanel:disable(cauder_wx:find(?ACTION_Automatic, wxPanel)),
    ok;
update_automatic(_, #wx_state{system = System}) ->
    wxPanel:enable(cauder_wx:find(?ACTION_Automatic, wxPanel)),

    Options = cauder:eval_opts(System),
    HasFwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?FWD_SEM end, Options),
    HasBwd = lists:any(fun(Opt) -> Opt#opt.sem =:= ?BWD_SEM end, Options),

    wxSpinCtrl:enable(cauder_wx:find(?ACTION_Automatic_Steps, wxSpinCtrl), [{enable, HasFwd orelse HasBwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Automatic_Forward_Button, wxSpinCtrl), [{enable, HasFwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Automatic_Backward_Button, wxSpinCtrl), [{enable, HasBwd}]),
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
    CenterHorizontal = [{flag, ?wxALIGN_CENTER_HORIZONTAL}],
    CenterVertical = [{flag, ?wxALIGN_CENTER_VERTICAL}],

    % Steps

    Steps = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Steps, CenterHorizontal),

    StepsText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
    wxBoxSizer:add(Steps, StepsText, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

    StepsSpinner = wxSpinCtrl:new(Win, [
        {id, ?ACTION_Replay_Steps},
        {min, 1},
        {max, ?MAX_STEPS},
        {initial, 1},
        InputSize
    ]),
    wxBoxSizer:add(Steps, StepsSpinner, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

    StepsButton = wxButton:new(Win, ?ACTION_Replay_Steps_Button, [{label, "Replay steps"}, ButtonSize]),
    wxBoxSizer:add(Steps, StepsButton, CenterVertical),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Spawn

    Spawn = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Spawn, CenterHorizontal),

    SpawnText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
    wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

    wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

    SpawnChoice = wxChoice:new(Win, ?ACTION_Replay_Spawn, [InputSize]),
    wxBoxSizer:add(Spawn, SpawnChoice, CenterVertical),

    wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

    SpawnButton = wxButton:new(Win, ?ACTION_Replay_Spawn_Button, [{label, "Replay spawn"}, ButtonSize]),
    wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

    SpawnChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(SpawnButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(SpawnChoice, command_choice_selected, [{callback, SpawnChoiceCallback}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Send

    Send = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Send, CenterHorizontal),

    SendText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
    wxBoxSizer:add(Send, SendText, CenterVertical),

    wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

    SendChoice = wxChoice:new(Win, ?ACTION_Replay_Send, [InputSize]),
    wxBoxSizer:add(Send, SendChoice, CenterVertical),

    wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

    SendButton = wxButton:new(Win, ?ACTION_Replay_Send_Button, [{label, "Replay send"}, ButtonSize]),
    wxBoxSizer:add(Send, SendButton, CenterVertical),

    SendChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(SendButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(SendChoice, command_choice_selected, [{callback, SendChoiceCallback}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Receive

    Receive = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Receive, CenterHorizontal),

    ReceiveText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
    wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

    wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

    ReceiveChoice = wxChoice:new(Win, ?ACTION_Replay_Receive, [InputSize]),
    wxBoxSizer:add(Receive, ReceiveChoice, CenterVertical),

    wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

    ReceiveButton = wxButton:new(Win, ?ACTION_Replay_Receive_Button, [{label, "Replay receive"}, ButtonSize]),
    wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

    ReceiveChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(ReceiveButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(ReceiveChoice, command_choice_selected, [{callback, ReceiveChoiceCallback}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % FullLog

    FullLog = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, FullLog, CenterHorizontal),

    FullLogButton = wxButton:new(Win, ?ACTION_Replay_FullLog_Button, [{label, "Replay full log"}, ButtonSize]),
    wxBoxSizer:add(FullLog, FullLogButton, CenterVertical),

    Win.

-spec update_replay(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_replay(#wx_state{task = T, system = S, pid = Pid}, #wx_state{task = T, system = S, pid = Pid}) ->
    ok;
update_replay(_, #wx_state{task = Action}) when Action =/= undefined ->
    wxPanel:disable(cauder_wx:find(?ACTION_Replay, wxPanel)),
    ok;
update_replay(_, #wx_state{system = undefined}) ->
    wxPanel:disable(cauder_wx:find(?ACTION_Replay, wxPanel)),
    ok;
update_replay(_, #wx_state{system = #sys{logs = LMap}, pid = Pid}) ->
    case lists:all(fun(Log) -> Log =:= [] end, maps:values(LMap)) of
        true ->
            wxPanel:disable(cauder_wx:find(?ACTION_Replay, wxPanel)),
            ok;
        false ->
            wxPanel:enable(cauder_wx:find(?ACTION_Replay, wxPanel)),

            CanReplaySteps = maps:get(Pid, LMap, []) =/= [],

            wxSpinCtrl:enable(cauder_wx:find(?ACTION_Replay_Steps, wxSpinCtrl), [{enable, CanReplaySteps}]),
            wxButton:enable(cauder_wx:find(?ACTION_Replay_Steps_Button, wxButton), [{enable, CanReplaySteps}]),

            % TODO Improve to avoid unnecessary updates

            #{spawn := SpawnPids, send := SendUids, 'receive' := ReceiveUids} =
                lists:foldl(
                    fun({K, V}, Map) -> maps:update_with(K, fun(Vs) -> ordsets:add_element(V, Vs) end, Map) end,
                    #{spawn => ordsets:new(), send => ordsets:new(), 'receive' => ordsets:new()},
                    lists:flatten(maps:values(LMap))
                ),

            SpawnChoice = cauder_wx:find(?ACTION_Replay_Spawn, wxChoice),
            SendChoice = cauder_wx:find(?ACTION_Replay_Send, wxChoice),
            ReceiveChoice = cauder_wx:find(?ACTION_Replay_Receive, wxChoice),

            populate_choice(SpawnChoice, SpawnPids),
            populate_choice(SendChoice, SendUids),
            populate_choice(ReceiveChoice, ReceiveUids),

            wxChoice:enable(SpawnChoice, [{enable, not wxChoice:isEmpty(SpawnChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Replay_Spawn_Button, wxButton)),

            wxChoice:enable(SendChoice, [{enable, not wxChoice:isEmpty(SendChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Replay_Send_Button, wxButton)),

            wxChoice:enable(ReceiveChoice, [{enable, not wxChoice:isEmpty(ReceiveChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Replay_Receive_Button, wxButton)),

            ok
    end.

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

    StepsText = wxStaticText:new(Win, ?wxID_ANY, "Steps:", StaticAlignRight),
    wxBoxSizer:add(Steps, StepsText, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_SMALL),

    StepsSpinner = wxSpinCtrl:new(Win, [
        {id, ?ACTION_Rollback_Steps},
        {min, 1},
        {max, ?MAX_STEPS},
        {initial, 1},
        InputSize
    ]),
    wxBoxSizer:add(Steps, StepsSpinner, CenterVertical),

    wxBoxSizer:addSpacer(Steps, ?SPACER_MEDIUM),

    StepsButton = wxButton:new(Win, ?ACTION_Rollback_Steps_Button, [{label, "Roll steps"}, ButtonSize]),
    wxBoxSizer:add(Steps, StepsButton, CenterVertical),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Spawn

    Spawn = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Spawn),

    SpawnText = wxStaticText:new(Win, ?wxID_ANY, "PID:", StaticAlignRight),
    wxBoxSizer:add(Spawn, SpawnText, CenterVertical),

    wxBoxSizer:addSpacer(Spawn, ?SPACER_SMALL),

    SpawnChoice = wxChoice:new(Win, ?ACTION_Rollback_Spawn, [InputSize]),
    wxBoxSizer:add(Spawn, SpawnChoice, CenterVertical),

    wxBoxSizer:addSpacer(Spawn, ?SPACER_MEDIUM),

    SpawnButton = wxButton:new(Win, ?ACTION_Rollback_Spawn_Button, [{label, "Roll spawn"}, ButtonSize]),
    wxBoxSizer:add(Spawn, SpawnButton, CenterVertical),

    SpawnChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(SpawnButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(SpawnChoice, command_choice_selected, [{callback, SpawnChoiceCallback}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Send

    Send = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Send),

    SendText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
    wxBoxSizer:add(Send, SendText, CenterVertical),

    wxBoxSizer:addSpacer(Send, ?SPACER_SMALL),

    SendChoice = wxChoice:new(Win, ?ACTION_Rollback_Send, [InputSize]),
    wxBoxSizer:add(Send, SendChoice, CenterVertical),

    wxBoxSizer:addSpacer(Send, ?SPACER_MEDIUM),

    SendButton = wxButton:new(Win, ?ACTION_Rollback_Send_Button, [{label, "Roll send"}, ButtonSize]),
    wxBoxSizer:add(Send, SendButton, CenterVertical),

    SendChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(SendButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(SendChoice, command_choice_selected, [{callback, SendChoiceCallback}]),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Receive

    Receive = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Receive),

    ReceiveText = wxStaticText:new(Win, ?wxID_ANY, "Msg. Uid:", StaticAlignRight),
    wxBoxSizer:add(Receive, ReceiveText, CenterVertical),

    wxBoxSizer:addSpacer(Receive, ?SPACER_SMALL),

    ReceiveChoice = wxChoice:new(Win, ?ACTION_Rollback_Receive, [InputSize]),
    wxBoxSizer:add(Receive, ReceiveChoice, CenterVertical),

    wxBoxSizer:addSpacer(Receive, ?SPACER_MEDIUM),

    ReceiveButton = wxButton:new(Win, ?ACTION_Rollback_Receive_Button, [{label, "Roll receive"}, ButtonSize]),
    wxBoxSizer:add(Receive, ReceiveButton, CenterVertical),

    ReceiveChoiceCallback =
        fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
            wxButton:enable(ReceiveButton, [{enable, Idx =/= ?wxNOT_FOUND}])
        end,

    wxChoice:connect(ReceiveChoice, command_choice_selected, [{callback, ReceiveChoiceCallback}]),

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

-spec update_rollback(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_rollback(#wx_state{task = T, system = S, pid = Pid}, #wx_state{task = T, system = S, pid = Pid}) ->
    ok;
update_rollback(_, #wx_state{task = Action}) when Action =/= undefined ->
    wxPanel:disable(cauder_wx:find(?ACTION_Rollback, wxPanel)),
    ok;
update_rollback(_, #wx_state{system = undefined}) ->
    wxPanel:disable(cauder_wx:find(?ACTION_Rollback, wxPanel)),
    ok;
update_rollback(_, #wx_state{system = #sys{procs = PMap}, pid = Pid}) ->
    CanRollBack =
        lists:any(
            fun(#proc{hist = Hist}) -> lists:any(fun cauder_utils:is_conc_item/1, Hist) end,
            maps:values(PMap)
        ),

    case CanRollBack of
        false ->
            wxPanel:disable(cauder_wx:find(?ACTION_Rollback, wxPanel)),
            ok;
        true ->
            wxPanel:enable(cauder_wx:find(?ACTION_Rollback, wxPanel)),

            #proc{hist = Hist} = maps:get(Pid, PMap),
            CanRollbackSteps = Hist =/= [],

            wxSpinCtrl:enable(cauder_wx:find(?ACTION_Rollback_Steps, wxSpinCtrl), [{enable, CanRollbackSteps}]),
            wxButton:enable(cauder_wx:find(?ACTION_Rollback_Steps_Button, wxButton), [{enable, CanRollbackSteps}]),

            % TODO Improve to avoid unnecessary updates

            HistEntries = lists:flatmap(fun(Proc) -> Proc#proc.hist end, maps:values(PMap)),

            #{spawn := SpawnPids, send := SendUids, rec := ReceiveUids} =
                lists:foldl(
                    fun
                        ({spawn = K, _Bs, _Es, _Stk, V}, Map) ->
                            maps:update_with(K, fun(Vs) -> ordsets:add_element(V, Vs) end, Map);
                        ({K, _Bs, _Es, _Stk, #message{uid = V}}, Map) when K =:= send orelse K =:= rec ->
                            maps:update_with(K, fun(Vs) -> ordsets:add_element(V, Vs) end, Map);
                        (_, Map) ->
                            Map
                    end,
                    #{spawn => ordsets:new(), send => ordsets:new(), rec => ordsets:new()},
                    HistEntries
                ),

            SpawnChoice = cauder_wx:find(?ACTION_Rollback_Spawn, wxChoice),
            SendChoice = cauder_wx:find(?ACTION_Rollback_Send, wxChoice),
            ReceiveChoice = cauder_wx:find(?ACTION_Rollback_Receive, wxChoice),

            populate_choice(SpawnChoice, SpawnPids),
            populate_choice(SendChoice, SendUids),
            populate_choice(ReceiveChoice, ReceiveUids),

            wxChoice:enable(SpawnChoice, [{enable, not wxChoice:isEmpty(SpawnChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Rollback_Spawn_Button, wxButton)),

            wxChoice:enable(SendChoice, [{enable, not wxChoice:isEmpty(SendChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Rollback_Send_Button, wxButton)),

            wxChoice:enable(ReceiveChoice, [{enable, not wxChoice:isEmpty(ReceiveChoice)}]),
            wxButton:disable(cauder_wx:find(?ACTION_Rollback_Receive_Button, wxButton)),

            ok
    end.

%%%=============================================================================

-spec populate_choice(Choice, Items) -> ok when
    Choice :: wxChoice:wxChoice(),
    Items :: [term() | {string(), term()}].

populate_choice(Choice, Items) ->
    wxChoice:freeze(Choice),
    wxChoice:clear(Choice),
    lists:foreach(
        fun
            ({Item, ClientData}) -> wxChoice:append(Choice, Item, ClientData);
            (Item) -> wxChoice:append(Choice, io_lib:format("~p", [Item]), Item)
        end,
        Items
    ),
    wxChoice:thaw(Choice).
