-module(cauder_wx_actions).

%% API
-export([create/1, update/2, update_process/2]).
-export([selected_pid/0]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_process.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-define(INPUT_SIZE, {size, {100, -1}}).
-define(BUTTON_SIZE, {size, {100, -1}}).

-define(ENABLE_BUTTON_CALLBACK(Button), fun(#wx{event = #wxCommand{commandInt = Idx}}, _) ->
    wxButton:enable(Button, [{enable, Idx =/= ?wxNOT_FOUND}])
end).

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
update_process(#wx_state{pid = OldPid}, #wx_state{system = #system{pool = Pool}}) ->
    Choice = cauder_wx:find(?ACTION_Process, wxChoice),
    wxChoice:freeze(Choice),
    wxChoice:enable(Choice),
    wxChoice:clear(Choice),
    {_, NewIdx} =
        lists:foldl(
            fun(P, {Idx, Match}) ->
                Label = cauder_pp:process(P),
                Pid = P#process.pid,
                wxChoice:append(Choice, Label, Pid),
                case Pid of
                    OldPid -> {Idx + 1, Idx};
                    Pid -> {Idx + 1, Match}
                end
            end,
            {0, 0},
            cauder_pool:to_list(Pool)
        ),
    wxChoice:setSelection(Choice, NewIdx),
    wxChoice:thaw(Choice),
    ok.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the PID of the process currently selected in the "Process"
%% dropdown.

-spec selected_pid() -> Pid | undefined when
    Pid :: cauder_process:id().

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

    OptsFwd = cauder_semantics_forwards:options(System, normal),
    OptsBwd = cauder_semantics_backwards:options(System),
    CanFwd = maps:is_key(Pid, OptsFwd),
    CanBwd = maps:is_key(Pid, OptsBwd),

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

    OptFwd = cauder_semantics_forwards:options(System, normal),
    OptBwd = cauder_semantics_backwards:options(System),
    CanFwd = maps:size(OptFwd) =/= 0,
    CanBwd = maps:size(OptBwd) =/= 0,

    wxSpinCtrl:enable(cauder_wx:find(?ACTION_Automatic_Steps, wxSpinCtrl), [{enable, CanFwd orelse CanBwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Automatic_Forward_Button, wxButton), [{enable, CanFwd}]),
    wxButton:enable(cauder_wx:find(?ACTION_Automatic_Backward_Button, wxButton), [{enable, CanBwd}]),
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

    % -----

    create_spinner(Win, Content, "Steps:", ?ACTION_Replay_Steps, {"Replay steps", ?ACTION_Replay_Steps_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Uid:", ?ACTION_Replay_Send, {"Replay send", ?ACTION_Replay_Send_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    %create_choice(Win, Content, "Uid:", ?ACTION_Replay_Deliver, {"Replay deliver", ?ACTION_Replay_Deliver_Button}),
    %wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Uid:", ?ACTION_Replay_Receive, {"Replay receive", ?ACTION_Replay_Receive_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Node:", ?ACTION_Replay_Start, {"Replay start", ?ACTION_Replay_Start_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Pid:", ?ACTION_Replay_Spawn, {"Replay spawn", ?ACTION_Replay_Spawn_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

    % Replay Full Log

    FullLog = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, FullLog, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

    FullLogButton = wxButton:new(Win, ?ACTION_Replay_FullLog_Button, [{label, "Replay full log"}, {size, {100, -1}}]),
    wxBoxSizer:add(FullLog, FullLogButton, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    Win.

-spec update_replay(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_replay(#wx_state{task = T, system = S, pid = Pid}, #wx_state{task = T, system = S, pid = Pid}) ->
    ok;
update_replay(_, #wx_state{task = Action}) when Action =/= undefined ->
    disable_replay();
update_replay(_, #wx_state{system = undefined}) ->
    disable_replay();
update_replay(_, #wx_state{system = #system{log = Log}, pid = Pid}) ->
    case cauder_log:is_empty(Log) of
        true ->
            disable_replay();
        false ->
            wxPanel:enable(cauder_wx:find(?ACTION_Replay, wxPanel)),

            CanReplaySteps = cauder_log:is_element(Pid, Log),

            wxSpinCtrl:enable(cauder_wx:find(?ACTION_Replay_Steps, wxSpinCtrl), [{enable, CanReplaySteps}]),
            wxButton:enable(cauder_wx:find(?ACTION_Replay_Steps_Button, wxButton), [{enable, CanReplaySteps}]),

            % TODO Improve to avoid unnecessary updates

            #{
                'send' := SendUids,
                'receive' := ReceiveUids,
                'start' := StartNodes,
                'spawn' := SpawnPids
            } = cauder_log:group_actions(Log),

            update_choice(?ACTION_Replay_Send, ?ACTION_Replay_Send_Button, SendUids),
            %update_choice(?ACTION_Replay_Deliver, ?ACTION_Replay_Deliver_Button, DeliverUids),
            update_choice(?ACTION_Replay_Receive, ?ACTION_Replay_Receive_Button, ReceiveUids),
            update_choice(?ACTION_Replay_Start, ?ACTION_Replay_Start_Button, StartNodes),
            update_choice(?ACTION_Replay_Spawn, ?ACTION_Replay_Spawn_Button, SpawnPids),

            ok
    end.

-spec disable_replay() -> ok.

disable_replay() ->
    wxPanel:disable(cauder_wx:find(?ACTION_Replay, wxPanel)),
    update_choice(?ACTION_Replay_Send, ?ACTION_Replay_Send_Button, []),
    %update_choice(?ACTION_Replay_Deliver, ?ACTION_Replay_Deliver_Button, []),
    update_choice(?ACTION_Replay_Receive, ?ACTION_Replay_Receive_Button, []),
    update_choice(?ACTION_Replay_Start, ?ACTION_Replay_Start_Button, []),
    update_choice(?ACTION_Replay_Spawn, ?ACTION_Replay_Spawn_Button, []),
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

    % -----

    create_spinner(Win, Content, "Steps:", ?ACTION_Rollback_Steps, {"Roll steps", ?ACTION_Rollback_Steps_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Uid:", ?ACTION_Rollback_Send, {"Roll send", ?ACTION_Rollback_Send_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    %create_choice(
    %    Win, Content, "Atom-Uid:", ?ACTION_Rollback_Senda, {"Roll send with atom", ?ACTION_Rollback_Senda_Button}
    %),
    %wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    %create_choice(Win, Content, "Uid:", ?ACTION_Rollback_Deliver, {"Roll deliver", ?ACTION_Rollback_Deliver_Button}),
    %wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Uid:", ?ACTION_Rollback_Receive, {"Roll receive", ?ACTION_Rollback_Receive_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Node:", ?ACTION_Rollback_Start, {"Roll start", ?ACTION_Rollback_Start_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(Win, Content, "Pid:", ?ACTION_Rollback_Spawn, {"Roll spawn", ?ACTION_Rollback_Spawn_Button}),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_text(Win, Content, "Name:", ?ACTION_Rollback_Variable, {"Roll variable", ?ACTION_Rollback_Variable_Button}),

    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(
        Win, Content, "Map Element:", ?ACTION_Rollback_Register, {"Roll register", ?ACTION_Rollback_Register_Button}
    ),
    wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),
    create_choice(
        Win, Content, "Map Element:", ?ACTION_Rollback_Delete, {"Roll delete", ?ACTION_Rollback_Delete_Button}
    ),

    Win.

-spec update_rollback(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_rollback(#wx_state{task = T, system = S, pid = Pid}, #wx_state{task = T, system = S, pid = Pid}) ->
    ok;
update_rollback(_, #wx_state{task = Action}) when Action =/= undefined ->
    disable_rollback();
update_rollback(_, #wx_state{system = undefined}) ->
    disable_rollback();
update_rollback(_, #wx_state{system = #system{pool = Pool}, pid = Pid}) ->
    CanRollBack =
        lists:any(
            fun(#process{hist = Hist}) -> lists:any(fun cauder_history:is_concurrent/1, Hist) end,
            cauder_pool:to_list(Pool)
        ),

    case CanRollBack of
        false ->
            disable_rollback();
        true ->
            wxPanel:enable(cauder_wx:find(?ACTION_Rollback, wxPanel)),

            #process{hist = Hist} = cauder_pool:get(Pid, Pool),
            CanRollbackSteps = not cauder_history:is_empty(Hist),

            wxSpinCtrl:enable(cauder_wx:find(?ACTION_Rollback_Steps, wxSpinCtrl), [{enable, CanRollbackSteps}]),
            wxButton:enable(cauder_wx:find(?ACTION_Rollback_Steps_Button, wxButton), [{enable, CanRollbackSteps}]),

            % TODO Improve to avoid unnecessary updates

            #{
                'send' := SendUids,
                'receive' := ReceiveUids,
                'start' := StartNodes,
                'spawn' := SpawnPids,
                'register' := RegEls,
                %'sendA' := AtomsUids,
                'del' := DelEls
            } = lists:foldl(
                fun(P, Map0) ->
                    maps:fold(
                        fun(Key, Set0, Map1) ->
                            maps:update_with(
                                Key,
                                fun(Set1) -> ordsets:union(Set0, Set1) end,
                                Set0,
                                Map1
                            )
                        end,
                        Map0,
                        cauder_history:group_actions(P#process.hist)
                    )
                end,
                maps:new(),
                cauder_pool:to_list(Pool)
            ),

            update_choice(?ACTION_Rollback_Send, ?ACTION_Rollback_Send_Button, SendUids),
            %update_choice(?ACTION_Rollback_Deliver, ?ACTION_Rollback_Deliver_Button, DeliverUids),
            update_choice(?ACTION_Rollback_Receive, ?ACTION_Rollback_Receive_Button, ReceiveUids),
            update_choice(?ACTION_Rollback_Start, ?ACTION_Rollback_Start_Button, StartNodes),
            update_choice(?ACTION_Rollback_Spawn, ?ACTION_Rollback_Spawn_Button, SpawnPids),

            %update_choice(?ACTION_Rollback_Senda, ?ACTION_Rollback_Senda_Button, AtomsUids),
            ElsReg = updates(RegEls),
            ElsDel = updates(DelEls),
            update_choice(?ACTION_Rollback_Register, ?ACTION_Rollback_Receive_Button, ElsReg),
            update_choice(?ACTION_Rollback_Delete, ?ACTION_Rollback_Delete_Button, ElsDel),

            ok
    end.

-spec disable_rollback() -> ok.

disable_rollback() ->
    wxPanel:disable(cauder_wx:find(?ACTION_Rollback, wxPanel)),
    update_choice(?ACTION_Rollback_Send, ?ACTION_Rollback_Send_Button, []),
    %update_choice(?ACTION_Rollback_Deliver, ?ACTION_Rollback_Deliver_Button, []),
    update_choice(?ACTION_Rollback_Receive, ?ACTION_Rollback_Receive_Button, []),
    update_choice(?ACTION_Rollback_Start, ?ACTION_Rollback_Start_Button, []),
    update_choice(?ACTION_Rollback_Spawn, ?ACTION_Rollback_Spawn_Button, []),

    %update_choice(?ACTION_Rollback_Senda, ?ACTION_Rollback_Senda_Button, []),
    update_choice(?ACTION_Rollback_Register, ?ACTION_Rollback_Register_Button, []),
    update_choice(?ACTION_Rollback_Delete, ?ACTION_Rollback_Delete_Button, []),

    ok.

%%%=============================================================================

-spec create_choice(Window, Sizer, Label, ChoiceId, {ButtonLabel, ButtonId}) -> ok when
    Window :: wxWindow:wxWindow(),
    Sizer :: wxSizer:wxSizer(),
    Label :: string(),
    ChoiceId :: integer(),
    ButtonLabel :: string(),
    ButtonId :: integer().

create_choice(ParentWin, ParentSizer, Label, ChoiceId, {ButtonLabel, ButtonId}) ->
    StaticTextFlags = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {60, -1}}],

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(ParentSizer, Sizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

    StaticText = wxStaticText:new(ParentWin, ?wxID_ANY, Label, StaticTextFlags),
    wxBoxSizer:add(Sizer, StaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_SMALL),

    Choice = wxChoice:new(ParentWin, ChoiceId, [?INPUT_SIZE]),
    wxBoxSizer:add(Sizer, Choice, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

    Button = wxButton:new(ParentWin, ButtonId, [{label, ButtonLabel}, ?BUTTON_SIZE]),
    wxBoxSizer:add(Sizer, Button, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxChoice:connect(Choice, command_choice_selected, [{callback, ?ENABLE_BUTTON_CALLBACK(Button)}]).

-spec create_spinner(Window, Sizer, Label, SpinnerId, {ButtonLabel, ButtonId}) -> ok when
    Window :: wxWindow:wxWindow(),
    Sizer :: wxSizer:wxSizer(),
    Label :: string(),
    SpinnerId :: integer(),
    ButtonLabel :: string(),
    ButtonId :: integer().

create_spinner(ParentWin, ParentSizer, Label, SpinnerId, {ButtonLabel, ButtonId}) ->
    StaticTextFlags = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {60, -1}}],

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(ParentSizer, Sizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

    StaticText = wxStaticText:new(ParentWin, ?wxID_ANY, Label, StaticTextFlags),
    wxBoxSizer:add(Sizer, StaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_SMALL),

    Spinner = wxSpinCtrl:new(ParentWin, [
        {id, SpinnerId},
        {min, 1},
        {max, ?MAX_STEPS},
        {initial, 1},
        ?INPUT_SIZE
    ]),
    wxBoxSizer:add(Sizer, Spinner, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

    Button = wxButton:new(ParentWin, ButtonId, [{label, ButtonLabel}, ?BUTTON_SIZE]),
    wxBoxSizer:add(Sizer, Button, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    ok.

-spec create_text(Window, Sizer, Label, TextId, {ButtonLabel, ButtonId}) -> ok when
    Window :: wxWindow:wxWindow(),
    Sizer :: wxSizer:wxSizer(),
    Label :: string(),
    TextId :: integer(),
    ButtonLabel :: string(),
    ButtonId :: integer().

create_text(ParentWin, ParentSizer, Label, TextId, {ButtonLabel, ButtonId}) ->
    StaticTextFlags = [{style, ?wxALIGN_RIGHT bor ?wxST_NO_AUTORESIZE}, {size, {60, -1}}],

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(ParentSizer, Sizer, [{flag, ?wxALIGN_CENTER_HORIZONTAL}]),

    StaticText = wxStaticText:new(ParentWin, ?wxID_ANY, Label, StaticTextFlags),
    wxBoxSizer:add(Sizer, StaticText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_SMALL),

    Text = wxTextCtrl:new(ParentWin, TextId, [?INPUT_SIZE]),
    wxBoxSizer:add(Sizer, Text, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

    Button = wxButton:new(ParentWin, ButtonId, [{label, ButtonLabel}, ?BUTTON_SIZE]),
    wxBoxSizer:add(Sizer, Button, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    ok.

-spec update_choice(ChoiceId, ButtonId, Items) -> ok when
    ChoiceId :: integer(),
    ButtonId :: integer(),
    Items :: [term()].

update_choice(ChoiceId, ButtonId, Items) ->
    Choice = cauder_wx:find(ChoiceId, wxChoice),
    populate_choice(Choice, Items),
    wxChoice:enable(Choice, [{enable, not wxChoice:isEmpty(Choice)}]),
    wxButton:disable(cauder_wx:find(ButtonId, wxButton)),
    ok.

-spec populate_choice(Choice, Items) -> ok when
    Choice :: wxChoice:wxChoice(),
    Items :: [term() | {string(), term()}].

populate_choice(Choice, Items) ->
    wxChoice:freeze(Choice),
    wxChoice:clear(Choice),
    lists:foreach(
        fun
            %ONLY FOR SEND WITH ATOM
            ({{A, _, _}, Item}) -> wxChoice:append(Choice, io_lib:format("~p", [{A, Item}]), {A, Item});
            ({Item, ClientData}) -> wxChoice:append(Choice, Item, ClientData);
            (Item) -> wxChoice:append(Choice, io_lib:format("~p", [Item]), Item)
        end,
        Items
    ),
    wxChoice:thaw(Choice).

updates([])->[];
updates([{A,B,C,_} | L]) -> [{A,B,C} | updates(L)].
