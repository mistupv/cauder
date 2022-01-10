-module(cauder_wx_system).

%% API
-export([create/1, update/2, update_trace_process/2]).
-export([selected_trace_pid/0]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates the <i>system info</i> panel and populates it.

-spec create(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create(Parent) ->
    Win = wxPanel:new(Parent, [{winid, ?SYSTEM_Panel}]),

    Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "System Info"}]),
    wxWindow:setSizer(Win, Sizer),

    Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

    NotebookNodesAndMail = wxNotebook:new(Win, ?SYSTEM_Notebook_NodesAndMail),
    wxNotebook:addPage(NotebookNodesAndMail, create_nodes(NotebookNodesAndMail), "Nodes"),
    wxNotebook:addPage(NotebookNodesAndMail, create_mail(NotebookNodesAndMail), "Mailbox"),
    wxSizer:add(Sizer, NotebookNodesAndMail, Expand),

    wxSizer:addSpacer(Sizer, 5),

    NotebookTraceAndRollLog = wxNotebook:new(Win, ?SYSTEM_Notebook_TraceAndRollLog),
    wxNotebook:addPage(NotebookTraceAndRollLog, create_trace(NotebookTraceAndRollLog), "Trace"),
    wxNotebook:addPage(NotebookTraceAndRollLog, create_roll_log(NotebookTraceAndRollLog), "Roll Log"),
    wxSizer:add(Sizer, NotebookTraceAndRollLog, Expand),

    Win.

%%------------------------------------------------------------------------------
%% @doc Updates the <i>system info</i> panel according to the given new state,
%% by comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update(OldState, NewState) ->
    update_nodes(OldState, NewState),
    update_mail(OldState, NewState),
    update_trace(OldState, NewState),
    update_roll_log(OldState, NewState).

%%%=============================================================================

-spec update_trace_process(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_trace_process(
    #wx_state{system = #system{trace = Trace}, trace_pid = Pid},
    #wx_state{system = #system{trace = Trace}, trace_pid = Pid}
) ->
    ok;
update_trace_process(_, #wx_state{system = undefined}) ->
    Choice = cauder_wx:find(?SYSTEM_Trace_Process, wxChoice),
    wxChoice:disable(Choice),
    wxChoice:clear(Choice),
    ok;
update_trace_process(_, #wx_state{system = #system{trace = Trace}}) when map_size(Trace) =:= 0 ->
    Choice = cauder_wx:find(?SYSTEM_Trace_Process, wxChoice),
    wxChoice:disable(Choice),
    wxChoice:clear(Choice),
    ok;
update_trace_process(#wx_state{trace_pid = OldPid}, #wx_state{system = #system{pool = PMap, trace = Trace}}) ->
    Choice = cauder_wx:find(?SYSTEM_Trace_Process, wxChoice),
    wxChoice:freeze(Choice),
    wxChoice:enable(Choice),
    wxChoice:clear(Choice),
    {_, NewIdx} =
        lists:foldl(
            fun(Proc, {Idx, Match}) ->
                Label = cauder_pp:process(Proc, [{icon, false}, {node, auto}, {pid, true}, {mfa, true}]),
                Pid = Proc#process.pid,
                wxChoice:append(Choice, Label, Pid),
                case Pid of
                    OldPid -> {Idx + 1, Idx};
                    Pid -> {Idx + 1, Match}
                end
            end,
            {0, 0},
            maps:values(maps:with(maps:keys(Trace), PMap))
        ),
    wxChoice:setSelection(Choice, NewIdx),
    wxChoice:thaw(Choice),
    ok.

%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the PID of the process currently selected in the "Process"
%% dropdown in the trace area.

-spec selected_trace_pid() -> Pid | undefined when
    Pid :: cauder_types:proc_id().

selected_trace_pid() ->
    Choice = cauder_wx:find(?SYSTEM_Trace_Process, wxChoice),
    case wxChoice:getSelection(Choice) of
        ?wxNOT_FOUND -> undefined;
        Idx -> wxChoice:getClientData(Choice, Idx)
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec create_mail(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create_mail(Parent) ->
    Win = wxPanel:new(Parent),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(Win, Sizer),

    MailArea = wxListCtrl:new(Win, [{winid, ?SYSTEM_Mail}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
    wxBoxSizer:add(Sizer, MailArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

    Item = wxListItem:new(),
    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

    wxListItem:setText(Item, "UID"),
    wxListItem:setFont(Item, Font),
    wxListCtrl:insertColumn(MailArea, 0, Item),

    wxListItem:setText(Item, "Value"),
    wxListItem:setFont(Item, Font),
    wxListCtrl:insertColumn(MailArea, 1, Item),

    wxListItem:setText(Item, "Src."),
    wxListItem:setFont(Item, Font),
    wxListCtrl:insertColumn(MailArea, 2, Item),

    wxListItem:setText(Item, "Dest."),
    wxListItem:setFont(Item, Font),
    wxListCtrl:insertColumn(MailArea, 3, Item),

    wxListItem:destroy(Item),

    wxListCtrl:setColumnWidth(MailArea, 0, 50),
    wxListCtrl:setColumnWidth(MailArea, 1, 150),
    wxListCtrl:setColumnWidth(MailArea, 2, 50),
    wxListCtrl:setColumnWidth(MailArea, 3, 50),

    wxListCtrl:connect(MailArea, command_list_item_activated),

    Win.

-spec create_nodes(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create_nodes(Parent) ->
    Win = wxPanel:new(Parent),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(Win, Sizer),

    NodesArea = wxTextCtrl:new(Win, ?SYSTEM_Nodes, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
    wxBoxSizer:add(Sizer, NodesArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    wxTextCtrl:setFont(NodesArea, Font),

    Win.

-spec update_nodes(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_nodes(#wx_state{system = #system{nodes = Nodes}}, #wx_state{system = #system{nodes = Nodes}}) ->
    ok;
update_nodes(_, #wx_state{system = undefined}) ->
    ok;
update_nodes(_, #wx_state{system = #system{nodes = Nodes}}) ->
    wxNotebook:setSelection(cauder_wx:find(?SYSTEM_Notebook_NodesAndMail, wxNotebook), ?SYSTEM_Notebook_Tab_Nodes),
    NodesArea = cauder_wx:find(?SYSTEM_Nodes, wxTextCtrl),
    wxTextCtrl:freeze(NodesArea),
    wxTextCtrl:clear(NodesArea),

    [FirstNode | RemNodes] = lists:map(fun(Node) -> atom_to_list(Node) end, Nodes),
    NodesListFormatted = "[" ++ lists:foldl(fun(Node, AccIn) -> Node ++ ", " ++ AccIn end, FirstNode, RemNodes) ++ "]",

    wxTextCtrl:appendText(NodesArea, NodesListFormatted),
    wxTextCtrl:thaw(NodesArea).

-spec update_mail(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_mail(
    #wx_state{system = #system{mail = Mail}, config = #config{mailbox = Show, mailbox_mode = all}},
    #wx_state{system = #system{mail = Mail}, config = #config{mailbox = Show, mailbox_mode = all}}
) ->
    ok;
update_mail(
    #wx_state{system = #system{mail = Mail}, pid = Pid, config = #config{mailbox = Show, mailbox_mode = process}},
    #wx_state{system = #system{mail = Mail}, pid = Pid, config = #config{mailbox = Show, mailbox_mode = process}}
) ->
    ok;
update_mail(_, #wx_state{system = undefined}) ->
    wxListCtrl:deleteAllItems(cauder_wx:find(?SYSTEM_Mail, wxListCtrl)),
    ok;
update_mail(_, #wx_state{system = #system{mail = Mail}, config = #config{mailbox_mode = all}}) ->
    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    MailArea = cauder_wx:find(?SYSTEM_Mail, wxListCtrl),
    wxListCtrl:freeze(MailArea),
    wxListCtrl:deleteAllItems(MailArea),
    lists:foldl(
        fun(#message{uid = Uid, val = Value, src = Src, dst = Dest}, Row) ->
            wxListCtrl:insertItem(MailArea, Row, ""),
            wxListCtrl:setItemFont(MailArea, Row, Font),
            wxListCtrl:setItem(MailArea, Row, 0, cauder_pp:to_string(Uid)),
            wxListCtrl:setItem(MailArea, Row, 1, cauder_pp:to_string(Value)),
            wxListCtrl:setItem(MailArea, Row, 2, cauder_pp:to_string(Src)),
            wxListCtrl:setItem(MailArea, Row, 3, cauder_pp:to_string(Dest)),
            Row + 1
        end,
        0,
        cauder_mailbox:to_list(Mail)
    ),
    wxListCtrl:thaw(MailArea),
    ok;
update_mail(_, #wx_state{system = #system{mail = Mail}, pid = Pid, config = #config{mailbox_mode = process}}) ->
    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    MailArea = cauder_wx:find(?SYSTEM_Mail, wxListCtrl),
    wxListCtrl:freeze(MailArea),
    wxListCtrl:deleteAllItems(MailArea),
    case Pid of
        undefined ->
            ok;
        Pid ->
            Messages = lists:flatmap(fun queue:to_list/1, cauder_mailbox:pid_get(Pid, Mail)),
            lists:foldl(
                fun(#message{uid = Uid, val = Value, src = Src, dst = Dest}, Row) ->
                    wxListCtrl:insertItem(MailArea, Row, ""),
                    wxListCtrl:setItemFont(MailArea, Row, Font),
                    wxListCtrl:setItem(MailArea, Row, 0, cauder_pp:to_string(Uid)),
                    wxListCtrl:setItem(MailArea, Row, 1, cauder_pp:to_string(Value)),
                    wxListCtrl:setItem(MailArea, Row, 2, cauder_pp:to_string(Src)),
                    wxListCtrl:setItem(MailArea, Row, 3, cauder_pp:to_string(Dest)),
                    Row + 1
                end,
                0,
                Messages
            )
    end,
    wxListCtrl:thaw(MailArea),
    ok.

%%%=============================================================================

-spec create_trace(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create_trace(Parent) ->
    Win = wxPanel:new(Parent),

    Border = wxBoxSizer:new(?wxVERTICAL),
    wxWindow:setSizer(Win, Border),

    Content = wxBoxSizer:new(?wxVERTICAL),
    wxBoxSizer:add(Border, Content, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    % Process

    Process = wxBoxSizer:new(?wxHORIZONTAL),
    wxBoxSizer:add(Content, Process, [{flag, ?wxEXPAND}]),

    ProcessText = wxStaticText:new(Win, ?wxID_ANY, "Process:"),
    wxBoxSizer:add(Process, ProcessText, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

    wxBoxSizer:addSpacer(Process, ?SPACER_SMALL),

    ProcessChoice = wxChoice:new(Win, ?SYSTEM_Trace_Process),
    wxStaticBoxSizer:add(Process, ProcessChoice, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]),

    wxEvtHandler:connect(ProcessChoice, command_choice_selected),

    % -----

    wxBoxSizer:addSpacer(Content, ?SPACER_MEDIUM),

    % Trace

    TraceArea = wxListBox:new(Win, ?SYSTEM_Trace_Content),
    wxBoxSizer:add(Content, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}]),

    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    wxListBox:setFont(TraceArea, Font),

    Win.

-spec update_trace(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_trace(
    #wx_state{system = #system{trace = Trace}, trace_pid = Pid},
    #wx_state{system = #system{trace = Trace}, trace_pid = Pid}
) ->
    ok;
update_trace(_, #wx_state{system = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Trace_Content, wxListBox)),
    ok;
update_trace(_, #wx_state{trace_pid = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Trace_Content, wxListBox)),
    ok;
update_trace(_, #wx_state{system = #system{trace = Trace}, trace_pid = Pid}) ->
    wxNotebook:setSelection(cauder_wx:find(?SYSTEM_Notebook_TraceAndRollLog, wxNotebook), ?SYSTEM_Notebook_Tab_Trace),

    TraceControl = cauder_wx:find(?SYSTEM_Trace_Content, wxListBox),
    wxListBox:freeze(TraceControl),
    wxListBox:clear(TraceControl),
    case Trace of
        #{Pid := Actions} ->
            Entries = lists:map(fun lists:flatten/1, lists:map(fun cauder_pp:trace_action/1, Actions)),
            lists:foreach(fun(Entry) -> wxListBox:append(TraceControl, Entry) end, Entries);
        _ ->
            ok
    end,
    wxListBox:thaw(TraceControl).

%%%=============================================================================

-spec create_roll_log(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create_roll_log(Parent) ->
    Win = wxPanel:new(Parent),

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(Win, Sizer),

    RollLogArea = wxListBox:new(Win, ?SYSTEM_RollLog),
    wxBoxSizer:add(Sizer, RollLogArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    wxListBox:setFont(RollLogArea, Font),

    Win.

-spec update_roll_log(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_roll_log(#wx_state{system = #system{roll = RollLog}}, #wx_state{system = #system{roll = RollLog}}) ->
    ok;
update_roll_log(_, #wx_state{system = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_RollLog, wxListBox)),
    ok;
update_roll_log(_, #wx_state{system = #system{roll = []}}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_RollLog, wxListBox)),
    ok;
update_roll_log(_, #wx_state{system = #system{roll = RollLog}}) ->
    wxNotebook:setSelection(cauder_wx:find(?SYSTEM_Notebook_TraceAndRollLog, wxNotebook), ?SYSTEM_Notebook_Tab_RollLog),
    RollLogArea = cauder_wx:find(?SYSTEM_RollLog, wxListBox),
    wxListBox:freeze(RollLogArea),
    wxListBox:clear(RollLogArea),
    lists:foreach(fun(Entry) -> wxListBox:append(RollLogArea, Entry) end, RollLog),
    wxListBox:thaw(RollLogArea).
