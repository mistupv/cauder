-module(cauder_wx_system).

%% API
-export([create/1, update/2]).

-include("cauder_system.hrl").
-include("cauder_message.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

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
        fun(#message{uid = Uid, src = Src, dst = Dst, val = Val}, Row) ->
            wxListCtrl:insertItem(MailArea, Row, ""),
            wxListCtrl:setItemFont(MailArea, Row, Font),
            wxListCtrl:setItem(MailArea, Row, 0, cauder_pp:to_string(Uid)),
            wxListCtrl:setItem(MailArea, Row, 1, cauder_pp:to_string(Val)),
            wxListCtrl:setItem(MailArea, Row, 2, cauder_pp:to_string(Src)),
            wxListCtrl:setItem(MailArea, Row, 3, cauder_pp:to_string(Dst)),
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
            Messages = lists:flatmap(fun queue:to_list/1, cauder_mailbox:find_destination(Pid, Mail)),
            lists:foldl(
                fun(#message{uid = Uid, src = Src, dst = Dst, val = Val}, Row) ->
                    wxListCtrl:insertItem(MailArea, Row, ""),
                    wxListCtrl:setItemFont(MailArea, Row, Font),
                    wxListCtrl:setItem(MailArea, Row, 0, cauder_pp:to_string(Uid)),
                    wxListCtrl:setItem(MailArea, Row, 1, cauder_pp:to_string(Val)),
                    wxListCtrl:setItem(MailArea, Row, 2, cauder_pp:to_string(Src)),
                    wxListCtrl:setItem(MailArea, Row, 3, cauder_pp:to_string(Dst)),
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

    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(Win, Sizer),

    TraceArea = wxListBox:new(Win, ?SYSTEM_Trace),
    wxBoxSizer:add(Sizer, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    wxListBox:setFont(TraceArea, Font),

    Win.

-spec update_trace(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_trace(
    #wx_state{system = #system{trace = Trace}, pid = Pid},
    #wx_state{system = #system{trace = Trace}, pid = Pid}
) ->
    ok;
update_trace(_, #wx_state{system = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Trace, wxListBox)),
    ok;
update_trace(_, #wx_state{pid = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Trace, wxListBox)),
    ok;
update_trace(_, #wx_state{system = #system{trace = Trace}, pid = Pid}) ->
    TraceArea = cauder_wx:find(?SYSTEM_Trace, wxListBox),
    wxListBox:freeze(TraceArea),
    wxListBox:clear(TraceArea),
    case cauder_trace:get(Pid, Trace) of
        [] ->
            ok;
        Actions ->
            Entries = lists:map(fun cauder_pp:trace_action/1, Actions),
            lists:foreach(fun(Entry) -> wxListBox:append(TraceArea, Entry) end, Entries)
    end,
    wxListBox:thaw(TraceArea),
    ok.

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
