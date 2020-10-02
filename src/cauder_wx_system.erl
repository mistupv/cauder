-module(cauder_wx_system).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/1, focus_roll_log/1]).


-spec create(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "System Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(Sizer, create_mail(Win), Expand),

  wxSizer:addSpacer(Sizer, 5),

  Notebook = wxNotebook:new(Win, ?SYSTEM_INFO_NOTEBOOK),
  wxNotebook:addPage(Notebook, create_trace(Notebook), "Trace"),
  wxNotebook:addPage(Notebook, create_roll_log(Notebook), "Roll Log"),
  wxSizer:add(Sizer, Notebook, Expand),

  Win.


-spec update(System :: cauder_types:system()) -> ok.

update(System) ->
  update_mail(System),
  update_trace(System),
  update_roll_log(System).


%% ===== Mail ===== %%


create_mail(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Mail"}]),
  wxPanel:setSizer(Win, Sizer),

  MailArea = wxListCtrl:new(Win, [{winid, ?MAIL_LIST}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  wxBoxSizer:add(Sizer, MailArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Item = wxListItem:new(),
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

  wxListItem:setText(Item, "Dest."),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 0, Item),

  wxListItem:setText(Item, "Value"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 1, Item),

  wxListItem:setText(Item, "UID"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MailArea, 2, Item),

  wxListItem:destroy(Item),

  wxListCtrl:setColumnWidth(MailArea, 0, 75),
  wxListCtrl:setColumnWidth(MailArea, 1, 150),
  wxListCtrl:setColumnWidth(MailArea, 2, 75),

  wxListCtrl:connect(MailArea, command_list_item_activated),

  Win.


update_mail(#sys{mail = Mail}) ->
  MailArea = utils_gui:find(?MAIL_LIST, wxListCtrl),
  wxListCtrl:freeze(MailArea),
  wxListCtrl:deleteAllItems(MailArea),
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  lists:foldl(
    fun(#msg{dest = Dest, val = Value, uid = UID}, Row) ->
      wxListCtrl:insertItem(MailArea, Row, ""),
      wxListCtrl:setItemFont(MailArea, Row, Font),
      wxListCtrl:setItem(MailArea, Row, 0, integer_to_list(Dest)),
      wxListCtrl:setItem(MailArea, Row, 1, io_lib:format("~p", [Value])),
      wxListCtrl:setItem(MailArea, Row, 2, integer_to_list(UID)),
      Row + 1
    end, 0, Mail),
  wxListCtrl:thaw(MailArea).


%% ===== Trace ===== %%


create_trace(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  TraceArea = wxListBox:new(Win, ?TRACE_LIST),
  wxBoxSizer:add(Sizer, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(TraceArea, Font),

  Win.


update_trace(#sys{trace = Trace}) ->
  TraceArea = utils_gui:find(?TRACE_LIST, wxListBox),
  wxListBox:freeze(TraceArea),
  wxListBox:clear(TraceArea),
  Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:trace_entry/1, Trace)),
  lists:foreach(fun(Entry) -> wxListBox:append(TraceArea, Entry) end, Entries),
  wxListBox:thaw(TraceArea).


%% ===== Roll Log ===== %%


create_roll_log(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  RollLogArea = wxListBox:new(Win, ?ROLL_LOG_LIST),
  wxBoxSizer:add(Sizer, RollLogArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(RollLogArea, Font),

  Win.


update_roll_log(#sys{roll = RollLog}) ->
  RollLogArea = utils_gui:find(?ROLL_LOG_LIST, wxListBox),
  wxListBox:freeze(RollLogArea),
  wxListBox:clear(RollLogArea),
  lists:foreach(fun(Entry) -> wxListBox:append(RollLogArea, Entry) end, RollLog),
  wxListBox:thaw(RollLogArea).


focus_roll_log(false) -> ok;
focus_roll_log(true)  -> wxNotebook:setSelection(utils_gui:find(?SYSTEM_INFO_NOTEBOOK, wxNotebook), ?PAGEPOS_ROLL).
