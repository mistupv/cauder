-module(cauder_wx_system).

-export([system_info_area/1, update_system_info/0]).


-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


%% ===== System Info ===== %%


-spec system_info_area(Parent :: wxWindow:wxWindow()) -> SystemInfoArea :: wxPanel:wxPanel().

system_info_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "System Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(Sizer, mail_area(Win), Expand),

  wxSizer:addSpacer(Sizer, 5),

  Notebook = wxNotebook:new(Win, ?SYSTEM_INFO_NOTEBOOK),
  ref_add(?SYSTEM_INFO_NOTEBOOK, Notebook),
  wxNotebook:addPage(Notebook, trace_area(Notebook), "Trace"),
  wxNotebook:addPage(Notebook, roll_log_area(Notebook), "Roll Log"),
  wxSizer:add(Sizer, Notebook, Expand),

  Win.


-spec update_system_info() -> ok.

update_system_info() ->
  update_mail(),
  update_trace(),
  update_roll_log().


%% ===== Mail ===== %%


mail_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Mail"}]),
  wxPanel:setSizer(Win, Sizer),

  MailArea = wxListCtrl:new(Win, [{winid, ?MAIL_LIST}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  ref_add(?MAIL_LIST, MailArea),
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


update_mail() ->
  MailArea = ref_lookup(?MAIL_LIST),
  wxListCtrl:freeze(MailArea),
  wxListCtrl:deleteAllItems(MailArea),
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  #sys{mail = Mail} = ref_lookup(?SYSTEM),
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


trace_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  TraceArea = wxListBox:new(Win, ?TRACE_LIST),
  ref_add(?TRACE_LIST, TraceArea),
  wxBoxSizer:add(Sizer, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(TraceArea, Font),

  Win.


update_trace() ->
  TraceArea = ref_lookup(?TRACE_LIST),
  wxListBox:freeze(TraceArea),
  wxListBox:clear(TraceArea),
  #sys{trace = Trace} = ref_lookup(?SYSTEM),
  Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:trace_entry/1, Trace)),
  lists:foreach(fun(Entry) -> wxListBox:append(TraceArea, Entry) end, Entries),
  wxListBox:thaw(TraceArea).


%% ===== Roll Log ===== %%


roll_log_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  RollLogArea = wxListBox:new(Win, ?ROLL_LOG_LIST),
  ref_add(?ROLL_LOG_LIST, RollLogArea),
  wxBoxSizer:add(Sizer, RollLogArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(RollLogArea, Font),

  Win.


update_roll_log() ->
  RollLogArea = ref_lookup(?ROLL_LOG_LIST),
  wxListBox:freeze(RollLogArea),
  wxListBox:clear(RollLogArea),
  #sys{roll = Roll} = ref_lookup(?SYSTEM),
  lists:foreach(fun(Entry) -> wxListBox:append(RollLogArea, Entry) end, Roll),
  wxListBox:thaw(RollLogArea).


%% ===== Utils ===== %%


ref_add(Id, Ref) -> cauder_gui:ref_add(Id, Ref).
ref_lookup(Id) -> cauder_gui:ref_lookup(Id).
