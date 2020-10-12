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

  Notebook = wxNotebook:new(Win, ?SYSTEM_Notebook),
  wxNotebook:addPage(Notebook, create_trace(Notebook), "Trace"),
  wxNotebook:addPage(Notebook, create_roll_log(Notebook), "Roll Log"),
  wxSizer:add(Sizer, Notebook, Expand),

  Win.


-spec update(System :: cauder_types:system() | 'undefined') -> ok.

update(System) ->
  update_mail(System),
  update_trace(System),
  update_roll_log(System).


%% ===== Mail ===== %%


-spec create_mail(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_mail(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Mail"}]),
  wxPanel:setSizer(Win, Sizer),

  MailArea = wxListCtrl:new(Win, [{winid, ?SYSTEM_Mail}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
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


-spec update_mail(System :: cauder_types:system() | 'undefined') -> ok.

update_mail(System) ->
  MailArea = utils_gui:find(?SYSTEM_Mail, wxListCtrl),
  wxListCtrl:freeze(MailArea),
  wxListCtrl:deleteAllItems(MailArea),
  case System of
    undefined -> ok;
    #sys{mail = Mail} ->
      Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
      lists:foldl(
        fun(#msg{dest = Dest, val = Value, uid = UID}, Row) ->
          wxListCtrl:insertItem(MailArea, Row, ""),
          wxListCtrl:setItemFont(MailArea, Row, Font),
          wxListCtrl:setItem(MailArea, Row, 0, integer_to_list(Dest)),
          wxListCtrl:setItem(MailArea, Row, 1, io_lib:format("~p", [Value])),
          wxListCtrl:setItem(MailArea, Row, 2, integer_to_list(UID)),
          Row + 1
        end, 0, Mail)
  end,
  wxListCtrl:thaw(MailArea).


%% ===== Trace ===== %%


-spec create_trace(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_trace(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  TraceArea = wxListBox:new(Win, ?SYSTEM_Trace),
  wxBoxSizer:add(Sizer, TraceArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(TraceArea, Font),

  Win.


-spec update_trace(System :: cauder_types:system() | 'undefined') -> ok.

update_trace(System) ->
  TraceArea = utils_gui:find(?SYSTEM_Trace, wxListBox),
  wxListBox:freeze(TraceArea),
  wxListBox:clear(TraceArea),
  case System of
    undefined -> ok;
    #sys{trace = Trace} ->
      Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:trace_entry/1, Trace)),
      lists:foreach(fun(Entry) -> wxListBox:append(TraceArea, Entry) end, Entries)
  end,
  wxListBox:thaw(TraceArea).


%% ===== Roll Log ===== %%


-spec create_roll_log(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_roll_log(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  RollLogArea = wxListBox:new(Win, ?SYSTEM_RollLog),
  wxBoxSizer:add(Sizer, RollLogArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(RollLogArea, Font),

  Win.


-spec update_roll_log(System :: cauder_types:system() | 'undefined') -> ok.

update_roll_log(System) ->
  RollLogArea = utils_gui:find(?SYSTEM_RollLog, wxListBox),
  wxListBox:freeze(RollLogArea),
  wxListBox:clear(RollLogArea),
  case System of
    undefined -> ok;
    #sys{roll = RollLog} ->
      lists:foreach(fun(Entry) -> wxListBox:append(RollLogArea, Entry) end, RollLog)
  end,
  wxListBox:thaw(RollLogArea).


-spec focus_roll_log(boolean()) -> 'ok'.

focus_roll_log(false) -> ok;
focus_roll_log(true)  -> wxNotebook:setSelection(utils_gui:find(?SYSTEM_Notebook, wxNotebook), ?SYSTEM_Notebook_RollLog), ok.
