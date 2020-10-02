-module(cauder_wx_process).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/1]).


-spec create(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Process Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Content = wxGridSizer:new(2, 2, 5, 5),
  wxStaticBoxSizer:add(Sizer, Content, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(Content, create_bindings(Win), Expand),
  wxSizer:add(Content, create_stack(Win), Expand),
  wxSizer:add(Content, create_log(Win), Expand),
  wxSizer:add(Content, create_history(Win), Expand),

  Win.


-spec update(Process :: cauder_types:process()) -> ok.

update(Process) ->
  update_bindings(Process),
  update_stack(Process),
  update_log(Process),
  update_history(Process).


%% ===== Bindings ===== %%


create_bindings(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Bindings"}]),
  wxPanel:setSizer(Win, Sizer),

  BindArea = wxListCtrl:new(Win, [{winid, ?BINDINGS_LIST}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  wxBoxSizer:add(Sizer, BindArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Item = wxListItem:new(),

  wxListItem:setText(Item, "Name"),
  wxListItem:setAlign(Item, ?wxLIST_FORMAT_LEFT),
  wxListCtrl:insertColumn(BindArea, 0, Item),

  wxListItem:setText(Item, "Value"),
  wxListCtrl:insertColumn(BindArea, 1, Item),

  wxListItem:destroy(Item),

  wxListCtrl:setColumnWidth(BindArea, 0, 100),
  wxListCtrl:setColumnWidth(BindArea, 1, 150),

  wxListCtrl:connect(BindArea, command_list_item_activated),

  Win.


update_bindings(#proc{env = Bs, exprs = Es}) ->
  BindArea = utils_gui:find(?BINDINGS_LIST, wxListCtrl),
  wxListCtrl:freeze(BindArea),
  wxListCtrl:deleteAllItems(BindArea),
  Bs0 = lists:zip(lists:seq(1, length(Bs)), Bs),
  Bs1 = Bs0, % TDOO
%%    case wxMenu:isChecked(MenuView, ?MENU_View_FullEnvironment) of
%%      true -> Bs0;
%%      false ->
%%        Es1 = cauder_syntax:to_abstract_expr(Es),
%%        Keys = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es1)),
%%        lists:filter(fun({_Id, {Key, _Val}}) -> sets:is_element(Key, Keys) end, Bs0)
%%    end,
  lists:foldl(
    fun({Id, {Key, Val}}, Row) ->
      wxListCtrl:insertItem(BindArea, Row, ""),
      wxListCtrl:setItemFont(BindArea, Row, wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL)),
      wxListCtrl:setItem(BindArea, Row, 0, atom_to_list(Key)),
      wxListCtrl:setItem(BindArea, Row, 1, io_lib:format("~p", [Val])),
      wxListCtrl:setItemData(BindArea, Row, Id),
      Row + 1
    end, 0, Bs1),
  wxListCtrl:thaw(BindArea).


%% ===== Stack ===== %%


create_stack(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Stack"}]),
  wxPanel:setSizer(Win, Sizer),

  StackArea = wxListBox:new(Win, ?STACK_LIST),
  wxStaticBoxSizer:add(Sizer, StackArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(StackArea, Font),

  Win.


update_stack(#proc{stack = Stk}) ->
  StackArea = utils_gui:find(?STACK_LIST, wxListBox),
  wxListBox:freeze(StackArea),
  wxListBox:clear(StackArea),
  Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:stack_entry/1, Stk)),
  lists:foreach(fun(Entry) -> wxListBox:append(StackArea, Entry) end, Entries),
  wxListBox:thaw(StackArea).


%% ===== Log ===== %%


create_log(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Log"}]),
  wxPanel:setSizer(Win, Sizer),

  LogArea = wxTextCtrl:new(Win, ?LOG_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, LogArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(LogArea, Font),

  Win.


update_log(#proc{log = Log}) ->
  LogArea = utils_gui:find(?LOG_TEXT, wxTextCtrl),
  wxTextCtrl:freeze(LogArea),
  wxTextCtrl:clear(LogArea),
  Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:log_entry/1, Log))),
  utils_gui:pp_marked_text(LogArea, Entries),
  wxTextCtrl:thaw(LogArea).


%% ===== History ===== %%


create_history(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "History"}]),
  wxPanel:setSizer(Win, Sizer),

  HistoryArea = wxTextCtrl:new(Win, ?HISTORY_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, HistoryArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(HistoryArea, Font),

  Win.


update_history(#proc{hist = Hist0}) ->
  HistoryArea = utils_gui:find(?HISTORY_TEXT, wxTextCtrl),
  wxTextCtrl:freeze(HistoryArea),
  wxTextCtrl:clear(HistoryArea),
  Hist1 = Hist0, % FIXME
%%    case wxMenu:isChecked(MenuView, ?MENU_View_FullHistory) of
%%      true -> Hist0;
%%      false -> lists:filter(fun pretty_print:is_conc_item/1, Hist0)
%%    end,
  Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:history_entry/1, Hist1))),
  utils_gui:pp_marked_text(HistoryArea, Entries),
  wxTextCtrl:thaw(HistoryArea).
