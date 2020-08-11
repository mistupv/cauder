-module(cauder_wx_process).

-export([process_info_area/1, update_process_info/0]).


-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


%% ===== Process Info ===== %%

-spec process_info_area(Parent :: wxWindow:wxWindow()) -> ProcessInfoArea :: wxPanel:wxPanel().

process_info_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Process Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Content = wxGridSizer:new(2, 2, 5, 5),
  wxStaticBoxSizer:add(Sizer, Content, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

  wxSizer:add(Content, bind_area(Win), Expand),
  wxSizer:add(Content, stack_area(Win), Expand),
  wxSizer:add(Content, log_area(Win), Expand),
  wxSizer:add(Content, history_area(Win), Expand),

  Win.


-spec update_process_info() -> ok.

update_process_info() ->
  _ViewOpts = utils_gui:toggle_opts(),

  update_bindings(),
  update_stack(),
  update_log(),
  update_history().


%% ===== Bindings ===== %%


bind_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Bindings"}]),
  wxPanel:setSizer(Win, Sizer),

  BindArea = wxListCtrl:new(Win, [{winid, ?BINDINGS_LIST}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  ref_add(?BINDINGS_LIST, BindArea),
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


update_bindings() ->
  MenuView = ref_lookup(?MENU_VIEW),
  BindArea = ref_lookup(?BINDINGS_LIST),
  wxListCtrl:freeze(BindArea),
  wxListCtrl:deleteAllItems(BindArea),
  case utils_gui:current_process() of
    none -> ok;
    #proc{env = Bs, exprs = Es} ->
      Bs1 =
        case wxMenu:isChecked(MenuView, ?RADIO_FULL_ENV) of
          true -> Bs;
          false -> pretty_print:relevant_bindings(Bs, Es)
        end,
      lists:foldl(
        fun({Var, Val}, Row) ->
          wxListCtrl:insertItem(BindArea, Row, ""),
          wxListCtrl:setItemFont(BindArea, Row, wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL)),
          wxListCtrl:setItem(BindArea, Row, 0, atom_to_list(Var)),
          wxListCtrl:setItem(BindArea, Row, 1, io_lib:format("~p", [Val])),
          Row + 1
        end, 0, Bs1)
  end,
  wxListCtrl:thaw(BindArea).


%% ===== Stack ===== %%


stack_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Stack"}]),
  wxPanel:setSizer(Win, Sizer),

  StackArea = wxListBox:new(Win, ?STACK_LIST),
  ref_add(?STACK_LIST, StackArea),
  wxStaticBoxSizer:add(Sizer, StackArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(StackArea, Font),

  Win.


update_stack() ->
  StackArea = ref_lookup(?STACK_LIST),
  wxListBox:freeze(StackArea),
  wxListBox:clear(StackArea),
  case utils_gui:current_process() of
    none -> ok;
    #proc{stack = Stk} ->
      Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:stack_entry/1, Stk)),
      lists:foreach(fun(Entry) -> wxListBox:append(StackArea, Entry) end, Entries)
  end,
  wxListBox:thaw(StackArea).


%% ===== Log ===== %%


log_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Log"}]),
  wxPanel:setSizer(Win, Sizer),

  LogArea = wxTextCtrl:new(Win, ?LOG_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  ref_add(?LOG_TEXT, LogArea),
  wxStaticBoxSizer:add(Sizer, LogArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(LogArea, Font),

  Win.


update_log() ->
  LogArea = ref_lookup(?LOG_TEXT),
  wxTextCtrl:freeze(LogArea),
  wxTextCtrl:clear(LogArea),
  case utils_gui:current_process() of
    none -> ok;
    #proc{log = Log} ->
      Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:log_entry/1, Log))),
      utils_gui:pp_marked_text(LogArea, Entries)
  end,
  wxTextCtrl:thaw(LogArea).


%% ===== History ===== %%


history_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "History"}]),
  wxPanel:setSizer(Win, Sizer),

  HistoryArea = wxTextCtrl:new(Win, ?HISTORY_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  ref_add(?HISTORY_TEXT, HistoryArea),
  wxStaticBoxSizer:add(Sizer, HistoryArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(HistoryArea, Font),

  Win.


update_history() ->
  MenuView = ref_lookup(?MENU_VIEW),
  HistoryArea = ref_lookup(?HISTORY_TEXT),
  wxTextCtrl:freeze(HistoryArea),
  wxTextCtrl:clear(HistoryArea),
  case utils_gui:current_process() of
    none -> ok;
    #proc{hist = Hist0} ->
      Hist1 =
        case wxMenu:isChecked(MenuView, ?RADIO_FULL_HIST) of
          true -> Hist0;
          false -> lists:filter(fun pretty_print:is_conc_item/1, Hist0)
        end,
      Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:history_entry/1, Hist1))),
      utils_gui:pp_marked_text(HistoryArea, Entries)
  end,
  wxTextCtrl:thaw(HistoryArea).


%% ===== Utils ===== %%


ref_add(Id, Ref) -> cauder_gui:ref_add(Id, Ref).
ref_lookup(Id) -> cauder_gui:ref_lookup(Id).
