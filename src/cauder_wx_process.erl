-module(cauder_wx_process).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/2]).


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


-spec update(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update(System, Pid) ->
  update_bindings(System, Pid),
  update_stack(System, Pid),
  update_log(System, Pid),
  update_history(System, Pid).


%% ===== Bindings ===== %%


-spec create_bindings(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

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


-spec update_bindings(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_bindings(System, Pid) ->
  BindArea = utils_gui:find(?BINDINGS_LIST, wxListCtrl),
  wxListCtrl:freeze(BindArea),
  wxListCtrl:deleteAllItems(BindArea),
  case System of
    undefined -> ok;
    #sys{procs = PDict} ->
      case Pid of
        none -> ok;
        _ ->
          {ok, #proc{env = Bs, exprs = Es}} = orddict:find(Pid, PDict),
          MenuBar = wxFrame:getMenuBar(utils_gui:find(?FRAME, wxFrame)),
          Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

          Bs0 = lists:zip(lists:seq(1, length(Bs)), Bs), % [{Idx, Binding}]
          Bs1 =
            case wxMenuBar:isChecked(MenuBar, ?MENU_View_FullEnvironment) of
              true -> Bs0;
              false ->
                Es1 = cauder_syntax:to_abstract_expr(Es),
                Keys = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es1)),
                lists:filter(fun({_Id, {Key, _Val}}) -> sets:is_element(Key, Keys) end, Bs0)
            end,
          lists:foldl(
            fun({Idx, {Key, Val}}, Row) ->
              wxListCtrl:insertItem(BindArea, Row, ""),
              wxListCtrl:setItemFont(BindArea, Row, Font),
              wxListCtrl:setItem(BindArea, Row, 0, atom_to_list(Key)),
              wxListCtrl:setItem(BindArea, Row, 1, io_lib:format("~p", [Val])),
              wxListCtrl:setItemData(BindArea, Row, Idx),
              Row + 1
            end, 0, Bs1)
      end
  end,
  wxListCtrl:thaw(BindArea).


%% ===== Stack ===== %%


-spec create_stack(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_stack(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Stack"}]),
  wxPanel:setSizer(Win, Sizer),

  StackArea = wxListBox:new(Win, ?STACK_LIST),
  wxStaticBoxSizer:add(Sizer, StackArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(StackArea, Font),

  Win.


-spec update_stack(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_stack(System, Pid) ->
  StackArea = utils_gui:find(?STACK_LIST, wxListBox),
  wxListBox:freeze(StackArea),
  wxListBox:clear(StackArea),
  case System of
    undefined -> ok;
    #sys{procs = PDict} ->
      case Pid of
        none -> ok;
        _ ->
          {ok, #proc{stack = Stk}} = orddict:find(Pid, PDict),
          Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:stack_entry/1, Stk)),
          lists:foreach(fun(Entry) -> wxListBox:append(StackArea, Entry) end, Entries)
      end
  end,
  wxListBox:thaw(StackArea).


%% ===== Log ===== %%


-spec create_log(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_log(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Log"}]),
  wxPanel:setSizer(Win, Sizer),

  LogArea = wxTextCtrl:new(Win, ?LOG_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, LogArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(LogArea, Font),

  Win.


-spec update_log(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_log(System, Pid) ->
  LogArea = utils_gui:find(?LOG_TEXT, wxTextCtrl),
  wxTextCtrl:freeze(LogArea),
  wxTextCtrl:clear(LogArea),
  case System of
    undefined -> ok;
    #sys{logs = Logs} ->
      case Pid of
        none -> ok;
        _ ->
          case orddict:find(Pid, Logs) of
            error -> ok;
            {ok, Log} ->
              Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:log_entry/1, Log))),
              utils_gui:pp_marked_text(LogArea, Entries)
          end
      end
  end,
  wxTextCtrl:thaw(LogArea).


%% ===== History ===== %%


-spec create_history(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_history(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "History"}]),
  wxPanel:setSizer(Win, Sizer),

  HistoryArea = wxTextCtrl:new(Win, ?HISTORY_TEXT, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, HistoryArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(HistoryArea, Font),

  Win.


-spec update_history(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_history(System, Pid) ->
  HistoryArea = utils_gui:find(?HISTORY_TEXT, wxTextCtrl),
  wxTextCtrl:freeze(HistoryArea),
  wxTextCtrl:clear(HistoryArea),
  case System of
    undefined -> ok;
    #sys{procs = PDict} ->
      case Pid of
        none -> ok;
        _ ->
          {ok, #proc{hist = Hist}} = orddict:find(Pid, PDict),
          MenuBar = wxFrame:getMenuBar(utils_gui:find(?FRAME, wxFrame)),
          Hist1 =
            case wxMenuBar:isChecked(MenuBar, ?MENU_View_FullHistory) of
              true -> Hist;
              false -> lists:filter(fun is_conc_item/1, Hist)
            end,
          Entries = lists:flatten(lists:join("\n", lists:map(fun pretty_print:history_entry/1, Hist1))),
          utils_gui:pp_marked_text(HistoryArea, Entries)
      end
  end,
  wxTextCtrl:thaw(HistoryArea).


-spec is_conc_item(cauder_types:history_entry()) -> boolean().

is_conc_item({spawn, _Bs, _Es, _Stk, _Pid}) -> true;
is_conc_item({send, _Bs, _Es, _Stk, _Msg})  -> true;
is_conc_item({rec, _Bs, _Es, _Stk, _Msg})   -> true;
is_conc_item(_)                             -> false.
