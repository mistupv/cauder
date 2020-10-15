-module(cauder_wx_process).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/2]).
-export([update_bindings/2, update_stack/2, update_log/2, update_history/2]).


-spec create(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?PROCESS_Panel}]),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Process Info"}]),
  wxWindow:setSizer(Win, Sizer),

  Options = [{proportion, 1}, {flag, ?wxEXPAND}],

  % -----

  LeftPanel = wxPanel:new(Win),
  wxBoxSizer:add(Sizer, LeftPanel, Options),

  LeftSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(LeftPanel, LeftSizer),

  wxBoxSizer:add(LeftSizer, create_bindings(LeftPanel), Options),
  wxBoxSizer:addSpacer(LeftSizer, ?SPACER_SMALL),
  wxBoxSizer:add(LeftSizer, create_log(LeftPanel), Options),

  % -----

  wxBoxSizer:addSpacer(Sizer, ?SPACER_SMALL),

  % -----

  RightPanel = wxPanel:new(Win),
  wxBoxSizer:add(Sizer, RightPanel, Options),

  RightSizer = wxBoxSizer:new(?wxVERTICAL),
  wxPanel:setSizer(RightPanel, RightSizer),

  wxBoxSizer:add(RightSizer, create_stack(RightPanel), Options),
  wxBoxSizer:addSpacer(RightSizer, ?SPACER_SMALL),
  wxBoxSizer:add(RightSizer, create_history(RightPanel), Options),

  % -----

  Win.


-spec update(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update(System, Pid) ->
  update_bindings(System, Pid),
  update_stack(System, Pid),
  update_log(System, Pid),
  update_history(System, Pid).


-spec create_bindings(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_bindings(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?PROCESS_Bindings_Panel}]),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Bindings"}]),
  wxPanel:setSizer(Win, Sizer),

  BindingsControl = wxListCtrl:new(Win, [{winid, ?PROCESS_Bindings_Control}, {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  wxBoxSizer:add(Sizer, BindingsControl, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Item = wxListItem:new(),

  wxListItem:setText(Item, "Name"),
  wxListItem:setAlign(Item, ?wxLIST_FORMAT_LEFT),
  wxListCtrl:insertColumn(BindingsControl, 0, Item),

  wxListItem:setText(Item, "Value"),
  wxListCtrl:insertColumn(BindingsControl, 1, Item),

  wxListItem:destroy(Item),

  wxListCtrl:setColumnWidth(BindingsControl, 0, 100),
  wxListCtrl:setColumnWidth(BindingsControl, 1, 150),

  wxListCtrl:connect(BindingsControl, command_list_item_activated),

  Win.


-spec update_bindings(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_bindings(System, Pid) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  MenuBar = wxFrame:getMenuBar(Frame),
  Show = wxMenuBar:isChecked(MenuBar, ?MENU_View_Bindings),

  show_and_resize(utils_gui:find(?PROCESS_Bindings_Panel, wxPanel), Show),

  case Show of
    false -> ok;
    true ->
      BindingsControl = utils_gui:find(?PROCESS_Bindings_Control, wxListCtrl),
      wxListCtrl:freeze(BindingsControl),
      wxListCtrl:deleteAllItems(BindingsControl),
      case System of
        undefined -> ok;
        #sys{procs = PDict} ->
          case Pid of
            none -> ok;
            _ ->
              {ok, #proc{env = Bs, exprs = Es}} = orddict:find(Pid, PDict),
              Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

              Bs0 = lists:zip(lists:seq(1, length(Bs)), Bs), % [{Idx, Binding}]
              Bs1 =
                case wxMenuBar:isChecked(MenuBar, ?MENU_View_AllBindings) of
                  true -> Bs0;
                  false ->
                    Es1 = cauder_syntax:to_abstract_expr(Es),
                    Keys = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es1)),
                    lists:filter(fun({_Id, {Key, _Val}}) -> sets:is_element(Key, Keys) end, Bs0)
                end,
              lists:foldl(
                fun({Idx, {Key, Val}}, Row) ->
                  wxListCtrl:insertItem(BindingsControl, Row, ""),
                  wxListCtrl:setItemFont(BindingsControl, Row, Font),
                  wxListCtrl:setItem(BindingsControl, Row, 0, atom_to_list(Key)),
                  wxListCtrl:setItem(BindingsControl, Row, 1, io_lib:format("~p", [Val])),
                  wxListCtrl:setItemData(BindingsControl, Row, Idx),
                  Row + 1
                end, 0, Bs1)
          end
      end,
      wxListCtrl:thaw(BindingsControl)
  end.


%%--------------------------------------------------------------------


-spec create_stack(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_stack(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?PROCESS_Stack_Panel}]),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Stack"}]),
  wxPanel:setSizer(Win, Sizer),

  StackArea = wxListBox:new(Win, ?PROCESS_Stack_Control),
  wxStaticBoxSizer:add(Sizer, StackArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxListBox:setFont(StackArea, Font),

  Win.


-spec update_stack(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_stack(System, Pid) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  MenuBar = wxFrame:getMenuBar(Frame),
  Show = wxMenuBar:isChecked(MenuBar, ?MENU_View_Stack),

  show_and_resize(utils_gui:find(?PROCESS_Stack_Panel, wxPanel), Show),

  case Show of
    false -> ok;
    true ->
      StackControl = utils_gui:find(?PROCESS_Stack_Control, wxListBox),
      wxListBox:freeze(StackControl),
      wxListBox:clear(StackControl),
      case System of
        undefined -> ok;
        #sys{procs = PDict} ->
          case Pid of
            none -> ok;
            _ ->
              {ok, #proc{stack = Stk}} = orddict:find(Pid, PDict),
              Entries = lists:map(fun lists:flatten/1, lists:map(fun pretty_print:stack_entry/1, Stk)),
              lists:foreach(fun(Entry) -> wxListBox:append(StackControl, Entry) end, Entries)
          end
      end,
      wxListBox:thaw(StackControl)
  end.


%%--------------------------------------------------------------------


-spec create_log(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_log(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?PROCESS_Log_Panel}]),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Log"}]),
  wxPanel:setSizer(Win, Sizer),

  LogArea = wxTextCtrl:new(Win, ?PROCESS_Log_Control, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, LogArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(LogArea, Font),

  Win.


-spec update_log(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_log(System, Pid) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  MenuBar = wxFrame:getMenuBar(Frame),
  Show = wxMenuBar:isChecked(MenuBar, ?MENU_View_Log),

  show_and_resize(utils_gui:find(?PROCESS_Log_Panel, wxPanel), Show),

  case Show of
    false -> ok;
    true ->
      LogControl = utils_gui:find(?PROCESS_Log_Control, wxTextCtrl),
      wxTextCtrl:freeze(LogControl),
      wxTextCtrl:clear(LogControl),
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
                  utils_gui:pp_marked_text(LogControl, Entries)
              end
          end
      end,
      wxTextCtrl:thaw(LogControl)
  end.


%%--------------------------------------------------------------------


-spec create_history(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create_history(Parent) ->
  Win = wxPanel:new(Parent, [{winid, ?PROCESS_History_Panel}]),

  Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "History"}]),
  wxPanel:setSizer(Win, Sizer),

  HistoryArea = wxTextCtrl:new(Win, ?PROCESS_History_Control, [{style, ?wxTE_MULTILINE bor ?wxTE_READONLY bor ?wxTE_RICH2}]),
  wxStaticBoxSizer:add(Sizer, HistoryArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(HistoryArea, Font),

  Win.


-spec update_history(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update_history(System, Pid) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  MenuBar = wxFrame:getMenuBar(Frame),
  Show = wxMenuBar:isChecked(MenuBar, ?MENU_View_History),

  show_and_resize(utils_gui:find(?PROCESS_History_Panel, wxPanel), Show),

  case Show of
    false -> ok;
    true ->
      HistoryControl = utils_gui:find(?PROCESS_History_Control, wxTextCtrl),
      wxTextCtrl:freeze(HistoryControl),
      wxTextCtrl:clear(HistoryControl),
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
              utils_gui:pp_marked_text(HistoryControl, Entries)
          end
      end,
      wxTextCtrl:thaw(HistoryControl)
  end.


-spec is_conc_item(cauder_types:history_entry()) -> boolean().

is_conc_item({spawn, _Bs, _Es, _Stk, _Pid}) -> true;
is_conc_item({send, _Bs, _Es, _Stk, _Msg})  -> true;
is_conc_item({rec, _Bs, _Es, _Stk, _Msg})   -> true;
is_conc_item(_)                             -> false.


%%--------------------------------------------------------------------


show_and_resize(Panel, Show) ->
  wxPanel:show(Panel, [{show, Show}]),

  % -----

  ParentSizer = wx:typeCast(wxWindow:getSizer(wxWindow:getParent(Panel)), wxSizer),

  [Top, Spacer0, Bottom] = wxSizer:getChildren(ParentSizer),

  ShowTop = wxSizerItem:isShown(Top),
  ShowBottom = wxSizerItem:isShown(Bottom),

  wxSizerItem:show(Spacer0, ShowTop and ShowBottom),

  % -----

  ProcessPanel = utils_gui:find(?PROCESS_Panel, wxPanel),
  ProcessSizer = wx:typeCast(wxPanel:getSizer(ProcessPanel), wxSizer),

  [Left, Spacer1, Right] = wxSizer:getChildren(ProcessSizer),

  LeftSizer = wx:typeCast(wxWindow:getSizer(wxSizerItem:getWindow(Left)), wxSizer),
  RightSizer = wx:typeCast(wxWindow:getSizer(wxSizerItem:getWindow(Right)), wxSizer),

  ShowLeft = lists:any(fun wxSizerItem:isShown/1, wxSizer:getChildren(LeftSizer)),
  ShowRight = lists:any(fun wxSizerItem:isShown/1, wxSizer:getChildren(RightSizer)),

  wxSizerItem:show(Left, ShowLeft),
  wxSizerItem:show(Spacer1, ShowLeft and ShowRight),
  wxSizerItem:show(Right, ShowRight),

  % -----

  wxPanel:layout(ProcessPanel).
