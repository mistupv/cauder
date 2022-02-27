-module(cauder_wx_map).

%% API
-export([create/1, update/2]).

-include("cauder_system.hrl").
-include("cauder_process.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates the <i>map info</i> panel and populates it.

-spec create(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create(Parent) ->
    Win = wxPanel:new(Parent, [{winid, ?SYSTEM_Map}]),

    Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Map Info"}]),
    wxWindow:setSizer(Win, Sizer),

    Expand = [{proportion, 1}, {flag, ?wxEXPAND}],

    LeftPanel = wxPanel:new(Win),
    wxBoxSizer:add(Sizer, LeftPanel, Expand),

    LeftSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxPanel:setSizer(LeftPanel, LeftSizer),

    wxBoxSizer:add(LeftSizer, create_map(LeftPanel), Expand),

    Win.

%%------------------------------------------------------------------------------
%% @doc Updates the <i>map info</i> panel according to the given new state,
%% by comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update(OldState, NewState) ->
    update_map(OldState, NewState).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec create_map(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

  create_map(Parent) ->
      Win = wxPanel:new(Parent),

      Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Node Map"}]),
      wxPanel:setSizer(Win, Sizer),

      BindingsControl = wxListCtrl:new(Win, [
          {winid, ?PROCESS_Map},
          {style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}
      ]),
      wxBoxSizer:add(Sizer, BindingsControl, [{proportion, 1}, {flag, ?wxEXPAND}]),

      Item = wxListItem:new(),

      wxListItem:setText(Item, "TUPLE ID"),
      wxListItem:setAlign(Item, ?wxLIST_FORMAT_LEFT),
      wxListCtrl:insertColumn(BindingsControl, 0, Item),

      wxListItem:setText(Item, "ATOM"),
      wxListCtrl:insertColumn(BindingsControl, 1, Item),

      wxListItem:setText(Item, "PID"),
      wxListCtrl:insertColumn(BindingsControl, 2, Item),

      wxListItem:destroy(Item),

      wxListCtrl:setColumnWidth(BindingsControl, 0, 100),
      wxListCtrl:setColumnWidth(BindingsControl, 1, 150),
      wxListCtrl:setColumnWidth(BindingsControl, 2, 150),

      wxListCtrl:connect(BindingsControl, command_list_item_activated),

      Win.

-spec update_map(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_map(_, #wx_state{system = undefined}) ->
  wxListCtrl:deleteAllItems(cauder_wx:find(?PROCESS_Map, wxListCtrl)),
    ok;
update_map(_, #wx_state{system = #system{maps = []}}) ->
    wxListCtrl:deleteAllItems(cauder_wx:find(?PROCESS_Map, wxListCtrl)),
    ok;
update_map(_, #wx_state{system = #system{pool = Pool, maps = Maps}, pid = Pid}) ->
    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    MapArea = cauder_wx:find(?PROCESS_Map, wxListCtrl),
    wxListCtrl:freeze(MapArea),
    wxListCtrl:deleteAllItems(MapArea),
    #process{node = Node} = cauder_pool:get(Pid, Pool),
    Map = cauder_map:get_map(Maps, Node),
    lists:foldl(
        fun(Entry,Row) ->
          {A,P,K} = Entry,
          wxListCtrl:insertItem(MapArea, Row, ""),
          wxListCtrl:setItemFont(MapArea, Row, Font),
          wxListCtrl:setItem(MapArea, Row, 0, cauder_pp:to_string(K)),
          wxListCtrl:setItem(MapArea, Row, 1, cauder_pp:to_string(A)),
          wxListCtrl:setItem(MapArea, Row, 2, cauder_pp:to_string(P)),
          Row + 1
        end, 0, Map),
    wxListCtrl:thaw(MapArea).
