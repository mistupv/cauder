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
    Win = wxPanel:new(Parent),

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

    Sizer = wxStaticBoxSizer:new(?wxHORIZONTAL, Win, [{label, "Local Map"}]),
    wxPanel:setSizer(Win, Sizer),

    MapArea = wxListBox:new(Win, ?SYSTEM_Map),
    wxBoxSizer:add(Sizer, MapArea, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
    wxListBox:setFont(MapArea, Font),

    Win.

-spec update_map(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update_map(_, #wx_state{system = undefined}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Map, wxListBox)),
    ok;
update_map(_, #wx_state{system = #system{maps = []}}) ->
    wxListBox:clear(cauder_wx:find(?SYSTEM_Map, wxListBox)),
    ok;
update_map(_, #wx_state{system = #system{pool = Pool, maps = Maps}, pid = Pid}) ->
    MapArea = cauder_wx:find(?SYSTEM_Map, wxListBox),
    wxListBox:freeze(MapArea),
    wxListBox:clear(MapArea),
    #process{node = Node} = cauder_pool:get(Pid, Pool),
    Map = cauder_map:get_map(Maps, Node),
    lists:foreach(fun(Entry) -> wxListBox:append(MapArea, io_lib:format("~p", [Entry])) end, Map),
    wxListBox:thaw(MapArea).
