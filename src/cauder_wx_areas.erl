-module(cauder_wx_areas).

%% API
-export([create/1]).

-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates all the different areas and populates them.

-spec create(Parent) -> Window when
    Parent :: wxWindow:wxWindow(),
    Window :: wxWindow:wxWindow().

create(Frame) ->
    Win = wxPanel:new(Frame),

    Border = wxBoxSizer:new(?wxVERTICAL),
    wxWindow:setSizer(Win, Border),

    Content = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Border, Content, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

    CodePanel = cauder_wx_code:create(Win),
    ActionsPanel = cauder_wx_actions:create(Win),
    ProcessPanel = cauder_wx_process:create(Win),
    SystemPanel = cauder_wx_system:create(Win),
    MapPanel = cauder_wx_map:create(Win),

    % ----- Left ----- %

    Left = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Content, Left, [{proportion, 3}, {flag, ?wxEXPAND}]),

    wxSizer:add(Left, CodePanel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Left, ?SPACER_MEDIUM),
    wxSizer:add(Left, ProcessPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    wxSizer:addSpacer(Left, ?SPACER_MEDIUM),
    wxSizer:add(Left, MapPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    % -----

    wxSizer:addSpacer(Content, ?SPACER_MEDIUM),

    % ----- Right ----- %

    Right = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Content, Right, [{proportion, 2}, {flag, ?wxEXPAND}]),

    wxSizer:add(Right, ActionsPanel, [{proportion, 0}, {flag, ?wxEXPAND}]),
    wxSizer:addSpacer(Right, ?SPACER_MEDIUM),
    wxSizer:add(Right, SystemPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),

    % -----

    Win.
