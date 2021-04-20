-module(cauder_wx_menu).

%% API
-export([create/1, update/2]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

-record(menu_item, {
    id :: integer(),
    text :: string(),
    help = "" :: string(),
    kind = normal :: normal | check | radio,
    enabled = true :: boolean()
}).

-type menuItemDesc() :: #menu_item{} | separator.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Creates the menu bar and populates it.

-spec create(Frame) -> MenuBar when
    Frame :: wxFrame:wxFrame(),
    MenuBar :: wxMenuBar:wxMenuBar().

create(Frame) ->
    MenuBar = wxMenuBar:new(),

    FileMenu = create_menu(
        [
            #menu_item{
                id = ?MENU_File_Open,
                text = "&Open...\tCtrl+O",
                help = ?HELP_File_Open
            },
            separator,
            #menu_item{
                id = ?MENU_File_Exit,
                text = "E&xit",
                help = ?HELP_File_Exit
            }
        ]
    ),

    EditMenu = create_menu(
        [
            #menu_item{
                id = ?MENU_Edit_Undo,
                text = "&Undo\tCtrl+Z",
                enabled = false
            },
            #menu_item{
                id = ?MENU_Edit_Redo,
                text = "&Redo\tCtrl+Y",
                enabled = false
            }
        ]
    ),

    ViewMenu = create_menu(
        [
            #menu_item{
                id = ?MENU_View_ZoomIn,
                text = "Zoom &In\tCtrl++",
                help = ?HELP_View_ZoomIn,
                enabled = false
            },
            #menu_item{
                id = ?MENU_View_ZoomOut,
                text = "Zoom &Out\tCtrl+-",
                help = ?HELP_View_ZoomOut,
                enabled = false
            },
            #menu_item{
                id = ?MENU_View_Zoom100,
                text = "Zoo&m 100%",
                help = ?HELP_View_Zoom100
            },
            separator,
            %% TODO Show/hide
            #menu_item{
                id = ?MENU_View_CurrentExpression,
                text = "Show Current E&xpression",
                help = ?HELP_View_CurrentExpression,
                kind = check,
                enabled = false
            },
            separator,
            #menu_item{
                id = ?MENU_View_Bindings,
                text = "Show &Environment",
                help = ?HELP_View_Bindings,
                kind = check
            },
            #menu_item{
                id = ?MENU_View_Stack,
                text = "Show S&tack",
                help = ?HELP_View_Stack,
                kind = check
            },
            #menu_item{
                id = ?MENU_View_Log,
                text = "Show &Log",
                help = ?HELP_View_Log,
                kind = check
            },
            #menu_item{
                id = ?MENU_View_History,
                text = "Show &History",
                help = ?HELP_View_History,
                kind = check
            },
            separator,
            #menu_item{
                id = ?MENU_View_RelevantBindings,
                text = "Relevant Environment",
                help = ?HELP_View_RelevantBindings,
                kind = radio
            },
            #menu_item{
                id = ?MENU_View_AllBindings,
                text = "Full Environment",
                help = ?HELP_View_AllBindings,
                kind = radio
            },
            separator,
            #menu_item{
                id = ?MENU_View_ConcurrentHistory,
                text = "Concurrent History",
                help = ?HELP_View_ConcurrentHistory,
                kind = radio
            },
            #menu_item{
                id = ?MENU_View_FullHistory,
                text = "Full History",
                help = ?HELP_View_FullHistory,
                kind = radio
            },
            separator,
            %% TODO Show/hide
            #menu_item{
                id = ?MENU_View_Mailbox,
                text = "Show &Mailbox",
                help = ?HELP_View_Mailbox,
                kind = check
            },
            separator,
            #menu_item{
                id = ?MENU_View_ProcessMessages,
                text = "Current Process Messages",
                help = ?HELP_View_ProcessMessages,
                kind = radio
            },
            #menu_item{
                id = ?MENU_View_AllMessages,
                text = "All Messages",
                help = ?HELP_View_AllMessages,
                kind = radio
            },
            separator,
            #menu_item{
                id = ?MENU_View_StatusBar,
                text = "&Status Bar",
                help = ?HELP_View_StatusBar,
                kind = check
            }
        ]
    ),

    RunMenu = create_menu(
        [
            #menu_item{
                id = ?MENU_Run_Start,
                text = "St&art...",
                help = ?HELP_Run_Start,
                enabled = false
            },
            #menu_item{
                id = ?MENU_Run_Stop,
                text = "St&op",
                help = ?HELP_Run_Stop,
                enabled = false
            }
        ]
    ),

    HelpMenu = create_menu(
        [
            #menu_item{
                id = ?MENU_Help_ViewHelp,
                text = "View &Help"
            },
            separator,
            #menu_item{
                id = ?MENU_Help_About,
                text = "&About " ?APP_NAME
            }
        ]
    ),

    wxMenuBar:append(MenuBar, FileMenu, "&File"),
    wxMenuBar:append(MenuBar, EditMenu, "&Edit"),
    wxMenuBar:append(MenuBar, ViewMenu, "&View"),
    wxMenuBar:append(MenuBar, RunMenu, "&Run"),
    wxMenuBar:append(MenuBar, HelpMenu, "&Help"),

    wxFrame:setMenuBar(Frame, MenuBar),

    MenuBar.

%%------------------------------------------------------------------------------
%% @doc Updates the menu according to the given new state, by comparing it with
%% the given old state.

-spec update(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update(#wx_state{module = Module, system = System}, #wx_state{module = Module, system = System}) ->
    ok;
update(_, #wx_state{module = undefined}) ->
    Frame = cauder_wx:find(?FRAME, wxFrame),
    MenuBar = wxFrame:getMenuBar(Frame),
    wxMenuBar:enable(MenuBar, ?MENU_Run_Start, false),
    wxMenuBar:enable(MenuBar, ?MENU_Run_Stop, false);
update(_, #wx_state{system = System}) ->
    Frame = cauder_wx:find(?FRAME, wxFrame),
    MenuBar = wxFrame:getMenuBar(Frame),
    wxMenuBar:enable(MenuBar, ?MENU_Run_Start, System =:= undefined),
    wxMenuBar:enable(MenuBar, ?MENU_Run_Stop, System =/= undefined).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec create_menu([ItemDescription]) -> Menu when
    ItemDescription :: menuItemDesc(),
    Menu :: wxMenu:wxMenu().

create_menu(Items) ->
    Menu = wxMenu:new(),
    lists:foreach(fun(Item) -> wxMenu:append(Menu, create_item(Item)) end, Items),
    Menu.

-spec create_item(ItemDescription) -> MenuItem when
    ItemDescription :: menuItemDesc(),
    MenuItem :: wxMenuItem:wxMenuItem().

create_item(#menu_item{id = Id, text = Text, help = Help, kind = normal, enabled = Enabled}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}, {help, Help}, {kind, ?wxITEM_NORMAL}]),
    wxMenuItem:enable(Item, [{enable, Enabled}]),
    Item;
create_item(#menu_item{id = Id, text = Text, help = Help, kind = check, enabled = Enabled}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}, {help, Help}, {kind, ?wxITEM_CHECK}]),
    wxMenuItem:enable(Item, [{enable, Enabled}]),
    Item;
create_item(#menu_item{id = Id, text = Text, help = Help, kind = radio, enabled = Enabled}) ->
    Item = wxMenuItem:new([{id, Id}, {text, Text}, {help, Help}, {kind, ?wxITEM_RADIO}]),
    wxMenuItem:enable(Item, [{enable, Enabled}]),
    Item;
create_item(separator) ->
    wxMenuItem:new([{kind, ?wxITEM_SEPARATOR}]).
