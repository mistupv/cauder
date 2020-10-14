-module(cauder_wx_menu).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, enable/2]).


-record(menu_item, {
  id :: integer(),
  text :: string(),
  help = "" :: string(),
  kind = normal :: normal | check | radio,
  enabled = true :: boolean()
}).

-type menuItemDesc() :: #menu_item{} | 'separator'.


-spec create(Frame :: wxFrame:wxFrame()) -> wxMenuBar:wxMenuBar().

create(Frame) ->
  MenuBar = wxMenuBar:new(),

  FileMenu = create_menu(
    [
      #menu_item{id = ?MENU_File_Open, text = "&Open...\tCtrl+O", help = ?HELP_File_Open},
      separator,
      #menu_item{id = ?MENU_File_Exit, text = "E&xit", help = ?HELP_File_Exit}
    ]
  ),

  EditMenu = create_menu(
    [
      #menu_item{id = ?MENU_Edit_Undo, text = "&Undo\tCtrl+Z", enabled = false},
      #menu_item{id = ?MENU_Edit_Redo, text = "&Redo\tCtrl+Y", enabled = false}
    ]
  ),

  ViewMenu = create_menu(
    [
      #menu_item{id = ?MENU_View_ZoomIn, text = "Zoom &In\tCtrl++", help = ?HELP_View_ZoomIn, enabled = false},
      #menu_item{id = ?MENU_View_ZoomOut, text = "Zoom &Out\tCtrl+-", help = ?HELP_View_ZoomOut, enabled = false},
      #menu_item{id = ?MENU_View_Zoom100, text = "Zoo&m 100%", help = ?HELP_View_Zoom100},
      separator,
      #menu_item{id = ?MENU_View_Mailbox, text = "Show &Mailbox", help = ?HELP_View_Mailbox, kind = check},
      #menu_item{id = ?MENU_View_History, text = "Show &History", help = ?HELP_View_History, kind = check},
      #menu_item{id = ?MENU_View_Log, text = "Show &Log", help = ?HELP_View_Log, kind = check},
      #menu_item{id = ?MENU_View_Stack, text = "Show S&tack", help = ?HELP_View_Stack, kind = check},
      #menu_item{id = ?MENU_View_Environment, text = "Show &Environment", help = ?HELP_View_Environment, kind = check},
      #menu_item{id = ?MENU_View_CurrentExpression, text = "Show Current E&xpression", help = ?HELP_View_CurrentExpression, kind = check},
      separator,
      #menu_item{id = ?MENU_View_ConcurrentHistory, text = "Concurrent History", help = ?HELP_View_ConcurrentHistory, kind = radio},
      #menu_item{id = ?MENU_View_FullHistory, text = "Full History", help = ?HELP_View_FullHistory, kind = radio},
      separator,
      #menu_item{id = ?MENU_View_RelevantEnvironment, text = "Relevant Environment", help = ?HELP_View_RelevantEnvironment, kind = radio},
      #menu_item{id = ?MENU_View_FullEnvironment, text = "Full Environment", help = ?HELP_View_FullEnvironment, kind = radio},
      separator,
      #menu_item{id = ?MENU_View_StatusBar, text = "&Status Bar", help = ?HELP_View_StatusBar, kind = check}
    ]
  ),

  RunMenu = create_menu(
    [
      #menu_item{id = ?MENU_Run_Start, text = "St&art...", help = ?HELP_Run_Start, enabled = false},
      #menu_item{id = ?MENU_Run_Stop, text = "St&op", help = ?HELP_Run_Stop, enabled = false}
    ]
  ),

  HelpMenu = create_menu(
    [
      #menu_item{id = ?MENU_Help_ViewHelp, text = "View &Help"},
      separator,
      #menu_item{id = ?MENU_Help_About, text = "&About " ?APPNAME}
    ]
  ),

  wxMenuBar:append(MenuBar, FileMenu, "&File"),
  wxMenuBar:append(MenuBar, EditMenu, "&Edit"),
  wxMenuBar:append(MenuBar, ViewMenu, "&View"),
  wxMenuBar:append(MenuBar, RunMenu, "&Run"),
  wxMenuBar:append(MenuBar, HelpMenu, "&Help"),

  wxFrame:setMenuBar(Frame, MenuBar),

  MenuBar.

-spec create_menu(list(menuItemDesc())) -> wxMenu:wxMenu().

create_menu(Items) ->
  Menu = wxMenu:new(),
  lists:foreach(fun(Item) -> wxMenu:append(Menu, create_item(Item)) end, Items),
  Menu.

-spec create_item(menuItemDesc()) -> wxMenuItem:wxMenuItem().

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


-spec enable(ItemId :: integer(), Enable :: boolean()) -> 'ok'.

enable(ItemId, Enable) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  MenuBar = wxFrame:getMenuBar(Frame),
  wxMenuBar:enable(MenuBar, ItemId, Enable).
