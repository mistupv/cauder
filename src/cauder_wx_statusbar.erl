-module(cauder_wx_statusbar).


%% API
-export([create/1, set_visible/2]).


-spec create(Frame :: wxFrame:wxFrame()) -> wxStatusBar:wxStatusBar().

create(Frame) ->
  StatusBar = wxFrame:createStatusBar(Frame, [{number, 2}]),
  wxStatusBar:setFieldsCount(StatusBar, 2, [{widths, [-77, -23]}]),
  StatusBar.


-spec set_visible(StatusBar :: wxStatusBar:wxStatusBar(), boolean()) -> 'ok'.

set_visible(StatusBar, true) ->
  wxStatusBar:show(StatusBar),
  Frame = wxStatusBar:getParent(StatusBar),
  wxFrame:sendSizeEvent(Frame);

set_visible(StatusBar, false) ->
  wxStatusBar:hide(StatusBar),
  Frame = wxStatusBar:getParent(StatusBar),
  wxFrame:sendSizeEvent(Frame).
