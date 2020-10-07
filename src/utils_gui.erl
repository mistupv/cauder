-module(utils_gui).

-export([find/2]).

%% Control related functions
-export([enable_control/1, enable_controls/1, enable_control_if/2, enable_controls_if/2,
         disable_control/1, disable_controls/1]).

%% Misc.
-export([sort_opts/1, pp_marked_text/2]).


%% ETS functions
%%-export([stop_refs/0]).


-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").


-spec find(Id :: integer(), Type :: atom()) -> wx:wx_object().

find(Id, Type) -> wx:typeCast(wxWindow:findWindowById(Id), Type).


%% ==================== Control utils evaluation ==================== %%


-spec enable_control(ControlId :: integer()) -> ok.

enable_control(Id) ->
  wxWindow:enable(wxWindow:findWindowById(Id)),
  ok.


-spec disable_control(ControlId :: integer()) -> ok.

disable_control(Id) ->
  wxWindow:disable(wxWindow:findWindowById(Id)),
  ok.


-spec enable_controls(ControlIds :: [integer()]) -> ok.

enable_controls(Ids) -> wx:foreach(fun enable_control/1, Ids).


-spec disable_controls(ControlIds :: [integer()]) -> ok.

disable_controls(Ids) -> wx:foreach(fun disable_control/1, Ids).


-spec enable_control_if(ControlId :: integer(), Condition :: boolean()) -> ok.

enable_control_if(Id, true)  -> enable_control(Id);
enable_control_if(Id, false) -> disable_control(Id).


-spec enable_controls_if(ControlIds :: [integer()], Condition :: boolean()) -> ok.

enable_controls_if(Ids, true)  -> enable_controls(Ids);
enable_controls_if(Ids, false) -> disable_controls(Ids).


%%stop_refs() ->
%%  case is_app_loaded() of
%%    true -> cauder:stop_refs();
%%    false -> ok
%%  end.


sort_opts(Opts) -> lists:sort(fun(P1, P2) -> P1#opt.pid < P2#opt.pid end, Opts).


-spec marked(wxTextCtrl:wxTextCtrl(), [char() | {wx:wx_colour(), string()}], string()) -> ok.

marked(Ctrl, [], Acc) ->
  wxTextCtrl:setDefaultStyle(Ctrl, wxTextAttr:new(?wxBLACK)),
  wxTextCtrl:appendText(Ctrl, Acc);
marked(Ctrl, [{Attr, Text} | Rest], Acc) ->
  wxTextCtrl:setDefaultStyle(Ctrl, wxTextAttr:new(?wxBLACK)),
  wxTextCtrl:appendText(Ctrl, Acc),
  wxTextCtrl:setDefaultStyle(Ctrl, wxTextAttr:new(Attr)),
  wxTextCtrl:appendText(Ctrl, Text),
  marked(Ctrl, Rest, "");
marked(Ctrl, [Char | Rest], Acc) ->
  marked(Ctrl, Rest, Acc ++ [Char]).


-spec pp_marked_text(wxTextCtrl:wxTextCtrl(), [char() | {wx:wx_colour(), string()}]) -> ok.

pp_marked_text(Ctrl, TextList) ->
  % Freeze control when inserting text
  wxTextCtrl:freeze(Ctrl),
  wxTextCtrl:clear(Ctrl),
  marked(Ctrl, TextList, ""),
  % Put scroll back at the top
  wxTextCtrl:setInsertionPoint(Ctrl, 0),
  % Unfreeze control
  wxTextCtrl:thaw(Ctrl).
