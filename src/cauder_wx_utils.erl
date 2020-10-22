%%%-------------------------------------------------------------------
%%% @doc CauDEr GUI utilities.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_wx_utils).

-export([find/2]).
-export([pp_marked_text/2]).

-include_lib("wx/include/wx.hrl").


%%------------------------------------------------------------------------------
%% @doc Find the first window with the given `Id' and casts it to the given
%% `Type'.

-spec find(Id, Type) -> Window when
  Id :: integer(),
  Type :: atom(),
  Window :: wx:wx_object().

find(Id, Type) -> wx:typeCast(wxWindow:findWindowById(Id), Type).


-spec marked(TextControl, TextList, StringAcc) -> ok when
  TextControl :: wxTextCtrl:wxTextCtrl(),
  TextList :: [char() | {wx:wx_colour(), string()}],
  StringAcc :: string().

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


-spec pp_marked_text(TextControl, TextList) -> ok when
  TextControl :: wxTextCtrl:wxTextCtrl(),
  TextList :: [char() | {wx:wx_colour(), string()}].

pp_marked_text(Ctrl, TextList) ->
  % Freeze control when inserting text
  wxTextCtrl:freeze(Ctrl),
  wxTextCtrl:clear(Ctrl),
  marked(Ctrl, TextList, ""),
  % Put scroll back at the top
  wxTextCtrl:setInsertionPoint(Ctrl, 0),
  % Unfreeze control
  wxTextCtrl:thaw(Ctrl).
