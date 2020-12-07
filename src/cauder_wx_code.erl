-module(cauder_wx_code).

%% API
-export([create/1, update/2]).
% TODO Remove exports, do all work inside update/2
-export([load_code/2, zoom_in/1, zoom_out/1, zoom_reset/1, update_margin/1, update_buttons/2]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

-define(KEYWORDS, ["after", "begin", "case", "try", "cond", "catch", "andalso", "orelse",
                   "end", "fun", "if", "let", "of", "receive", "when", "bnot", "not",
                   "div", "rem", "band", "and", "bor", "bxor", "bsl", "bsr", "or", "xor"]).

%% For wx-2.9 usage
-ifndef(wxSTC_ERLANG_COMMENT_FUNCTION).
-define(wxSTC_ERLANG_COMMENT_FUNCTION, 14).
-define(wxSTC_ERLANG_COMMENT_MODULE, 15).
-define(wxSTC_ERLANG_COMMENT_DOC, 16).
-define(wxSTC_ERLANG_COMMENT_DOC_MACRO, 17).
-define(wxSTC_ERLANG_ATOM_QUOTED, 18).
-define(wxSTC_ERLANG_MACRO_QUOTED, 19).
-define(wxSTC_ERLANG_RECORD_QUOTED, 20).
-define(wxSTC_ERLANG_NODE_NAME_QUOTED, 21).
-define(wxSTC_ERLANG_BIFS, 22).
-define(wxSTC_ERLANG_MODULES, 23).
-define(wxSTC_ERLANG_MODULES_ATT, 24).
-endif.

-define(STYLES, [{?wxSTC_ERLANG_DEFAULT, {0, 0, 0}},
                 {?wxSTC_ERLANG_COMMENT, {160, 53, 35}},
                 {?wxSTC_ERLANG_VARIABLE, {150, 100, 40}},
                 {?wxSTC_ERLANG_NUMBER, {5, 5, 100}},
                 {?wxSTC_ERLANG_KEYWORD, {130, 40, 172}},
                 {?wxSTC_ERLANG_STRING, {170, 45, 132}},
                 {?wxSTC_ERLANG_OPERATOR, {30, 0, 0}},
                 {?wxSTC_ERLANG_ATOM, {0, 0, 0}},
                 {?wxSTC_ERLANG_FUNCTION_NAME, {64, 102, 244}},
                 {?wxSTC_ERLANG_CHARACTER, {236, 155, 172}},
                 {?wxSTC_ERLANG_MACRO, {40, 144, 170}},
                 {?wxSTC_ERLANG_RECORD, {40, 100, 20}},
                 {?wxSTC_ERLANG_SEPARATOR, {0, 0, 0}},
                 {?wxSTC_ERLANG_NODE_NAME, {0, 0, 0}},
                 %% Optional 2.9 stuff
                 {?wxSTC_ERLANG_COMMENT_FUNCTION, {160, 53, 35}},
                 {?wxSTC_ERLANG_COMMENT_MODULE, {160, 53, 35}},
                 {?wxSTC_ERLANG_COMMENT_DOC, {160, 53, 35}},
                 {?wxSTC_ERLANG_COMMENT_DOC_MACRO, {160, 53, 35}},
                 {?wxSTC_ERLANG_ATOM_QUOTED, {0, 0, 0}},
                 {?wxSTC_ERLANG_MACRO_QUOTED, {40, 144, 170}},
                 {?wxSTC_ERLANG_RECORD_QUOTED, {40, 100, 20}},
                 {?wxSTC_ERLANG_NODE_NAME_QUOTED, {0, 0, 0}},
                 {?wxSTC_ERLANG_BIFS, {130, 40, 172}},
                 {?wxSTC_ERLANG_MODULES, {64, 102, 244}},
                 {?wxSTC_ERLANG_MODULES_ATT, {64, 102, 244}}]).

-define(ACTIVE_LINE_MARKER, 0).
-define(ACTIVE_LINE_BACKGROUND, 1).
-define(SUSPEND_LINE_MARKER, 2).
-define(SUSPEND_LINE_BACKGROUND, 3).


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates the <i>code</i> panel and populates it.

-spec create(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Code"}]),
  wxWindow:setSizer(Win, Sizer),

  wxStaticBoxSizer:add(Sizer, create_code(Win), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxStaticBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),
  wxStaticBoxSizer:add(Sizer, create_expression(Win), [{proportion, 0}, {flag, ?wxEXPAND}]),

  Win.


%%------------------------------------------------------------------------------
%% @doc Updates the <i>code</i> panel according to the given new state, by
%% comparing it with the given old state.

-spec update(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update(OldState, NewState) ->
  update_code(OldState, NewState),
  update_expression(OldState, NewState).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec create_code(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_code(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  % TODO Add tabs to allow for multiple open files

  CodeControl = wxStyledTextCtrl:new(Win, [{id, ?CODE_Code_Control}]),
  wxBoxSizer:add(Sizer, CodeControl, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(?FONT_SIZE_ACTUAL_DEFAULT, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL),

  %% Styles
  wxStyledTextCtrl:styleClearAll(CodeControl),
  wxStyledTextCtrl:styleSetFont(CodeControl, ?wxSTC_STYLE_DEFAULT, Font),
  wxStyledTextCtrl:styleSetFont(CodeControl, ?wxSTC_STYLE_LINENUMBER, Font),

  lists:foreach(
    fun({Style, Color}) ->
      wxStyledTextCtrl:styleSetFont(CodeControl, Style, Font),
      wxStyledTextCtrl:styleSetForeground(CodeControl, Style, Color)
    end, ?STYLES),

  wxStyledTextCtrl:setLexer(CodeControl, ?wxSTC_LEX_ERLANG),
  wxStyledTextCtrl:setKeyWords(CodeControl, 0, keyWords()),
  wxStyledTextCtrl:setMarginType(CodeControl, 0, ?wxSTC_MARGIN_NUMBER),
  wxStyledTextCtrl:setSelectionMode(CodeControl, ?wxSTC_SEL_LINES),

  %% Line Markers
  wxStyledTextCtrl:markerDefine(CodeControl, ?ACTIVE_LINE_MARKER, ?wxSTC_MARK_ARROW, [{foreground, {20, 170, 20}}]),
  wxStyledTextCtrl:markerDefine(CodeControl, ?ACTIVE_LINE_MARKER, ?wxSTC_MARK_ARROW, [{background, {200, 255, 200}}]),
  wxStyledTextCtrl:markerDefine(CodeControl, ?ACTIVE_LINE_BACKGROUND, ?wxSTC_MARK_BACKGROUND, [{background, {200, 255, 200}}]),

  wxStyledTextCtrl:markerDefine(CodeControl, ?SUSPEND_LINE_MARKER, ?wxSTC_MARK_ARROW, [{foreground, {200, 200, 20}}]),
  wxStyledTextCtrl:markerDefine(CodeControl, ?SUSPEND_LINE_MARKER, ?wxSTC_MARK_ARROW, [{background, {255, 255, 140}}]),
  wxStyledTextCtrl:markerDefine(CodeControl, ?SUSPEND_LINE_BACKGROUND, ?wxSTC_MARK_BACKGROUND, [{background, {255, 255, 140}}]),

  %% Scrolling
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
  wxStyledTextCtrl:setYCaretPolicy(CodeControl, Policy, 3),
  wxStyledTextCtrl:setVisiblePolicy(CodeControl, Policy, 3),

  wxStyledTextCtrl:setReadOnly(CodeControl, true),
  wxStyledTextCtrl:setZoom(CodeControl, ?ZOOM_DEFAULT),
  wxStyledTextCtrl:connect(CodeControl, 'stc_zoom'),

  %% Drag and drop support
  wxPanel:dragAcceptFiles(Win, true),
  wxPanel:connect(Win, 'drop_files'),

  wxPanel:connect(Win, 'stc_updateui'),

  Win.


-spec update_code(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_code(#wx_state{system = System, pid = Pid}, #wx_state{system = System, pid = Pid}) ->
  ok;
update_code(_, #wx_state{system = undefined}) ->
  CodeControl = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  unmark_line(CodeControl),
  goto_line(CodeControl, 1),
  ok;
update_code(_, #wx_state{pid = undefined}) ->
  CodeControl = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  unmark_line(CodeControl),
  goto_line(CodeControl, 1),
  ok;
update_code(_, #wx_state{system = #sys{procs = PMap}, pid = Pid}) ->
  CodeControl = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  unmark_line(CodeControl),
  #proc{exprs = [E | _], suspend = SuspendInfo} = maps:get(Pid, PMap),
  Line = element(2, E),
  Mode =
    case SuspendInfo of
      undefined -> active;
      _ -> suspend
    end,
  mark_line(CodeControl, Line, Mode),
  goto_line(CodeControl, Line),
  ok.


%%%=============================================================================


-spec create_expression(Parent) -> Window when
  Parent :: wxWindow:wxWindow(),
  Window :: wxWindow:wxWindow().

create_expression(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(Win, Sizer),

  ExpressionControl = wxTextCtrl:new(Win, ?CODE_Expression_Control, [{style, ?wxTE_READONLY}]),
  wxStaticBoxSizer:add(Sizer, ExpressionControl, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(ExpressionControl, Font),

  Win.


-spec update_expression(OldState, NewState) -> ok when
  OldState :: cauder_wx:state(),
  NewState :: cauder_wx:state().

update_expression(#wx_state{system = System, pid = Pid}, #wx_state{system = System, pid = Pid}) ->
  ok;
update_expression(_, #wx_state{system = undefined}) ->
  wxTextCtrl:clear(cauder_wx:find(?CODE_Expression_Control, wxTextCtrl)),
  ok;
update_expression(_, #wx_state{pid = undefined}) ->
  wxTextCtrl:clear(cauder_wx:find(?CODE_Expression_Control, wxTextCtrl)),
  ok;
update_expression(_, #wx_state{system = #sys{procs = PMap}, pid = Pid}) ->
  ExpressionControl = cauder_wx:find(?CODE_Expression_Control, wxTextCtrl),
  #proc{exprs = [Expr | _]} = maps:get(Pid, PMap),
  StrExpr = cauder_pp:expression(Expr),
  wxTextCtrl:setValue(ExpressionControl, StrExpr),
  ok.


%%%=============================================================================


keyWords() -> lists:flatten(lists:join($\s, ?KEYWORDS), [0]).


-spec load_code(CodeControl, Code) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Code :: binary().

load_code(CodeCtrl, Code) ->
  wxStyledTextCtrl:freeze(CodeCtrl),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, false),
  wxStyledTextCtrl:setTextRaw(CodeCtrl, Code),
  update_margin(CodeCtrl),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, true),
  wxStyledTextCtrl:thaw(CodeCtrl).


-spec mark_line(CodeControl, Line, Mode) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Line :: pos_integer(),
  Mode :: active | suspend.

mark_line(CodeCtrl, Line, Mode) ->
  put(line, {Line, Mode}),
  case Mode of
    active ->
      wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?ACTIVE_LINE_MARKER),
      wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?ACTIVE_LINE_BACKGROUND);
    suspend ->
      wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?SUSPEND_LINE_MARKER),
      wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?SUSPEND_LINE_BACKGROUND)
  end,
  ok.


-spec unmark_line(CodeControl) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl().

unmark_line(CodeCtrl) ->
  case get(line) of
    undefined -> ok;
    {Line, active} ->
      wxStyledTextCtrl:markerDelete(CodeCtrl, Line - 1, ?ACTIVE_LINE_MARKER),
      wxStyledTextCtrl:markerDelete(CodeCtrl, Line - 1, ?ACTIVE_LINE_BACKGROUND);
    {Line, suspend} ->
      wxStyledTextCtrl:markerDelete(CodeCtrl, Line - 1, ?SUSPEND_LINE_MARKER),
      wxStyledTextCtrl:markerDelete(CodeCtrl, Line - 1, ?SUSPEND_LINE_BACKGROUND)
  end,
  erase(line),
  ok.


-spec goto_line(CodeControl, Line) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Line :: pos_integer().

goto_line(CodeCtrl, Line) ->
  wxStyledTextCtrl:gotoLine(CodeCtrl, Line - 1),
  ok.


-spec zoom_in(CodeControl) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl().

zoom_in(CodeCtrl) -> wxStyledTextCtrl:zoomIn(CodeCtrl).


-spec zoom_out(CodeControl) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl().

zoom_out(CodeCtrl) -> wxStyledTextCtrl:zoomOut(CodeCtrl).


-spec zoom_reset(CodeControl) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl().

zoom_reset(CodeCtrl) -> wxStyledTextCtrl:setZoom(CodeCtrl, ?ZOOM_DEFAULT).


-spec update_margin(CodeControl) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl().

update_margin(CodeCtrl) ->
  Lines = wxStyledTextCtrl:getLineCount(CodeCtrl),
  Digits = trunc(math:log10(Lines)) + 1,
  Width = wxStyledTextCtrl:textWidth(CodeCtrl, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Digits, $0)),
  wxStyledTextCtrl:setMarginWidth(CodeCtrl, 0, Width + 5).


-spec update_buttons(CodeControl, MenuBar) -> ok when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  MenuBar :: wxMenuBar:wxMenuBar().

update_buttons(CodeCtrl, Menubar) ->
  wxMenuBar:enable(Menubar, ?MENU_View_ZoomIn, can_zoom_in(CodeCtrl)),
  wxMenuBar:enable(Menubar, ?MENU_View_ZoomOut, can_zoom_out(CodeCtrl)).


-spec can_zoom_in(CodeControl) -> CanZoomIn when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  CanZoomIn :: boolean().

can_zoom_in(CodeCtrl) -> wxStyledTextCtrl:getZoom(CodeCtrl) < ?ZOOM_MAX.


-spec can_zoom_out(CodeControl) -> CanZoomOut when
  CodeControl :: wxStyledTextCtrl:wxStyledTextCtrl(),
  CanZoomOut :: boolean().

can_zoom_out(CodeCtrl) -> wxStyledTextCtrl:getZoom(CodeCtrl) > ?ZOOM_MIN.
