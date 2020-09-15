-module(cauder_wx_code).

-export([code_area/1, load_code/2, unload_code/1, mark_line/3,
         can_zoom_in/1, can_zoom_out/1, zoom_in/1, zoom_out/1, zoom_reset/1, update_margin/1, update_zoom_buttons/1, update_expr/1]).


-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").


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

-define(LINE_MARKER, 0).
-define(LINE_BACKGROUND, 1).


-spec code_area(Parent) -> Panel when
  Parent :: wxWindow:wxWindow(),
  Panel :: wxPanel:wxPanel().

code_area(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Code"}]),
  wxWindow:setSizer(Win, Sizer),

  FontCode = wxFont:new(?FONT_SIZE_ACTUAL_DEFAULT, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL),
  CodeArea = wxStyledTextCtrl:new(Win, [{id, ?CODE_TEXT}]),
  ref_add(?CODE_TEXT, CodeArea),
  wxStaticBoxSizer:add(Sizer, CodeArea, [{proportion, 1}, {flag, ?wxEXPAND}]),

  %% Styles
  wxStyledTextCtrl:styleClearAll(CodeArea),
  wxStyledTextCtrl:styleSetFont(CodeArea, ?wxSTC_STYLE_DEFAULT, FontCode),
  wxStyledTextCtrl:styleSetFont(CodeArea, ?wxSTC_STYLE_LINENUMBER, FontCode),

  lists:foreach(
    fun({Style, Color}) ->
      wxStyledTextCtrl:styleSetFont(CodeArea, Style, FontCode),
      wxStyledTextCtrl:styleSetForeground(CodeArea, Style, Color)
    end, ?STYLES),

  wxStyledTextCtrl:setLexer(CodeArea, ?wxSTC_LEX_ERLANG),
  wxStyledTextCtrl:setKeyWords(CodeArea, 0, keyWords()),
  wxStyledTextCtrl:setMarginType(CodeArea, 0, ?wxSTC_MARGIN_NUMBER),
  wxStyledTextCtrl:setSelectionMode(CodeArea, ?wxSTC_SEL_LINES),

  %% Current Line
  wxStyledTextCtrl:markerDefine(CodeArea, ?LINE_MARKER, ?wxSTC_MARK_ARROW, [{foreground, {20, 170, 20}}]),
  wxStyledTextCtrl:markerDefine(CodeArea, ?LINE_MARKER, ?wxSTC_MARK_ARROW, [{background, {200, 255, 200}}]),
  wxStyledTextCtrl:markerDefine(CodeArea, ?LINE_BACKGROUND, ?wxSTC_MARK_BACKGROUND, [{background, {200, 255, 200}}]),

  %% Scrolling
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
  wxStyledTextCtrl:setYCaretPolicy(CodeArea, Policy, 3),
  wxStyledTextCtrl:setVisiblePolicy(CodeArea, Policy, 3),

  wxStyledTextCtrl:setReadOnly(CodeArea, true),

  wxStyledTextCtrl:setZoom(CodeArea, ?ZOOM_DEFAULT),

  wxStyledTextCtrl:connect(CodeArea, 'stc_zoom'),

  % -----

  wxStaticBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

  % -----

  ExprArea = wxTextCtrl:new(Win, ?EXPR_TEXT, [{style, ?wxTE_READONLY}]),
  ref_add(?EXPR_TEXT, ExprArea),
  wxStaticBoxSizer:add(Sizer, ExprArea, [{proportion, 0}, {flag, ?wxEXPAND}]),

  FontExpr = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(ExprArea, FontExpr),

  % -----

  Win.


keyWords() -> lists:flatten(lists:join($\s, ?KEYWORDS), [0]).


-spec load_code(This, Code) -> 'ok' when
  This :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Code :: binary().

load_code(This, Code) ->
  wxStyledTextCtrl:freeze(This),
  wxStyledTextCtrl:setReadOnly(This, false),
  wxStyledTextCtrl:setTextRaw(This, Code),
  update_margin(This),
  wxStyledTextCtrl:setReadOnly(This, true),
  wxStyledTextCtrl:thaw(This).


-spec unload_code(This) -> 'ok' when
  This :: wxStyledTextCtrl:wxStyledTextCtrl().

unload_code(This) ->
  wxStyledTextCtrl:freeze(This),
  wxStyledTextCtrl:setReadOnly(This, false),
  wxStyledTextCtrl:setTextRaw(This, <<0:8>>),
  wxStyledTextCtrl:setMarginWidth(This, 0, 0),
  wxStyledTextCtrl:setReadOnly(This, true),
  wxStyledTextCtrl:thaw(This).


-spec mark_line(This, Prev, Line) -> 'ok' when
  This :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Prev :: non_neg_integer(),
  Line :: non_neg_integer().

mark_line(This, Prev, Line) ->
  wxStyledTextCtrl:freeze(This),
  goto_line(This, Line),
  wxStyledTextCtrl:markerDelete(This, Prev - 1, ?LINE_MARKER),
  wxStyledTextCtrl:markerDelete(This, Prev - 1, ?LINE_BACKGROUND),
  wxStyledTextCtrl:markerAdd(This, Line - 1, ?LINE_MARKER),
  wxStyledTextCtrl:markerAdd(This, Line - 1, ?LINE_BACKGROUND),
  wxStyledTextCtrl:thaw(This).


-spec goto_line(This, Line) -> 'ok' | ignore when
  This :: wxStyledTextCtrl:wxStyledTextCtrl(),
  Line :: non_neg_integer().

goto_line(_This, 0)   -> ignore;
goto_line(This, Line) -> wxStyledTextCtrl:gotoLine(This, Line - 1).


can_zoom_in(This) -> wxStyledTextCtrl:getZoom(This) < ?ZOOM_MAX.


can_zoom_out(This) -> wxStyledTextCtrl:getZoom(This) > ?ZOOM_MIN.


zoom_in(This) ->
  wxStyledTextCtrl:zoomIn(This),
  update_margin(This),
  update_zoom_buttons(This).


zoom_out(This) ->
  wxStyledTextCtrl:zoomOut(This),
  update_margin(This),
  update_zoom_buttons(This).


zoom_reset(This) ->
  wxStyledTextCtrl:setZoom(This, ?ZOOM_DEFAULT),
  update_margin(This),
  update_zoom_buttons(This).


update_margin(This) ->
  Lines = wxStyledTextCtrl:getLineCount(This),
  Digits = trunc(math:log10(Lines)) + 1,
  Width = wxStyledTextCtrl:textWidth(This, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Digits, $0)),
  wxStyledTextCtrl:setMarginWidth(This, 0, Width + 5).


update_zoom_buttons(This) ->
  wxMenuItem:enable(ref_lookup(?BUTTON_ZOOM_IN), [{enable, cauder_wx_code:can_zoom_in(This)}]),
  wxMenuItem:enable(ref_lookup(?BUTTON_ZOOM_OUT), [{enable, cauder_wx_code:can_zoom_out(This)}]).


update_expr(Expr) ->
  ExprText = ref_lookup(?EXPR_TEXT),
  wxTextCtrl:setValue(ExprText, pretty_print:expression(Expr)).


%% ===== Utils ===== %%


ref_add(Id, Ref) -> cauder_wx:ref_add(Id, Ref).
ref_lookup(Id) -> cauder_wx:ref_lookup(Id).
