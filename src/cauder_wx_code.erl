-module(cauder_wx_code).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/2]).
-export([load_code/2, unload_code/1, mark_line/3, zoom_in/1, zoom_out/1, zoom_reset/1, update_margin/1, update_buttons/2]).

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


-spec create(Parent :: wxWindow:wxWindow()) -> wxWindow:wxWindow().

create(Parent) ->
  Win = wxPanel:new(Parent),

  Sizer = wxStaticBoxSizer:new(?wxVERTICAL, Win, [{label, "Code"}]),
  wxWindow:setSizer(Win, Sizer),

  % -----

  % TODO Add tabs to allow for multiple open files

  CodeCtrl = wxStyledTextCtrl:new(Win, [{id, ?CODE_TEXT}]),
  wxStaticBoxSizer:add(Sizer, CodeCtrl, [{proportion, 1}, {flag, ?wxEXPAND}]),

  FontCode = wxFont:new(?FONT_SIZE_ACTUAL_DEFAULT, ?wxFONTFAMILY_TELETYPE, ?wxNORMAL, ?wxNORMAL),

  %% Styles
  wxStyledTextCtrl:styleClearAll(CodeCtrl),
  wxStyledTextCtrl:styleSetFont(CodeCtrl, ?wxSTC_STYLE_DEFAULT, FontCode),
  wxStyledTextCtrl:styleSetFont(CodeCtrl, ?wxSTC_STYLE_LINENUMBER, FontCode),

  lists:foreach(
    fun({Style, Color}) ->
      wxStyledTextCtrl:styleSetFont(CodeCtrl, Style, FontCode),
      wxStyledTextCtrl:styleSetForeground(CodeCtrl, Style, Color)
    end, ?STYLES),

  wxStyledTextCtrl:setLexer(CodeCtrl, ?wxSTC_LEX_ERLANG),
  wxStyledTextCtrl:setKeyWords(CodeCtrl, 0, keyWords()),
  wxStyledTextCtrl:setMarginType(CodeCtrl, 0, ?wxSTC_MARGIN_NUMBER),
  wxStyledTextCtrl:setSelectionMode(CodeCtrl, ?wxSTC_SEL_LINES),

  %% Current Line
  wxStyledTextCtrl:markerDefine(CodeCtrl, ?LINE_MARKER, ?wxSTC_MARK_ARROW, [{foreground, {20, 170, 20}}]),
  wxStyledTextCtrl:markerDefine(CodeCtrl, ?LINE_MARKER, ?wxSTC_MARK_ARROW, [{background, {200, 255, 200}}]),
  wxStyledTextCtrl:markerDefine(CodeCtrl, ?LINE_BACKGROUND, ?wxSTC_MARK_BACKGROUND, [{background, {200, 255, 200}}]),

  %% Scrolling
  Policy = ?wxSTC_CARET_SLOP bor ?wxSTC_CARET_JUMPS bor ?wxSTC_CARET_EVEN,
  wxStyledTextCtrl:setYCaretPolicy(CodeCtrl, Policy, 3),
  wxStyledTextCtrl:setVisiblePolicy(CodeCtrl, Policy, 3),

  wxStyledTextCtrl:setReadOnly(CodeCtrl, true),
  wxStyledTextCtrl:setZoom(CodeCtrl, ?ZOOM_DEFAULT),
  wxStyledTextCtrl:connect(CodeCtrl, 'stc_zoom'),

  % -----

  wxStaticBoxSizer:addSpacer(Sizer, ?SPACER_MEDIUM),

  % -----

  ExprCtrl = wxTextCtrl:new(Win, ?EXPR_TEXT, [{style, ?wxTE_READONLY}]),
  wxStaticBoxSizer:add(Sizer, ExprCtrl, [{proportion, 0}, {flag, ?wxEXPAND}]),

  FontExpr = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),
  wxTextCtrl:setFont(ExprCtrl, FontExpr),

  % -----

  Win.


-spec update(System, Pid) -> ok when
  System :: cauder_types:system() | 'undefined',
  Pid :: cauder_types:proc_id() | 'none'.

update(undefined, _) -> ok; % TODO
update(#sys{}, none) -> ok; % TODO
update(#sys{procs = PDict}, Pid) ->
  {ok, #proc{exprs = [E | _]}} = orddict:find(Pid, PDict),
  Line = element(2, E),
  Prev = get(line),
  CodeCtrl = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  mark_line(CodeCtrl, Prev, Line),
  put(line, Line),
  ExprCtrl = utils_gui:find(?EXPR_TEXT, wxTextCtrl),
  Expr = pretty_print:expression(E),
  wxTextCtrl:setValue(ExprCtrl, Expr).



keyWords() -> lists:flatten(lists:join($\s, ?KEYWORDS), [0]).


-spec load_code(wxStyledTextCtrl:wxStyledTextCtrl(), Code :: binary()) -> 'ok'.

load_code(CodeCtrl, Code) ->
  wxStyledTextCtrl:freeze(CodeCtrl),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, false),
  wxStyledTextCtrl:setTextRaw(CodeCtrl, Code),
  update_margin(CodeCtrl),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, true),
  wxStyledTextCtrl:thaw(CodeCtrl).


-spec unload_code(wxStyledTextCtrl:wxStyledTextCtrl()) -> 'ok'.

unload_code(CodeCtrl) ->
  wxStyledTextCtrl:freeze(CodeCtrl),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, false),
  wxStyledTextCtrl:setTextRaw(CodeCtrl, <<0:8>>),
  wxStyledTextCtrl:setMarginWidth(CodeCtrl, 0, 0),
  wxStyledTextCtrl:setReadOnly(CodeCtrl, true),
  wxStyledTextCtrl:thaw(CodeCtrl).


-spec mark_line(wxStyledTextCtrl:wxStyledTextCtrl(), Prev :: non_neg_integer(), Line :: non_neg_integer()) -> 'ok'.

mark_line(CodeCtrl, Prev, Line) ->
  wxStyledTextCtrl:freeze(CodeCtrl),
  wxStyledTextCtrl:markerDelete(CodeCtrl, Prev - 1, ?LINE_MARKER),
  wxStyledTextCtrl:markerDelete(CodeCtrl, Prev - 1, ?LINE_BACKGROUND),
  goto_line(CodeCtrl, Line),
  wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?LINE_MARKER),
  wxStyledTextCtrl:markerAdd(CodeCtrl, Line - 1, ?LINE_BACKGROUND),
  wxStyledTextCtrl:thaw(CodeCtrl).


-spec goto_line(wxStyledTextCtrl:wxStyledTextCtrl(), Line :: non_neg_integer()) -> 'ok' | ignore.

goto_line(_CodeCtrl, 0)   -> ignore;
goto_line(CodeCtrl, Line) -> wxStyledTextCtrl:gotoLine(CodeCtrl, Line - 1).


-spec zoom_in(wxStyledTextCtrl:wxStyledTextCtrl()) -> ok.
-spec zoom_out(wxStyledTextCtrl:wxStyledTextCtrl()) -> ok.
-spec zoom_reset(wxStyledTextCtrl:wxStyledTextCtrl()) -> ok.

zoom_in(CodeCtrl) -> wxStyledTextCtrl:zoomIn(CodeCtrl).
zoom_out(CodeCtrl) -> wxStyledTextCtrl:zoomOut(CodeCtrl).
zoom_reset(CodeCtrl) -> wxStyledTextCtrl:setZoom(CodeCtrl, ?ZOOM_DEFAULT).


-spec update_margin(wxStyledTextCtrl:wxStyledTextCtrl()) -> ok.

update_margin(CodeCtrl) ->
  Lines = wxStyledTextCtrl:getLineCount(CodeCtrl),
  Digits = trunc(math:log10(Lines)) + 1,
  Width = wxStyledTextCtrl:textWidth(CodeCtrl, ?wxSTC_STYLE_LINENUMBER, lists:duplicate(Digits, $0)),
  wxStyledTextCtrl:setMarginWidth(CodeCtrl, 0, Width + 5).


-spec update_buttons(wxStyledTextCtrl:wxStyledTextCtrl(), wxMenuBar:wxMenuBar()) -> ok.

update_buttons(CodeCtrl, Menubar) ->
  wxMenuBar:enable(Menubar, ?MENU_View_ZoomIn, can_zoom_in(CodeCtrl)),
  wxMenuBar:enable(Menubar, ?MENU_View_ZoomOut, can_zoom_out(CodeCtrl)).


-spec can_zoom_in(wxStyledTextCtrl:wxStyledTextCtrl()) -> boolean().
-spec can_zoom_out(wxStyledTextCtrl:wxStyledTextCtrl()) -> boolean().

can_zoom_in(CodeCtrl) -> wxStyledTextCtrl:getZoom(CodeCtrl) < ?ZOOM_MAX.
can_zoom_out(CodeCtrl) -> wxStyledTextCtrl:getZoom(CodeCtrl) > ?ZOOM_MIN.
