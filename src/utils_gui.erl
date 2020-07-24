-module(utils_gui).

%% Status check functions
-export([is_app_loaded/0, is_app_running/0]).

%% Button related functions
-export([enable_button_if/2, enable_replay_buttons/0, enable_roll_buttons/0,
         disable_button/1, disable_buttons/1, disable_all_buttons/0,
         button_to_option/1, option_to_button_label/1, set_button_label_from/2]).

%% Font size functions
-export([can_zoom_in/1, can_zoom_out/1, prev_font_size/1, next_font_size/1]).

%% Misc.
-export([set_choices/1, clear_texts/0, sort_opts/1, toggle_opts/0, pp_marked_text/2]).

%% ----- Status text update functions ----- %%
-export([update_status_text/1]).
%% Manual evaluation functions
-export([sttext_single/1]).
%% Automatic evaluation functions
-export([sttext_mult/2, sttext_norm/1]).
%% Replay evaluation functions
-export([sttext_replay/2, sttext_replay_send/2, sttext_replay_spawn/2, sttext_replay_rec/2]).
%% Rollback evaluation functions
-export([sttext_roll/2, sttext_roll_send/2, sttext_roll_spawn/2, sttext_roll_rec/2, sttext_roll_var/2]).
-export([sttext_comp/0]).

%% ETS functions
-export([stop_refs/0]).


-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


is_app_loaded() ->
  Status = ref_lookup(?STATUS),
  Status#status.loaded.

is_app_running() ->
  Status = ref_lookup(?STATUS),
  Status#status.running.


-spec get_label_from_option(cauder_types:option()) -> string().

get_label_from_option(#opt{rule = ?RULE_SEQ})     -> "Seq";
get_label_from_option(#opt{rule = ?RULE_SELF})    -> "Self";
get_label_from_option(#opt{rule = ?RULE_SPAWN})   -> "Spawn";
get_label_from_option(#opt{rule = ?RULE_SEND})    -> "Send";
get_label_from_option(#opt{rule = ?RULE_RECEIVE}) -> "Receive".


-spec get_rule_from_button(ButtonId :: integer()) -> ?RULE_SEQ | ?RULE_SEND | ?RULE_RECEIVE | ?RULE_SPAWN | ?RULE_SELF.

get_rule_from_button(ButtonId) ->
  Label = wxButton:getLabel(ref_lookup(ButtonId)),
  case Label of
    "Seq" -> ?RULE_SEQ;
    "Send" -> ?RULE_SEND;
    "Receive" -> ?RULE_RECEIVE;
    "Spawn" -> ?RULE_SPAWN;
    "Self" -> ?RULE_SELF
  end.


-spec button_to_option(ButtonId :: integer()) -> cauder_types:option().

button_to_option(ButtonId) ->
  Rule = get_rule_from_button(ButtonId),
  Sem =
  case ButtonId of
    ?FWD_INT_BUTTON -> ?FWD_SEM;
    ?BWD_INT_BUTTON -> ?BWD_SEM
  end,
  #opt{sem = Sem, rule = Rule}.


-spec option_to_button_label(cauder_types:option()) -> {integer(), string()}.

option_to_button_label(Option) ->
  Label = get_label_from_option(Option),
  Button =
  case Option#opt.sem of
    ?FWD_SEM -> ?FWD_INT_BUTTON;
    ?BWD_SEM -> ?BWD_INT_BUTTON
  end,
  {Button, Label}.


-spec disable_button(ButtonId :: integer()) -> ok.

disable_button(Id) ->
  wxButton:disable(ref_lookup(Id)),
  ok.


-spec disable_buttons(ButtonIds :: [integer()]) -> ok.

disable_buttons(Ids) -> lists:foreach(fun disable_button/1, Ids).


-spec disable_all_buttons() -> ok.

disable_all_buttons() -> disable_buttons(?ALL_BUTTONS).


-spec set_button_label_from(ButtonId :: integer(), ButtonLabels :: [{integer(), string()}]) -> ok.

set_button_label_from(Id, ButtonLabels) ->
  Button = ref_lookup(Id),
  case lists:keyfind(Id, 1, ButtonLabels) of
    false ->
      wxButton:disable(Button),
      wxButton:setLabel(Button, "Seq");
    {Id, Label} ->
      wxButton:enable(Button),
      wxButton:setLabel(Button, Label)
  end.


-spec enable_button_if(ButtonId :: integer(), Condition :: boolean()) -> ok.

enable_button_if(Id, true) ->
  wxButton:enable(ref_lookup(Id)),
  ok;
enable_button_if(Id, false) ->
  wxButton:disable(ref_lookup(Id)),
  ok.


-spec enable_replay_buttons() -> ok.

enable_replay_buttons() ->
  wxButton:enable(ref_lookup(?REPLAY_BUTTON)),
  wxButton:enable(ref_lookup(?REPLAY_SPAWN_BUTTON)),
  wxButton:enable(ref_lookup(?REPLAY_SEND_BUTTON)),
  wxButton:enable(ref_lookup(?REPLAY_REC_BUTTON)),
  ok.


-spec enable_roll_buttons() -> ok.

enable_roll_buttons() ->
  wxButton:enable(ref_lookup(?ROLL_BUTTON)),
  wxButton:enable(ref_lookup(?ROLL_SPAWN_BUTTON)),
  wxButton:enable(ref_lookup(?ROLL_SEND_BUTTON)),
  wxButton:enable(ref_lookup(?ROLL_REC_BUTTON)),
  wxButton:enable(ref_lookup(?ROLL_VAR_BUTTON)),
  ok.

set_choices(Choices) ->
  FunChoice = ref_lookup(?FUN_CHOICE),
  wxChoice:clear(FunChoice),
  [wxChoice:append(FunChoice, Choice) || Choice <- Choices],
  wxChoice:setSelection(FunChoice, 0).

clear_texts() ->
  StateText = ref_lookup(?STATE_TEXT),
  TraceText = ref_lookup(?TRACE_TEXT),
  RollLogText = ref_lookup(?ROLL_LOG_TEXT),
  wxTextCtrl:clear(StateText),
  wxTextCtrl:clear(TraceText),
  wxTextCtrl:clear(RollLogText).

stop_refs() ->
  case is_app_loaded() of
    true -> cauder:stop_refs();
    false -> ok
  end.

sttext_single(Button) ->
  Option = button_to_option(Button),
  #opt{sem = Sem} = Option,
  SemStr =
  case Sem of
    ?FWD_SEM -> " forward ";
    ?BWD_SEM -> " backward "
  end,
  LabelStr = get_label_from_option(Option),
  FullStr = "Fired" ++ SemStr ++ LabelStr ++ " rule",
  update_status_text(FullStr).

sttext_norm(Steps) ->
  StepsStr = integer_to_list(Steps),
  FullStr = StepsStr ++ " steps done",
  update_status_text(FullStr).

sttext_mult(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps done",
  update_status_text(FullStr).


%% ==================== Rollback status text ==================== %%


sttext_replay(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps replayed",
  update_status_text(FullStr).

sttext_replay_send(false, _) -> update_status_text("Could not replay the sending of that message");
sttext_replay_send(true, Id) -> update_status_text("Replayed sending of message with id " ++ Id).

sttext_replay_spawn(false, _) -> update_status_text("Could not replay the spawning of that process");
sttext_replay_spawn(true, Id) -> update_status_text("Replayed spawning of process with Pid " ++ Id).

sttext_replay_rec(false, _) -> update_status_text("Could not replay the receiving of that message");
sttext_replay_rec(true, Id) -> update_status_text("Replayed receiving of message with id " ++ Id).


%% ==================== Rollback status text ==================== %%


sttext_roll(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps rolled back",
  update_status_text(FullStr).

sttext_roll_send(false, _) -> update_status_text("Could not roll back the sending of that message");
sttext_roll_send(true, Id) -> update_status_text("Rolled back sending of message with id " ++ Id).

sttext_roll_spawn(false, _) -> update_status_text("Could not roll back the spawning of that process");
sttext_roll_spawn(true, Id) -> update_status_text("Rolled back spawning of process with Pid " ++ Id).

sttext_roll_rec(false, _) -> update_status_text("Could not roll back the receiving of that message");
sttext_roll_rec(true, Id) -> update_status_text("Rolled back receiving of message with id " ++ Id).

sttext_roll_var(false, _) -> update_status_text("Could not roll back the binding of that variable");
sttext_roll_var(true, Id) -> update_status_text("Rolled back binding of variable " ++ Id).



sttext_comp() ->
  FullStr = "Compiler options have changed, open file again to take effect",
  update_status_text(FullStr).

update_status_text(String) ->
  Frame = ref_lookup(?FRAME),
  wxFrame:setStatusText(Frame, String).

index_of(Elem, List) -> index_of(Elem, List, 1).

index_of(_, [], _)                -> not_found;
index_of(Elem, [Elem | _], Index) -> Index;
index_of(Elem, [_ | Rest], Index) -> index_of(Elem, Rest, Index + 1).

prev_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  case SizeIdx == 1 of
    true -> CurSize;
    false -> lists:nth(SizeIdx - 1, ?FONT_SIZES)
  end.

next_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  SizeLen = length(?FONT_SIZES),
  case SizeIdx == SizeLen of
    true -> CurSize;
    false -> lists:nth(SizeIdx + 1, ?FONT_SIZES)
  end.

can_zoom_in(Size) -> index_of(Size, ?FONT_SIZES) < length(?FONT_SIZES).
can_zoom_out(Size) -> index_of(Size, ?FONT_SIZES) > 1.

sort_opts(Opts) ->
  SortOpts = lists:sort(fun(P1, P2) -> P1#opt.id < P2#opt.id end, Opts),
  SortOpts.


-spec toggle_opts() -> [{cauder_types:print_option(), boolean()}].

toggle_opts() ->
  MenuView = ref_lookup(?MENU_VIEW),
  MenuComp = ref_lookup(?MENU_COMP),
  [{?PRINT_MAIL, wxMenu:isChecked(MenuView, ?TOGGLE_MAIL)},
   {?PRINT_LOG, wxMenu:isChecked(MenuView, ?TOGGLE_LOG)},
   {?PRINT_HIST, wxMenu:isChecked(MenuView, ?TOGGLE_HIST)},
   {?PRINT_STACK, wxMenu:isChecked(MenuView, ?TOGGLE_STACK)},
   {?PRINT_ENV, wxMenu:isChecked(MenuView, ?TOGGLE_ENV)},
   {?PRINT_EXP, wxMenu:isChecked(MenuView, ?TOGGLE_EXP)},
   {?FULL_HIST, wxMenu:isChecked(MenuView, ?RADIO_FULL_HIST)},
   {?FULL_ENV, wxMenu:isChecked(MenuView, ?RADIO_FULL_ENV)},
   {?COMP_OPT, wxMenu:isChecked(MenuComp, ?TOGGLE_COMP)}].


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

ref_lookup(Id) ->
  ets:lookup_element(?GUI_REF, Id, 2).
