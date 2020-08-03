-module(utils_gui).

%% Status check functions
-export([is_app_loaded/0, is_app_running/0]).

%% Button related functions
-export([enable_control_if/2, enable_replay/0, enable_roll/0,
         disable_control/1, disable_controls/1, disable_all_buttons/0,
         button_to_option/1, option_to_button_label/1, set_button_label_from/2, disable_all_inputs/0]).

%% Font size functions
-export([can_zoom_in/1, can_zoom_out/1, prev_font_size/1, next_font_size/1]).

%% Misc.
-export([clear_texts/0, sort_opts/1, toggle_opts/0, pp_marked_text/2]).

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

-export([update_process_choices/1, get_selected_process/0, update_code_line/0]).


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
      ?SINGLE_FORWARD_BUTTON -> ?FWD_SEM;
      ?SINGLE_BACKWARD_BUTTON -> ?BWD_SEM
    end,
  #opt{sem = Sem, rule = Rule}.


-spec option_to_button_label(cauder_types:option()) -> {integer(), string()}.

option_to_button_label(Option) ->
  Label = get_label_from_option(Option),
  Button =
    case Option#opt.sem of
      ?FWD_SEM -> ?SINGLE_FORWARD_BUTTON;
      ?BWD_SEM -> ?SINGLE_BACKWARD_BUTTON
    end,
  {Button, Label}.


-spec enable_control(ControlId :: integer()) -> ok.

enable_control(Id) ->
  wxWindow:enable(ref_lookup(Id)),
  ok.


-spec disable_control(ControlId :: integer()) -> ok.

disable_control(Id) ->
  wxWindow:disable(ref_lookup(Id)),
  ok.


-spec enable_controls(ControlIds :: [integer()]) -> ok.

enable_controls(Ids) -> wx:foreach(fun enable_control/1, Ids).


-spec disable_controls(ControlIds :: [integer()]) -> ok.

disable_controls(Ids) -> wx:foreach(fun disable_control/1, Ids).


-spec disable_all_inputs() -> ok.

disable_all_inputs() -> disable_controls(?ALL_INPUTS).


-spec disable_all_buttons() -> ok.

disable_all_buttons() -> disable_controls(?ALL_BUTTONS).


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


-spec enable_control_if(ControlId :: integer(), Condition :: boolean()) -> ok.

enable_control_if(Id, true) ->
  wxWindow:enable(ref_lookup(Id)),
  ok;
enable_control_if(Id, false) ->
  wxWindow:disable(ref_lookup(Id)),
  ok.


-spec enable_replay() -> ok.

enable_replay() ->
  enable_controls(?REPLAY_INPUTS),
  enable_controls(?REPLAY_BUTTONS),
  ok.


-spec enable_roll() -> ok.

enable_roll() ->
  enable_controls(?ROLL_INPUTS),
  enable_controls(?ROLL_BUTTONS),
  ok.

%%set_choices(Choices) ->
%%  FunChoice = ref_lookup(?FUN_CHOICE),
%%  wxChoice:clear(FunChoice),
%%  [wxChoice:append(FunChoice, Choice) || Choice <- Choices],
%%  wxChoice:setSelection(FunChoice, 0).

clear_texts() ->
  Batch =
    fun() ->
      wxListBox:clear(ref_lookup(?TRACE_LIST)),
      wxListBox:clear(ref_lookup(?ROLL_LOG_LIST)),
      wxListCtrl:deleteAllItems(ref_lookup(?BINDINGS_LIST)),
      wxListBox:clear(ref_lookup(?STACK_LIST)),
      wxTextCtrl:clear(ref_lookup(?LOG_TEXT)),
      wxTextCtrl:clear(ref_lookup(?HISTORY_TEXT))
    end,

  wx:batch(Batch).


get_selected_process() ->
  Choice = ref_lookup(?PROC_CHOICE),
  case wxChoice:getSelection(Choice) of
    ?wxNOT_FOUND -> none;
    Idx ->
      #sys{procs = ProcDict} = ref_lookup(?SYSTEM),
      {_, Proc} = lists:nth(Idx + 1, ProcDict),
      Proc
  end.


update_process_choices(#sys{procs = []}) ->
  Choice = ref_lookup(?PROC_CHOICE),
  wxChoice:freeze(Choice),
  wxChoice:disable(Choice),
  wxChoice:clear(Choice),
  wxChoice:thaw(Choice);
update_process_choices(#sys{procs = ProcDict}) ->
  Choice = ref_lookup(?PROC_CHOICE),
  wxChoice:freeze(Choice),
  wxChoice:enable(Choice),
  PrevSel = wxChoice:getStringSelection(Choice),
  wxChoice:clear(Choice),
  Procs = lists:map(fun({_, Proc}) -> Proc end, ProcDict),
  Values = lists:map(fun pretty_print:process/1, Procs),
  wxChoice:appendStrings(Choice, Values),
  case lists:member(PrevSel, Values) of
    true -> wxChoice:setStringSelection(Choice, PrevSel);
    false -> wxChoice:setSelection(Choice, 0)
  end,
  wxChoice:thaw(Choice),
  ok.


update_code_line() ->
  case get_selected_process() of
    none -> ok;
    #proc{exprs = [E | _]} ->
      Line = element(2, E),
      Prev = get(line),
      cauder_wx_code:mark_line(ref_lookup(?CODE_TEXT), Prev, Line),
      put(line, Line),
      ok
  end
.


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


ref_lookup(Id) -> cauder_gui:ref_lookup(Id).
