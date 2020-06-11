-module(utils_gui).
-export([is_app_loaded/0, is_app_running/0,
         option_to_button_label/1, button_to_option/1,
         disable_rule_buttons/1, set_button_label_if/2, set_ref_button_if/2,
         set_choices/1, disable_all_buttons/0, enable_perm_buttons/0,
         clear_texts/0, stop_refs/0, update_status_text/1,
         sttext_single/1, sttext_mult/2, sttext_norm/1,
         sttext_roll/2, sttext_roll_send/2, sttext_roll_spawn/2,
         sttext_roll_rec/2, sttext_roll_var/2, sttext_comp/0,
         prev_font_size/1, next_font_size/1, sort_opts/1, toggle_opts/0,
         pp_marked_text/2, sched_opt/0]).

-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").

is_app_loaded() ->
  Status = ref_lookup(?STATUS),
  #status{loaded = LoadedStatus} = Status,
  case LoadedStatus of
    {true, _} -> true;
    _Other -> false
  end.

is_app_running() ->
  Status = ref_lookup(?STATUS),
  #status{running = RunningStatus} = Status,
  RunningStatus.

get_label_from_option(Option) ->
  case Option of
    #opt{rule = ?RULE_SEQ}     -> "Seq";
    #opt{rule = ?RULE_SEND}    -> "Send";
    #opt{rule = ?RULE_RECEIVE} -> "Receive";
    #opt{rule = ?RULE_SPAWN}   -> "Spawn";
    #opt{rule = ?RULE_SELF}    -> "Self";
    #opt{rule = ?RULE_SCHED}   -> ?NULL_LABEL
  end.

get_rule_from_button(Button) ->
  Label = wxButton:getLabel(ref_lookup(Button)),
  case Label of
     "Seq"     -> ?RULE_SEQ;
     "Send"    -> ?RULE_SEND;
     "Receive" -> ?RULE_RECEIVE;
     "Spawn"   -> ?RULE_SPAWN;
     "Self"    -> ?RULE_SELF
  end.

button_to_option(Button) ->
  case Button of
    ?FORW_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?FWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?FORW_SCH_BUTTON ->
      #opt{sem = ?FWD_SEM, type = ?TYPE_MSG, rule = ?RULE_SCHED};
    ?BACK_INT_BUTTON ->
      Rule = get_rule_from_button(Button),
      #opt{sem = ?BWD_SEM, type = ?TYPE_PROC, rule = Rule};
    ?BACK_SCH_BUTTON ->
      #opt{sem = ?BWD_SEM, type = ?TYPE_MSG, rule = ?RULE_SCHED}
  end.

option_to_button_label(Option) ->
  #opt{sem = Sem, type = Type} = Option,
  Label = get_label_from_option(Option),
  Button =
    case Sem of
      ?FWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?FORW_SCH_BUTTON;
          ?TYPE_PROC -> ?FORW_INT_BUTTON
        end;
      ?BWD_SEM ->
        case Type of
          ?TYPE_MSG  -> ?BACK_SCH_BUTTON;
          ?TYPE_PROC -> ?BACK_INT_BUTTON
        end
    end,
  {Button, Label}.

disable_rule_buttons(Buttons) ->
  [wxButton:disable(ref_lookup(Button)) || Button <- Buttons].

set_button_label_if(Button, EnabledButtonLabels) ->
  RefButton = ref_lookup(Button),
  case lists:keyfind(Button, 1, EnabledButtonLabels) of
    false ->
      wxButton:disable(RefButton),
      case Button of
        ?FORW_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        ?BACK_INT_BUTTON -> wxButton:setLabel(RefButton, "Seq");
        _Other -> ok
      end;
    {Button, Label} ->
      wxButton:enable(RefButton),
      case Label of
        ?NULL_LABEL -> ok;
        Label -> wxButton:setLabel(RefButton, Label)
      end
  end.

set_ref_button_if(Ref, Cond) ->
  RefButton = ref_lookup(Ref),
  case Cond of
    true ->
      wxButton:enable(RefButton);
    false ->
      wxButton:disable(RefButton)
  end.

disable_all_buttons() ->
  ForwIntButton   = ref_lookup(?FORW_INT_BUTTON),
  ForwSchButton   = ref_lookup(?FORW_SCH_BUTTON),
  BackIntButton   = ref_lookup(?BACK_INT_BUTTON),
  BackSchButton   = ref_lookup(?BACK_SCH_BUTTON),
  ForwardButton   = ref_lookup(?FORWARD_BUTTON),
  BackwardButton  = ref_lookup(?BACKWARD_BUTTON),
  NormalizeButton = ref_lookup(?NORMALIZE_BUTTON),
  RollButton      = ref_lookup(?ROLL_BUTTON),
  RollSpawnButton = ref_lookup(?ROLL_SPAWN_BUTTON),
  RollSendButton  = ref_lookup(?ROLL_SEND_BUTTON),
  RollRecButton   = ref_lookup(?ROLL_REC_BUTTON),
  RollVarButton   = ref_lookup(?ROLL_VAR_BUTTON),
  wxButton:disable(ForwIntButton),
  wxButton:disable(ForwSchButton),
  wxButton:disable(BackIntButton),
  wxButton:disable(BackSchButton),
  wxButton:disable(ForwardButton),
  wxButton:disable(BackwardButton),
  wxButton:disable(NormalizeButton),
  wxButton:disable(RollButton),
  wxButton:disable(RollSpawnButton),
  wxButton:disable(RollSendButton),
  wxButton:disable(RollRecButton),
  wxButton:disable(RollVarButton).

enable_perm_buttons() ->
  RollButton      = ref_lookup(?ROLL_BUTTON),
  RollSpawnButton = ref_lookup(?ROLL_SPAWN_BUTTON),
  RollSendButton  = ref_lookup(?ROLL_SEND_BUTTON),
  RollRecButton   = ref_lookup(?ROLL_REC_BUTTON),
  RollVarButton   = ref_lookup(?ROLL_VAR_BUTTON),
  wxButton:enable(RollButton),
  wxButton:enable(RollSpawnButton),
  wxButton:enable(RollSendButton),
  wxButton:enable(RollRecButton),
  wxButton:enable(RollVarButton).

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
  case is_app_running() of
    true ->
      cauder:stop_refs(),
      ok;
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
  Label = get_label_from_option(Option),
  LabelStr =
    case Label of
      ?NULL_LABEL -> "Sched";
      _Other -> Label
    end,
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

sttext_roll(StepsDone, Steps) ->
  StepsDoneStr = integer_to_list(StepsDone),
  StepsStr = integer_to_list(Steps),
  FullStr = StepsDoneStr ++ " of " ++ StepsStr ++ " steps rolled back",
  update_status_text(FullStr).

sttext_roll_send(false, _) ->
  FullStr = "Could not roll back the sending of that message",
  update_status_text(FullStr);
sttext_roll_send(true, Id) ->
  FullStr = "Rolled back sending of message with id " ++ Id,
  update_status_text(FullStr).

sttext_roll_spawn(false, _) ->
  FullStr = "Could not roll back the spawning of that process",
  update_status_text(FullStr);
sttext_roll_spawn(true, Id) ->
  FullStr = "Rolled back spawning of process with Pid " ++ Id,
  update_status_text(FullStr).

sttext_roll_rec(false, _) ->
  FullStr = "Could not roll back the receiving of that message",
  update_status_text(FullStr);
sttext_roll_rec(true, Id) ->
  FullStr = "Rolled back receiving of message with id " ++ Id,
  update_status_text(FullStr).

sttext_roll_var(false, _) ->
  FullStr = "Could not roll back the binding of that variable",
  update_status_text(FullStr);
sttext_roll_var(true, Id) ->
  FullStr = "Rolled back binding of variable " ++ Id,
  update_status_text(FullStr).

sttext_comp() ->
  FullStr = "Compiler options have changed, open file again to take effect",
  update_status_text(FullStr).

update_status_text(String) ->
  Frame = ref_lookup(?FRAME),
  wxFrame:setStatusText(Frame, String).

index_of(Elem, List) -> index_of(Elem, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Elem, [Elem|_], Index) -> Index;
index_of(Elem, [_|Rest], Index) -> index_of(Elem, Rest, Index + 1).

prev_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  case SizeIdx == 1 of
    true  -> CurSize;
    false -> lists:nth(SizeIdx - 1, ?FONT_SIZES)
  end.

next_font_size(CurSize) ->
  SizeIdx = index_of(CurSize, ?FONT_SIZES),
  SizeLen = length(?FONT_SIZES),
  case SizeIdx == SizeLen of
    true  -> CurSize;
    false -> lists:nth(SizeIdx + 1, ?FONT_SIZES)
  end.

sort_opts(Opts) ->
  SortOpts = lists:sort(fun(P1, P2) -> P1#opt.id < P2#opt.id end, Opts),
  SortOpts.


-spec toggle_opts() -> [{cauder_types:print_option(), boolean()}].

toggle_opts() ->
  MenuView = ref_lookup(?MENU_VIEW),
  MenuComp = ref_lookup(?MENU_COMP),
  [{?PRINT_MAIL,     wxMenu:isChecked(MenuView, ?TOGGLE_MAIL)},
   {?PRINT_HIST,     wxMenu:isChecked(MenuView, ?TOGGLE_HIST)},
   {?PRINT_ENV,      wxMenu:isChecked(MenuView, ?TOGGLE_ENV)},
   {?PRINT_EXP,      wxMenu:isChecked(MenuView, ?TOGGLE_EXP)},
   {?PRINT_FULL,     wxMenu:isChecked(MenuView, ?RADIO_FULL)},
   {?COMP_OPT,       wxMenu:isChecked(MenuComp, ?TOGGLE_COMP)},
   {?PRINT_FULL_ENV, wxMenu:isChecked(MenuView, ?RADIO_FULL_ENV)}].

sched_opt() ->
  MenuSched = ref_lookup(?MENU_SCHED),
  SchedOpts =
    [{wxMenu:isChecked(MenuSched, ?RADIO_RAND), ?SCHED_RANDOM},
     {wxMenu:isChecked(MenuSched, ?RADIO_PRIO), ?SCHED_PRIO_RANDOM}],
  proplists:get_value(true, SchedOpts).


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
