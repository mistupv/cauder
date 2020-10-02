-module(utils_gui).

%% Status check functions
-export([is_app_loaded/0, is_app_running/0]).

-export([find/2]).

%% Control related functions
-export([enable_control/1, enable_controls/1, enable_control_if/2, enable_controls_if/2, enable_replay/0, enable_roll/0,
         disable_control/1, disable_controls/1, disable_all_buttons/0, disable_all_inputs/0]).

%% Misc.
-export([clear_texts/0, sort_opts/1, pp_marked_text/2]).

%% ----- Status text update functions ----- %%
-export([update_status_text/1]).
%% Manual evaluation functions
-export([sttext_noproc/0, sttext_nomatch/0, sttext_reduce/2, sttext_step/2, sttext_step/3]).
%% Automatic evaluation functions
-export([sttext_mult/3]).
%% Replay evaluation functions
-export([sttext_replay/2, sttext_replay_send/2, sttext_replay_spawn/2, sttext_replay_rec/2]).
%% Rollback evaluation functions
-export([sttext_roll/2, sttext_roll_send/2, sttext_roll_spawn/2, sttext_roll_rec/2, sttext_roll_var/2]).
-export([sttext_comp/0]).

%% ETS functions
%%-export([stop_refs/0]).


-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").


is_app_loaded() ->
  Status = cauder:ref_lookup(?STATUS),
  Status#status.loaded.

is_app_running() ->
  Status = cauder:ref_lookup(?STATUS),
  Status#status.running.


-spec find(Id :: integer(), Type :: atom()) -> wx:wx_object().

find(Id, Type) -> wx:typeCast(wxWindow:findWindowById(Id), Type).


-spec rule_to_string(Rule) -> string() when
  Rule :: ?RULE_SEQ | ?RULE_SELF | ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE.

rule_to_string(?RULE_SEQ)     -> "Seq";
rule_to_string(?RULE_SELF)    -> "Self";
rule_to_string(?RULE_SPAWN)   -> "Spawn";
rule_to_string(?RULE_SEND)    -> "Send";
rule_to_string(?RULE_RECEIVE) -> "Receive".


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


-spec disable_all_inputs() -> ok.

disable_all_inputs() -> disable_controls(?ALL_INPUTS).


-spec disable_all_buttons() -> ok.

disable_all_buttons() -> disable_controls(?ALL_BUTTONS).


-spec enable_control_if(ControlId :: integer(), Condition :: boolean()) -> ok.

enable_control_if(Id, true)  -> enable_control(Id);
enable_control_if(Id, false) -> disable_control(Id).


-spec enable_controls_if(ControlIds :: [integer()], Condition :: boolean()) -> ok.

enable_controls_if(Ids, true)  -> enable_controls(Ids);
enable_controls_if(Ids, false) -> disable_controls(Ids).


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
      wxListBox:clear(find(?TRACE_LIST, wxListBox)),
      wxListBox:clear(find(?ROLL_LOG_LIST, wxListBox)),
      wxListCtrl:deleteAllItems(find(?BINDINGS_LIST, wxListCtrl)),
      wxListBox:clear(find(?STACK_LIST, wxListBox)),
      wxTextCtrl:clear(find(?LOG_TEXT, wxTextCtrl)),
      wxTextCtrl:clear(find(?HISTORY_TEXT, wxTextCtrl))
    end,

  wx:batch(Batch).


%%stop_refs() ->
%%  case is_app_loaded() of
%%    true -> cauder:stop_refs();
%%    false -> ok
%%  end.


semantics_to_string(?FWD_SEM) -> "forward";
semantics_to_string(?BWD_SEM) -> "backward".


sttext_noproc() -> update_status_text("Cannot perform any action because no process is selected.").

sttext_nomatch() -> update_status_text("Cannot perform any forward action because there is no matching message to be received.").


%% ==================== Manual actions ==================== %%


sttext_reduce(Sem, Rule) ->
  Status = io_lib:format("Performed a single ~s reduction step using rule: ~s.",
                         [semantics_to_string(Sem), rule_to_string(Rule)]),
  update_status_text(Status).

sttext_step(Sem, Steps) ->
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped over to the next expression.",
                         [Steps, semantics_to_string(Sem)]),
  update_status_text(Status).

sttext_step(Sem, Steps, {M, F, A}) ->
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped into the function '~s:~s/~b'.",
                         [Steps, semantics_to_string(Sem), M, F, A]),
  update_status_text(Status).


%% ==================== Automatic actions ==================== %%


sttext_mult(Sem, Done, Total) ->
  Dir = semantics_to_string(Sem),
  DoneStr = integer_to_list(Done),
  TotalStr = integer_to_list(Total),
  update_status_text("Performed " ++ DoneStr ++ " " ++ Dir ++ " steps, from a total of " ++ TotalStr ++ ".").


%% ==================== Rollback actions ==================== %%


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


%% ==================== Rollback actions ==================== %%


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
  Frame = find(?FRAME, wxFrame), % TODO
  wxFrame:setStatusText(Frame, String).


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
