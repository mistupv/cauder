-module(cauder_wx_statusbar).

%% API
-export([create/1, update_text/1, update_position/2, update_process_count/1, set_visibility/1]).
%% Predefined statuses
-export([no_process/0, no_match/0]).
-export([step/2, step_over/2, step_into/3]).
-export([step_multiple/3]).
-export([replay_steps/2, replay_spawn/2, replay_send/2, replay_receive/2]).
-export([rollback_steps/2, rollback_spawn/2, rollback_send/2, rollback_receive/2, rollback_variable/2]).

-include("cauder.hrl").
-include("cauder_wx.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Creates the status bar and populates it.

-spec create(Frame) -> StatusBar when
  Frame :: wxFrame:wxFrame(),
  StatusBar :: wxStatusBar:wxStatusBar().

create(Frame) ->
  StatusBar = wxFrame:createStatusBar(Frame),
  wxStatusBar:setFieldsCount(StatusBar, 3, [{widths, [-1, 100, 125]}]),
  StatusBar.


%%------------------------------------------------------------------------------
%% @doc Updates the text in the status bar.

-spec update_text(Text) -> ok when
  Text :: unicode:chardata().

update_text(Text) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  wxStatusBar:setStatusText(StatusBar, Text).


%%------------------------------------------------------------------------------
%% @doc Updates the position shown in the status bar.

-spec update_position(Line, Column) -> ok when
  Line :: pos_integer(),
  Column :: pos_integer().

update_position(Line, Column) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  Text = io_lib:format(" Ln ~b, Col ~b", [Line, Column]),
  wxStatusBar:setStatusText(StatusBar, Text, [{number, 1}]).


%%------------------------------------------------------------------------------
%% @doc Updates the position shown in the status bar.

-spec update_process_count(System) -> ok when
  System :: cauder_types:system() | undefined.

update_process_count(#sys{procs = PDict}) ->
  {Alive, Dead} =
    orddict:fold(
      fun(_, Proc, {Alive, Dead}) ->
        case cauder_utils:is_dead(Proc) of
          true -> {Alive, Dead + 1};
          false -> {Alive + 1, Dead}
        end
      end,
      {0, 0},
      PDict),
  Frame = cauder_wx:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  Text = io_lib:format(" Alive ~b, Dead ~b", [Alive, Dead]),
  wxStatusBar:setStatusText(StatusBar, Text, [{number, 2}]);

update_process_count(undefined) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  wxStatusBar:setStatusText(StatusBar, " System not started", [{number, 2}]).


%%------------------------------------------------------------------------------
%% @doc Shows/hides the status bar.

-spec set_visibility(Visible) -> ok when
  Visible :: boolean().

set_visibility(Visible) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  case Visible of
    true -> wxStatusBar:show(StatusBar);
    false -> wxStatusBar:hide(StatusBar)
  end,
  wxFrame:sendSizeEvent(Frame).


%%%=============================================================================
%%% Predefined statuses
%%%=============================================================================


no_process() -> update_text("Cannot perform any action because no process is selected.").


no_match() -> update_text("Cannot perform any forward action because there is no matching message to be received.").


%%%=============================================================================


-spec step(Semantics, Rule) -> ok when
  Semantics :: cauder_types:semantics(),
  Rule :: cauder_types:rule().

step(Sem, Rule) ->
  StrSem = semantics_to_string(Sem),
  StrRule = rule_to_string(Rule),
  Status = io_lib:format("Performed a single ~s reduction step using rule: ~s.", [StrSem, StrRule]),
  update_text(Status).


-spec step_over(Semantics, Steps) -> ok when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer().

step_over(Sem, Steps) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped over to the next expression.", [Steps, StrSem]),
  update_text(Status).


-spec step_into(Semantics, Steps, MFA) -> ok when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  MFA :: mfa().

step_into(Sem, Steps, {M, F, A}) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped into the definition of function: ~s:~s/~b.", [Steps, StrSem, M, F, A]),
  update_text(Status).


%%%=============================================================================


-spec step_multiple(Semantics, StepsDone, StepsTotal) -> ok when
  Semantics :: cauder_types:semantics(),
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer().

step_multiple(Sem, Done, Total) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s steps, from a total of ~b.", [Done, StrSem, Total]),
  update_text(Status).


%%%=============================================================================


-spec replay_steps(StepsDone, StepsTotal) -> ok when
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer().

replay_steps(Done, Total) -> update_text(io_lib:format("~b of ~b steps replayed.", [Done, Total])).


-spec replay_spawn(DidSucceed, Pid) -> ok when
  DidSucceed :: boolean(),
  Pid :: cauder_types:proc_id() | none.

replay_spawn(false, _)  -> update_text("Could not replay the spawning of that process");
replay_spawn(true, Pid) -> update_text(io_lib:format("Replayed spawning of process with PID: ~p", [Pid])).


-spec replay_send(DidSucceed, Uid) -> ok when
  DidSucceed :: boolean(),
  Uid :: cauder_types:msg_id() | none.

replay_send(false, _)  -> update_text("Could not replay the sending of that message");
replay_send(true, Uid) -> update_text(io_lib:format("Replayed sending of message with UID: ~p", [Uid])).


-spec replay_receive(DidSucceed, Uid) -> ok when
  DidSucceed :: boolean(),
  Uid :: cauder_types:msg_id() | none.

replay_receive(false, _)  -> update_text("Could not replay the receiving of that message");
replay_receive(true, Uid) -> update_text(io_lib:format("Replayed receiving of message with UID: ~p", [Uid])).


%%%=============================================================================


-spec rollback_steps(StepsDone, StepsTotal) -> ok when
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer().

rollback_steps(Done, Total) -> update_text(io_lib:format("~b of ~b steps rolled back.", [Done, Total])).


-spec rollback_spawn(DidSucceed, Pid) -> ok when
  DidSucceed :: boolean(),
  Pid :: cauder_types:proc_id() | none.

rollback_spawn(false, _)  -> update_text("Could not roll back the spawning of that process");
rollback_spawn(true, Pid) -> update_text(io_lib:format("Rolled back spawning of process with PID: ~p", [Pid])).


-spec rollback_send(DidSucceed, Uid) -> ok when
  DidSucceed :: boolean(),
  Uid :: cauder_types:msg_id() | none.

rollback_send(false, _)  -> update_text("Could not roll back the sending of that message");
rollback_send(true, Uid) -> update_text(io_lib:format("Rolled back sending of message with UID: ~p", [Uid])).


-spec rollback_receive(DidSucceed, Uid) -> ok when
  DidSucceed :: boolean(),
  Uid :: cauder_types:msg_id() | none.

rollback_receive(false, _)  -> update_text("Could not roll back the receiving of that message");
rollback_receive(true, Uid) -> update_text(io_lib:format("Rolled back receiving of message with UID: ~p", [Uid])).


-spec rollback_variable(DidSucceed, Name) -> ok when
  DidSucceed :: boolean(),
  Name :: atom() | none.

rollback_variable(false, _)   -> update_text("Could not roll back the binding of that variable");
rollback_variable(true, Name) -> update_text(io_lib:format("Rolled back binding of variable with name: ~p", [Name])).


%%%=============================================================================


-spec semantics_to_string(Semantics) -> String when
  Semantics :: cauder_types:semantics(),
  String :: string().

semantics_to_string(?FWD_SEM) -> "forward";
semantics_to_string(?BWD_SEM) -> "backward".


-spec rule_to_string(Rule) -> String when
  Rule :: cauder_types:rule(),
  String :: string().

rule_to_string(?RULE_SEQ)     -> "Seq";
rule_to_string(?RULE_SELF)    -> "Self";
rule_to_string(?RULE_SPAWN)   -> "Spawn";
rule_to_string(?RULE_SEND)    -> "Send";
rule_to_string(?RULE_RECEIVE) -> "Receive".
