-module(cauder_wx_statusbar).

%% API
-export([create/1, update_position/2, update_process_count/2, set_visibility/1]).
%% Predefined statuses
-export([no_process/0, no_match/0]).
-export([load_start/1, load_finish/2]).
-export([init_start/0, init_finish/1]).
-export([stop_finish/0]).
% Manual
-export([step_start/1, step_finish/3]).
-export([step_over_finish/3, step_into_finish/3, step_multiple_finish/3]).
% Replay
-export([replay_steps_start/0, replay_steps_finish/2]).
-export([replay_spawn_start/1, replay_spawn_finish/2, replay_spawn_fail/0]).
-export([replay_send_start/1, replay_send_finish/2, replay_send_fail/0]).
-export([replay_receive_start/1, replay_receive_finish/2, replay_receive_fail/0]).
-export([replay_full_log_start/0, replay_full_log_finish/1]).
% Rollback
-export([rollback_steps_start/0, rollback_steps_finish/2]).
-export([rollback_spawn_start/1, rollback_spawn_finish/2, rollback_spawn_fail/0]).
-export([rollback_send_start/1, rollback_send_finish/2, rollback_send_fail/0]).
-export([rollback_receive_start/1, rollback_receive_finish/2, rollback_receive_fail/0]).
-export([rollback_variable_start/1, rollback_variable_finish/2, rollback_variable_fail/0]).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include("cauder_wx_statusbar.hrl").


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
  wxStatusBar:setStatusText(StatusBar, " System not started", [{number, 2}]),
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

-spec update_process_count(OldSystem, NewSystem) -> ok when
  OldSystem :: cauder_types:system() | undefined,
  NewSystem :: cauder_types:system() | undefined.

update_process_count(#sys{procs = PDict}, #sys{procs = PDict}) -> ok;
update_process_count(_, #sys{procs = PDict}) ->
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
update_process_count(_, undefined) ->
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


no_process() -> update_text(?NO_PROCESS).


no_match() -> update_text(?NO_MATCH).


%%%=============================================================================


-spec load_start(File) -> ok when
  File :: file:filename().

load_start(File) ->
  Status = io_lib:format(?LOAD_START, [File]),
  update_text(Status).


-spec load_finish(Module, Time) -> ok when
  Module :: module(),
  Time :: non_neg_integer().

load_finish(Module, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?LOAD_FINISH, [Module, TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec init_start() -> ok.

init_start() -> update_text(?INIT_START).


-spec init_finish(Time) -> ok when
  Time :: non_neg_integer().

init_finish(Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?INIT_FINISH, [TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec stop_finish() -> ok.

stop_finish() -> update_text(?STOP_FINISH).


%%%=============================================================================


-spec step_start(Semantics) -> ok when
  Semantics :: cauder_types:semantics().

step_start(Sem) ->
  SemStr = semantics_to_string(Sem),
  Status = io_lib:format(?STEP_START, [SemStr]),
  update_text(Status).


-spec step_finish(Semantics, Rule, Time) -> ok when
  Semantics :: cauder_types:semantics(),
  Rule :: cauder_types:rule(),
  Time :: non_neg_integer().

step_finish(Sem, Rule, Time) ->
  SemStr = semantics_to_string(Sem),
  RuleStr = rule_to_string(Rule),
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?STEP_FINISH, [SemStr, RuleStr, TimeStr]),
  update_text(Status).


-spec step_over_finish(Semantics, Steps, Time) -> ok when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  Time :: non_neg_integer().

step_over_finish(Sem, Steps, Time) ->
  SemStr = semantics_to_string(Sem),
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?STEP_OVER_FINISH, [Steps, SemStr, TimeStr]),
  update_text(Status).


-spec step_into_finish(Semantics, Steps, Time) -> ok when
  Semantics :: cauder_types:semantics(),
  Steps :: pos_integer(),
  Time :: non_neg_integer().

step_into_finish(Sem, Steps, Time) ->
  SemStr = semantics_to_string(Sem),
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?STEP_INTO_FINISH, [Steps, SemStr, TimeStr]),
  update_text(Status).


-spec step_multiple_finish(Semantics, {StepsDone, StepsTotal}, Time) -> ok when
  Semantics :: cauder_types:semantics(),
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer(),
  Time :: non_neg_integer().

step_multiple_finish(Sem, {Done, Total}, Time) ->
  SemStr = semantics_to_string(Sem),
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?STEP_MULTIPLE_FINISH, [Done, Total, SemStr, TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec replay_steps_start() -> ok.

replay_steps_start() -> update_text(?REPLAY_STEPS_START).


-spec replay_steps_finish({StepsDone, StepsTotal}, Time) -> ok when
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer(),
  Time :: non_neg_integer().

replay_steps_finish({Done, Total}, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?REPLAY_STEPS_FINISH, [Done, Total, TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec replay_spawn_start(Pid) -> ok when
  Pid :: cauder_types:proc_id().

replay_spawn_start(Pid) -> update_text(io_lib:format(?REPLAY_SPAWN_START, [Pid])).


-spec replay_spawn_finish(Pid, Time) -> ok when
  Pid :: cauder_types:proc_id(),
  Time :: non_neg_integer().

replay_spawn_finish(Pid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?REPLAY_SPAWN_FINISH, [Pid, TimeStr]),
  update_text(Status).


-spec replay_spawn_fail() -> ok.

replay_spawn_fail() -> update_text(?REPLAY_SPAWN_FAIL).


%%%=============================================================================


-spec replay_send_start(Uid) -> ok when
  Uid :: cauder_types:msg_id().

replay_send_start(Uid) -> update_text(io_lib:format(?REPLAY_SEND_START, [Uid])).


-spec replay_send_finish(Uid, Time) -> ok when
  Uid :: cauder_types:msg_id(),
  Time :: non_neg_integer().

replay_send_finish(Uid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?REPLAY_SEND_FINISH, [Uid, TimeStr]),
  update_text(Status).


-spec replay_send_fail() -> ok.

replay_send_fail() -> update_text(?REPLAY_SEND_FAIL).


%%%=============================================================================


-spec replay_receive_start(Uid) -> ok when
  Uid :: cauder_types:msg_id().

replay_receive_start(Uid) -> update_text(io_lib:format(?REPLAY_RECEIVE_START, [Uid])).


-spec replay_receive_finish(Uid, Time) -> ok when
  Uid :: cauder_types:msg_id(),
  Time :: non_neg_integer().

replay_receive_finish(Uid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?REPLAY_RECEIVE_FINISH, [Uid, TimeStr]),
  update_text(Status).


-spec replay_receive_fail() -> ok.

replay_receive_fail() -> update_text(?REPLAY_RECEIVE_FAIL).


%%%=============================================================================


-spec replay_full_log_start() -> ok.

replay_full_log_start() -> update_text(?REPLAY_FULL_LOG_START).


-spec replay_full_log_finish(Time) -> ok when
  Time :: non_neg_integer().

replay_full_log_finish(Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?REPLAY_FULL_LOG_FINISH, [TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec rollback_steps_start() -> ok.

rollback_steps_start() -> update_text(?ROLLBACK_STEPS_START).


-spec rollback_steps_finish({StepsDone, StepsTotal}, Time) -> ok when
  StepsDone :: non_neg_integer(),
  StepsTotal :: pos_integer(),
  Time :: non_neg_integer().

rollback_steps_finish({Done, Total}, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?ROLLBACK_STEPS_FINISH, [Done, Total, TimeStr]),
  update_text(Status).


%%%=============================================================================


-spec rollback_spawn_start(Pid) -> ok when
  Pid :: cauder_types:proc_id().

rollback_spawn_start(Pid) -> update_text(io_lib:format(?ROLLBACK_SPAWN_START, [Pid])).


-spec rollback_spawn_finish(Pid, Time) -> ok when
  Pid :: cauder_types:proc_id(),
  Time :: non_neg_integer().

rollback_spawn_finish(Pid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?ROLLBACK_SPAWN_FINISH, [Pid, TimeStr]),
  update_text(Status).


-spec rollback_spawn_fail() -> ok.

rollback_spawn_fail() -> update_text(?ROLLBACK_SPAWN_FAIL).


%%%=============================================================================


-spec rollback_send_start(Uid) -> ok when
  Uid :: cauder_types:msg_id().

rollback_send_start(Uid) -> update_text(io_lib:format(?ROLLBACK_SEND_START, [Uid])).


-spec rollback_send_finish(Uid, Time) -> ok when
  Uid :: cauder_types:msg_id(),
  Time :: non_neg_integer().

rollback_send_finish(Uid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?ROLLBACK_SEND_FINISH, [Uid, TimeStr]),
  update_text(Status).


-spec rollback_send_fail() -> ok.

rollback_send_fail() -> update_text(?ROLLBACK_SEND_FAIL).


%%%=============================================================================


-spec rollback_receive_start(Uid) -> ok when
  Uid :: cauder_types:msg_id().

rollback_receive_start(Uid) -> update_text(io_lib:format(?ROLLBACK_RECEIVE_START, [Uid])).


-spec rollback_receive_finish(Uid, Time) -> ok when
  Uid :: cauder_types:msg_id(),
  Time :: non_neg_integer().

rollback_receive_finish(Uid, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?ROLLBACK_RECEIVE_FINISH, [Uid, TimeStr]),
  update_text(Status).


-spec rollback_receive_fail() -> ok.

rollback_receive_fail() -> update_text(?ROLLBACK_RECEIVE_FAIL).


%%%=============================================================================


-spec rollback_variable_start(Name) -> ok when
  Name :: atom().

rollback_variable_start(Name) -> update_text(io_lib:format(?ROLLBACK_VARIABLE_START, [Name])).


-spec rollback_variable_finish(Name, Time) -> ok when
  Name :: atom(),
  Time :: non_neg_integer().

rollback_variable_finish(Name, Time) ->
  TimeStr = time_to_string(Time),
  Status = io_lib:format(?ROLLBACK_VARIABLE_FINISH, [Name, TimeStr]),
  update_text(Status).


-spec rollback_variable_fail() -> ok.

rollback_variable_fail() -> update_text(?ROLLBACK_VARIABLE_FAIL).


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


-spec time_to_string(Time) -> String when
  Time :: non_neg_integer(),
  String :: string().

time_to_string(Time) when Time < 1000      -> "<1 ms";
time_to_string(Time) when Time < 1000 * 60 -> io_lib:format("~b ms", [Time div 1000]);
time_to_string(Time)                       -> io_lib:format("~b s ~s ms", [Time div 1000 * 60, Time div 1000]).
