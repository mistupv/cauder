-module(cauder_wx_statusbar).

%% API
-export([create/1, update/2, update_position/2]).
%% Predefined statuses
-export([load_start/1, load_finish/2, load_fail/0]).
-export([init_start/0, init_manual_finish/1, init_replay_finish/1]).
-export([stop_finish/0]).
% Manual
-export([step_start/1, step_finish/3]).
-export([step_multiple_finish/3]).
% Replay
-export([replay_steps_start/0, replay_steps_finish/2]).
-export([replay_start_start/1, replay_start_finish/2]).
-export([replay_spawn_start/1, replay_spawn_finish/2, replay_spawn_fail/0]).
-export([replay_send_start/1, replay_send_finish/2, replay_send_fail/0]).
-export([replay_receive_start/1, replay_receive_finish/2, replay_receive_fail/0]).
-export([replay_register_start/1, replay_register_finish/2, replay_register_fail/0]).
-export([replay_delete_start/1, replay_delete_finish/2, replay_delete_fail/0]).
-export([replay_full_log_start/0, replay_full_log_finish/1]).
% Rollback
-export([rollback_steps_start/0, rollback_steps_finish/2]).
-export([rollback_start_begin/1, rollback_start_finish/2, rollback_start_fail/0]).
-export([rollback_spawn_start/1, rollback_spawn_finish/2, rollback_spawn_fail/0]).
-export([rollback_send_start/1, rollback_send_finish/2, rollback_send_fail/0]).
-export([rollback_receive_start/1, rollback_receive_finish/2, rollback_receive_fail/0]).
-export([rollback_variable_start/1, rollback_variable_finish/2, rollback_variable_fail/0]).
-export([rollback_reg_start/1, rollback_reg_finish/2, rollback_reg_fail/0]).
-export([rollback_del_start/1, rollback_del_finish/2, rollback_del_fail/0]).
%-export([rollback_senda_start/1, rollback_senda_finish/2, rollback_senda_fail/0]).

-elvis([{elvis_style, god_modules, disable}]).

-include("cauder_system.hrl").
-include("cauder_semantics.hrl").
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
%% @doc Updates the status bar according to the given new state, by comparing it
%% with the given old state.

-spec update(OldState, NewState) -> ok when
    OldState :: cauder_wx:state(),
    NewState :: cauder_wx:state().

update(
    #wx_state{system = #system{pool = Pool}, config = #config{status_bar = Show}},
    #wx_state{system = #system{pool = Pool}, config = #config{status_bar = Show}}
) ->
    ok;
update(_, #wx_state{config = #config{status_bar = false}}) ->
    set_visibility(false);
update(_, #wx_state{system = undefined}) ->
    set_visibility(true),

    StatusBar = wxFrame:getStatusBar(cauder_wx:find(?FRAME, wxFrame)),
    wxStatusBar:setStatusText(StatusBar, " System not started", [{number, 2}]);
update(_, #wx_state{system = #system{pool = Pool}}) ->
    set_visibility(true),

    {Alive, Dead} =
        lists:foldl(
            fun(Proc, {Alive, Dead}) ->
                case cauder_process:is_alive(Proc) of
                    true -> {Alive + 1, Dead};
                    false -> {Alive, Dead + 1}
                end
            end,
            {0, 0},
            cauder_pool:to_list(Pool)
        ),

    StatusBar = wxFrame:getStatusBar(cauder_wx:find(?FRAME, wxFrame)),
    Text = io_lib:format(" Alive ~b, Dead ~b", [Alive, Dead]),
    wxStatusBar:setStatusText(StatusBar, Text, [{number, 2}]).

%%------------------------------------------------------------------------------
%% @doc Updates the position shown in the status bar.

-spec update_position(Line, Column) -> ok when
    Line :: pos_integer(),
    Column :: pos_integer().

update_position(Line, Column) ->
    StatusBar = wxFrame:getStatusBar(cauder_wx:find(?FRAME, wxFrame)),
    Text = io_lib:format(" Ln ~b, Col ~b", [Line, Column]),
    wxStatusBar:setStatusText(StatusBar, Text, [{number, 1}]).

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

%no_process() -> set_text(?NO_PROCESS).

%no_match() -> set_text(?NO_MATCH).

%%%=============================================================================

-spec load_start(File) -> ok when
    File :: file:filename().

load_start(File) ->
    Status = io_lib:format(?LOAD_START, [File]),
    set_text(Status).

-spec load_finish(Module, Time) -> ok when
    Module :: module(),
    Time :: non_neg_integer().

load_finish(Module, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?LOAD_FINISH, [Module, TimeStr]),
    set_text(Status).

-spec load_fail() -> ok.

load_fail() -> set_text(?LOAD_FAIL).

%%%=============================================================================

-spec init_start() -> ok.

init_start() -> set_text(?INIT_START).

-spec init_manual_finish(Time) -> ok when
    Time :: non_neg_integer().

init_manual_finish(Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?INIT_MANUAL_FINISH, [TimeStr]),
    set_text(Status).

-spec init_replay_finish(Time) -> ok when
    Time :: non_neg_integer().

init_replay_finish(Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?INIT_REPLAY_FINISH, [TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec stop_finish() -> ok.

stop_finish() -> set_text(?STOP_FINISH).

%%%=============================================================================

-spec step_start(Semantics) -> ok when
    Semantics :: cauder_semantics:semantics().

step_start(Sem) ->
    SemStr = semantics_to_string(Sem),
    Status = io_lib:format(?STEP_START, [SemStr]),
    set_text(Status).

-spec step_finish(Semantics, {StepsDone, StepsTotal}, Time) -> ok when
    Semantics :: cauder_semantics:semantics(),
    StepsDone :: non_neg_integer(),
    StepsTotal :: pos_integer(),
    Time :: non_neg_integer().

step_finish(Sem, {Done, Total}, Time) ->
    SemStr = semantics_to_string(Sem),
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?STEP_FINISH, [Done, Total, SemStr, TimeStr]),
    set_text(Status).

%-spec step_suspend() -> ok.

%step_suspend() -> set_text(?STEP_SUSPEND).

-spec step_multiple_finish(Semantics, {StepsDone, StepsTotal}, Time) -> ok when
    Semantics :: cauder_semantics:semantics(),
    StepsDone :: non_neg_integer(),
    StepsTotal :: pos_integer(),
    Time :: non_neg_integer().

step_multiple_finish(Sem, {Done, Total}, Time) ->
    SemStr = semantics_to_string(Sem),
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?STEP_MULTIPLE_FINISH, [Done, Total, SemStr, TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec replay_steps_start() -> ok.

replay_steps_start() -> set_text(?REPLAY_STEPS_START).

-spec replay_steps_finish({StepsDone, StepsTotal}, Time) -> ok when
    StepsDone :: non_neg_integer(),
    StepsTotal :: pos_integer(),
    Time :: non_neg_integer().

replay_steps_finish({Done, Total}, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_STEPS_FINISH, [Done, Total, TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec replay_start_start(Node) -> ok when
    Node :: node().

replay_start_start(Node) -> set_text(io_lib:format(?REPLAY_START_START, [Node])).

-spec replay_start_finish(Node, Time) -> ok when
    Node :: node(),
    Time :: non_neg_integer().

replay_start_finish(Node, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_START_FINISH, [Node, TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec replay_spawn_start(Pid) -> ok when
    Pid :: cauder_process:id().

replay_spawn_start(Pid) -> set_text(io_lib:format(?REPLAY_SPAWN_START, [Pid])).

-spec replay_spawn_finish(Pid, Time) -> ok when
    Pid :: cauder_process:id(),
    Time :: non_neg_integer().

replay_spawn_finish(Pid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_SPAWN_FINISH, [Pid, TimeStr]),
    set_text(Status).

-spec replay_spawn_fail() -> ok.

replay_spawn_fail() -> set_text(?REPLAY_SPAWN_FAIL).

%%%=============================================================================

-spec replay_send_start(Uid) -> ok when
    Uid :: cauder_message:uid().

replay_send_start(Uid) -> set_text(io_lib:format(?REPLAY_SEND_START, [Uid])).

-spec replay_send_finish(Uid, Time) -> ok when
    Uid :: cauder_message:uid(),
    Time :: non_neg_integer().

replay_send_finish(Uid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_SEND_FINISH, [Uid, TimeStr]),
    set_text(Status).

-spec replay_send_fail() -> ok.

replay_send_fail() -> set_text(?REPLAY_SEND_FAIL).

%%%=============================================================================

-spec replay_receive_start(Uid) -> ok when
    Uid :: cauder_message:uid().

replay_receive_start(Uid) -> set_text(io_lib:format(?REPLAY_RECEIVE_START, [Uid])).

-spec replay_receive_finish(Uid, Time) -> ok when
    Uid :: cauder_message:uid(),
    Time :: non_neg_integer().

replay_receive_finish(Uid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_RECEIVE_FINISH, [Uid, TimeStr]),
    set_text(Status).

-spec replay_receive_fail() -> ok.

replay_receive_fail() -> set_text(?REPLAY_RECEIVE_FAIL).

%%%=============================================================================

-spec replay_register_start(Key) -> ok when
    Key :: cauder_map:key().

replay_register_start(Key) -> set_text(io_lib:format(?REPLAY_REGISTER_START, [Key])).

-spec replay_register_finish(Key, Time) -> ok when
    Key :: cauder_map:key(),
    Time :: non_neg_integer().

replay_register_finish(Key, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_REGISTER_FINISH, [Key, TimeStr]),
    set_text(Status).

-spec replay_register_fail() -> ok.

replay_register_fail() -> set_text(?REPLAY_REGISTER_FAIL).

%%%=============================================================================

-spec replay_delete_start(Key) -> ok when
    Key :: cauder_map:key().

replay_delete_start(Key) -> set_text(io_lib:format(?REPLAY_DELETE_START, [Key])).

-spec replay_delete_finish(Key, Time) -> ok when
    Key :: cauder_map:key(),
    Time :: non_neg_integer().

replay_delete_finish(Key, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_DELETE_FINISH, [Key, TimeStr]),
    set_text(Status).

-spec replay_delete_fail() -> ok.

replay_delete_fail() -> set_text(?REPLAY_DELETE_FAIL).

%%%=============================================================================

-spec replay_full_log_start() -> ok.

replay_full_log_start() -> set_text(?REPLAY_FULL_LOG_START).

-spec replay_full_log_finish(Time) -> ok when
    Time :: non_neg_integer().

replay_full_log_finish(Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?REPLAY_FULL_LOG_FINISH, [TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec rollback_steps_start() -> ok.

rollback_steps_start() -> set_text(?ROLLBACK_STEPS_START).

-spec rollback_steps_finish({StepsDone, StepsTotal}, Time) -> ok when
    StepsDone :: non_neg_integer(),
    StepsTotal :: pos_integer(),
    Time :: non_neg_integer().

rollback_steps_finish({Done, Total}, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_STEPS_FINISH, [Done, Total, TimeStr]),
    set_text(Status).

%%%=============================================================================

-spec rollback_start_begin(Node) -> ok when
    Node :: node().

rollback_start_begin(Node) -> set_text(io_lib:format(?ROLLBACK_START_BEGIN, [Node])).

-spec rollback_start_finish(Node, Time) -> ok when
    Node :: node(),
    Time :: non_neg_integer().

rollback_start_finish(Node, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_START_FINISH, [Node, TimeStr]),
    set_text(Status).

-spec rollback_start_fail() -> ok.

rollback_start_fail() -> set_text(?ROLLBACK_START_FAIL).

%%%=============================================================================

-spec rollback_spawn_start(Pid) -> ok when
    Pid :: cauder_process:id().

rollback_spawn_start(Pid) -> set_text(io_lib:format(?ROLLBACK_SPAWN_START, [Pid])).

-spec rollback_spawn_finish(Pid, Time) -> ok when
    Pid :: cauder_process:id(),
    Time :: non_neg_integer().

rollback_spawn_finish(Pid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_SPAWN_FINISH, [Pid, TimeStr]),
    set_text(Status).

-spec rollback_spawn_fail() -> ok.

rollback_spawn_fail() -> set_text(?ROLLBACK_SPAWN_FAIL).

%%%=============================================================================

-spec rollback_send_start(Uid) -> ok when
    Uid :: cauder_message:uid().

rollback_send_start(Uid) -> set_text(io_lib:format(?ROLLBACK_SEND_START, [Uid])).

-spec rollback_send_finish(Uid, Time) -> ok when
    Uid :: cauder_message:uid(),
    Time :: non_neg_integer().

rollback_send_finish(Uid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_SEND_FINISH, [Uid, TimeStr]),
    set_text(Status).

-spec rollback_send_fail() -> ok.

rollback_send_fail() -> set_text(?ROLLBACK_SEND_FAIL).

%%%=============================================================================

%-spec rollback_senda_start(Uid) -> ok when
%    Uid :: cauder_message:uid().

%rollback_senda_start(Uid) -> set_text(io_lib:format(?ROLLBACK_SENDA_START, [Uid])).

%-spec rollback_senda_finish(Uid, Time) -> ok when
%    Uid :: cauder_message:uid(),
%    Time :: non_neg_integer().

%rollback_senda_finish(Uid, Time) ->
%    TimeStr = time_to_string(Time),
%    Status = io_lib:format(?ROLLBACK_SENDA_FINISH, [Uid, TimeStr]),
%    set_text(Status).

%-spec rollback_senda_fail() -> ok.

%rollback_senda_fail() -> set_text(?ROLLBACK_SENDA_FAIL).

%%%=============================================================================

-spec rollback_reg_start(El) -> ok when
    El :: cauder_map:map_element().

rollback_reg_start(El) -> set_text(io_lib:format(?ROLLBACK_REG_START, [El])).

-spec rollback_reg_finish(El, Time) -> ok when
    El :: cauder_map:map_element(),
    Time :: non_neg_integer().

rollback_reg_finish(El, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_REG_FINISH, [El, TimeStr]),
    set_text(Status).

-spec rollback_reg_fail() -> ok.

rollback_reg_fail() -> set_text(?ROLLBACK_REG_FAIL).

%%%=============================================================================

-spec rollback_del_start(El) -> ok when
    El :: cauder_map:map_element().

rollback_del_start(El) -> set_text(io_lib:format(?ROLLBACK_DEL_START, [El])).

-spec rollback_del_finish(El, Time) -> ok when
    El :: cauder_map:map_element(),
    Time :: non_neg_integer().

rollback_del_finish(El, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_DEL_FINISH, [El, TimeStr]),
    set_text(Status).

-spec rollback_del_fail() -> ok.

rollback_del_fail() -> set_text(?ROLLBACK_DEL_FAIL).

%%%=============================================================================
-spec rollback_receive_start(Uid) -> ok when
    Uid :: cauder_message:uid().

rollback_receive_start(Uid) -> set_text(io_lib:format(?ROLLBACK_RECEIVE_START, [Uid])).

-spec rollback_receive_finish(Uid, Time) -> ok when
    Uid :: cauder_message:uid(),
    Time :: non_neg_integer().

rollback_receive_finish(Uid, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_RECEIVE_FINISH, [Uid, TimeStr]),
    set_text(Status).

-spec rollback_receive_fail() -> ok.

rollback_receive_fail() -> set_text(?ROLLBACK_RECEIVE_FAIL).

%%%=============================================================================

-spec rollback_variable_start(Name) -> ok when
    Name :: atom().

rollback_variable_start(Name) -> set_text(io_lib:format(?ROLLBACK_VARIABLE_START, [Name])).

-spec rollback_variable_finish(Name, Time) -> ok when
    Name :: atom(),
    Time :: non_neg_integer().

rollback_variable_finish(Name, Time) ->
    TimeStr = time_to_string(Time),
    Status = io_lib:format(?ROLLBACK_VARIABLE_FINISH, [Name, TimeStr]),
    set_text(Status).

-spec rollback_variable_fail() -> ok.

rollback_variable_fail() -> set_text(?ROLLBACK_VARIABLE_FAIL).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

-spec set_text(Text) -> ok when
    Text :: unicode:chardata().

set_text(Text) ->
    Frame = cauder_wx:find(?FRAME, wxFrame),
    StatusBar = wxFrame:getStatusBar(Frame),
    wxStatusBar:setStatusText(StatusBar, Text).

-spec semantics_to_string(Semantics) -> String when
    Semantics :: cauder_semantics:semantics(),
    String :: string().

semantics_to_string(?SEM_FWD) -> "forward";
semantics_to_string(?SEM_BWD) -> "backward".

-spec time_to_string(Time) -> String when
    Time :: non_neg_integer(),
    String :: string().

time_to_string(Time) when Time < 1000 -> "<1 ms";
time_to_string(Time) when Time < 1000 * 60 -> io_lib:format("~b ms", [Time div 1000]);
time_to_string(Time) -> io_lib:format("~b s ~b ms", [Time div (1000 * 60), Time div 1000]).
