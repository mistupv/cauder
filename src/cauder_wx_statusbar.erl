-module(cauder_wx_statusbar).

-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([create/1, update/1, set_visibility/1]).
%% Predefined statuses
-export([no_process/0, no_match/0]).
-export([step/2, step_over/2, step_into/3]).
-export([multi/3]).
-export([replay/2, replay_spawn/2, replay_send/2, replay_rec/2]).
-export([roll/2, roll_spawn/2, roll_send/2, roll_rec/2, roll_var/2]).


-spec create(Frame :: wxFrame:wxFrame()) -> wxStatusBar:wxStatusBar().

create(Frame) -> wxFrame:createStatusBar(Frame).


-spec update(Text :: unicode:chardata()) -> 'ok'.

update(Text) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  Statusbar = wxFrame:getStatusBar(Frame),
  wxStatusBar:setStatusText(Statusbar, Text).


-spec set_visibility(Visible :: boolean()) -> 'ok'.

set_visibility(Visible) ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  StatusBar = wxFrame:getStatusBar(Frame),
  case Visible of
    true -> wxStatusBar:show(StatusBar);
    false -> wxStatusBar:hide(StatusBar)
  end,
  wxFrame:sendSizeEvent(Frame).


%%%===================================================================
%%% Predefined statuses
%%%===================================================================


%% -------------------- Errors -------------------- %%


no_process() -> update("Cannot perform any action because no process is selected.").

no_match() -> update("Cannot perform any forward action because there is no matching message to be received.").


%% -------------------- Manual actions -------------------- %%


step(Sem, Rule) ->
  StrSem = semantics_to_string(Sem),
  StrRule = rule_to_string(Rule),
  Status = io_lib:format("Performed a single ~s reduction step using rule: ~s.", [StrSem, StrRule]),
  update(Status).

step_over(Sem, Steps) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped over to the next expression.", [Steps, StrSem]),
  update(Status).

step_into(Sem, Steps, {M, F, A}) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s reduction steps. Stepped into the definition of function: ~s:~s/~b.", [Steps, StrSem, M, F, A]),
  update(Status).


%% -------------------- Automatic actions -------------------- %%


multi(Sem, Done, Total) ->
  StrSem = semantics_to_string(Sem),
  Status = io_lib:format("Performed ~b ~s steps, from a total of ~b.", [Done, StrSem, Total]),
  update(Status).


%% -------------------- Rollback actions -------------------- %%


replay(Done, Total) -> update(io_lib:format("~b of ~b steps replayed.", [Done, Total])).

replay_spawn(false, _)  -> update("Could not replay the spawning of that process");
replay_spawn(true, Pid) -> update(io_lib:format("Replayed spawning of process with PID: ~p", [Pid])).

replay_send(false, _)  -> update("Could not replay the sending of that message");
replay_send(true, Uid) -> update(io_lib:format("Replayed sending of message with UID: ~p", [Uid])).

replay_rec(false, _)  -> update("Could not replay the receiving of that message");
replay_rec(true, Uid) -> update(io_lib:format("Replayed receiving of message with UID: ~p", [Uid])).


%% -------------------- Rollback actions -------------------- %%


roll(Done, Total) -> update(io_lib:format("~b of ~b steps rolled back.", [Done, Total])).

roll_spawn(false, _)  -> update("Could not roll back the spawning of that process");
roll_spawn(true, Pid) -> update(io_lib:format("Rolled back spawning of process with PID: ~p", [Pid])).

roll_send(false, _)  -> update("Could not roll back the sending of that message");
roll_send(true, Uid) -> update(io_lib:format("Rolled back sending of message with UID: ~p", [Uid])).

roll_rec(false, _)  -> update("Could not roll back the receiving of that message");
roll_rec(true, Uid) -> update(io_lib:format("Rolled back receiving of message with UID: ~p", [Uid])).

roll_var(false, _)   -> update("Could not roll back the binding of that variable");
roll_var(true, Name) -> update(io_lib:format("Rolled back binding of variable with name: ~p", [Name])).


%% -------------------- Utility functions -------------------- %%


-spec semantics_to_string(Sem) -> string() when
  Sem :: ?FWD_SEM | ?BWD_SEM.

semantics_to_string(?FWD_SEM) -> "forward";
semantics_to_string(?BWD_SEM) -> "backward".


-spec rule_to_string(Rule) -> string() when
  Rule :: ?RULE_SEQ | ?RULE_SELF | ?RULE_SPAWN | ?RULE_SEND | ?RULE_RECEIVE.

rule_to_string(?RULE_SEQ)     -> "Seq";
rule_to_string(?RULE_SELF)    -> "Self";
rule_to_string(?RULE_SPAWN)   -> "Spawn";
rule_to_string(?RULE_SEND)    -> "Send";
rule_to_string(?RULE_RECEIVE) -> "Receive".
