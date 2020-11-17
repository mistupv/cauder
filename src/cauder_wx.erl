-module(cauder_wx).

-behaviour(wx_object).

%% API
-export([start/0, start_link/0, stop/1, wait_forever/1, find/2]).
-export([show_error/1]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MENU_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_menu_selected}}).
-define(MENU_EVENT(Id, CmdValue), #wx{id = Id, event = #wxCommand{type = command_menu_selected, commandInt = CmdValue}}).
-define(BUTTON_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_button_clicked}}).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-type state() :: #wx_state{}.

-export_type([state/0]).


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Starts the debugging server.

-spec start() -> {ok, Pid, Window} | {error, Reason} when
  Pid :: pid(),
  Window :: wxWindow:wxWindow(),
  Reason :: term().

start() ->
  case whereis(cauder) of
    undefined ->
      {error, {not_started, cauder}};
    _ ->
      case wx_object:start({local, ?SERVER}, ?MODULE, [], []) of
        {error, _} = Error -> Error;
        Window -> {ok, wx_object:get_pid(Window), Window}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Starts the debugging server as part of a supervision tree.

-spec start_link() -> {ok, Pid, Window} | {error, Reason} when
  Pid :: pid(),
  Window :: wxWindow:wxWindow(),
  Reason :: term().

start_link() ->
  case whereis(cauder) of
    undefined ->
      {error, {not_started, cauder}};
    _ ->
      case wx_object:start_link({local, ?SERVER}, ?MODULE, [], []) of
        {error, _} = Error -> Error;
        Window -> {ok, wx_object:get_pid(Window), Window}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Stops the debugging server.

-spec stop(WxObject) -> ok when
  WxObject :: wxWindow:wxWindow().

stop(WxObject) -> wx_object:stop(WxObject).


%%------------------------------------------------------------------------------
%% @doc Waits until the UI is closed.

-spec wait_forever(WxObject) -> ok when
  WxObject :: wxWindow:wxWindow().

wait_forever(WxObject) ->
  catch wx_object:call(WxObject, noreply),
  ok.


%%------------------------------------------------------------------------------
%% @doc Find the first window with the given `Id' and casts it to the given
%% `Type'.

-spec find(Id, Type) -> Window when
  Id :: integer(),
  Type :: atom(),
  Window :: wx:wx_object().

find(Id, Type) -> wx:typeCast(wxWindow:findWindowById(Id), Type).


%%%=============================================================================


-spec show_error(Error) -> ok when
  Error :: any().

show_error(Error) ->
  wx:new(),
  Message = io_lib:format("There was an error: ~p", [Error]),
  Options = [{caption, "CauDer: Error"}, {style, ?wxICON_ERROR}],
  Dialog = wxMessageDialog:new(wx:null(), Message, Options),
  try
    wxMessageDialog:showModal(Dialog)
  after
    wxMessageDialog:destroy(Dialog),
    wx:destroy()
  end,
  ok.


%%%=============================================================================
%%% wx_object callbacks
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @private

-spec init(Args) -> {WxObject, State} when
  Args :: term(),
  WxObject :: wxFrame:wxFrame(),
  State :: state().

init([]) ->
  ?GUI_DB = ets:new(?GUI_DB, [set, private, named_table]),

  wx:new(),

  Frame = wxFrame:new(wx:null(), ?FRAME, ?APP_NAME, [{size, ?FRAME_SIZE_INIT}]),

  MenuBar = cauder_wx_menu:create(Frame),
  Content = cauder_wx_areas:create(Frame),
  StatusBar = cauder_wx_statusbar:create(Frame),

  cauder:subscribe(),

  CodeCtrl = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:update_buttons(CodeCtrl, MenuBar),

  % Subscribe to UI events

  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  wxEvtHandler:connect(Frame, command_text_updated),

  % Update menu bar

  Config = cauder_wx_config:load(),

  wxMenuBar:check(MenuBar, ?MENU_View_CurrentExpression, true), % Config#config.current_expression
  wxMenuBar:check(MenuBar, ?MENU_View_Bindings, Config#config.bindings),
  wxMenuBar:check(MenuBar, ?MENU_View_Stack, Config#config.stack),
  wxMenuBar:check(MenuBar, ?MENU_View_Log, Config#config.log),
  wxMenuBar:check(MenuBar, ?MENU_View_History, Config#config.history),
  wxMenuBar:check(MenuBar, ?MENU_View_Mailbox, true), % Config#config.mailbox

  case Config#config.bindings_mode of
    all -> wxMenuBar:check(MenuBar, ?MENU_View_AllBindings, true);
    relevant -> wxMenuBar:check(MenuBar, ?MENU_View_RelevantBindings, true)
  end,

  case Config#config.history_mode of
    full -> wxMenuBar:check(MenuBar, ?MENU_View_FullHistory, true);
    concurrent -> wxMenuBar:check(MenuBar, ?MENU_View_ConcurrentHistory, true)
  end,

  wxMenuBar:check(MenuBar, ?MENU_View_StatusBar, Config#config.status_bar),

  % Disable action

  wxChoice:disable(cauder_wx:find(?ACTION_Process, wxChoice)),
  wxPanel:disable(cauder_wx:find(?ACTION_Manual, wxPanel)),
  wxPanel:disable(cauder_wx:find(?ACTION_Automatic, wxPanel)),
  wxPanel:disable(cauder_wx:find(?ACTION_Replay, wxPanel)),
  wxPanel:disable(cauder_wx:find(?ACTION_Rollback, wxPanel)),

  % Show window

  wxFrame:show(Frame),
  wxFrame:raise(Frame),

  State = #wx_state{
    frame     = Frame,
    menubar   = MenuBar,
    content   = Content,
    statusbar = StatusBar
  },

  {Frame, refresh(State, State#wx_state{config = Config})}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_event(Event, State) -> {noreply, NewState} | {stop, normal, NewState} when
  Event :: wx(),
  State :: state(),
  NewState :: state().

handle_event(?MENU_EVENT(?MENU_File_Open), #wx_state{frame = Frame} = State) ->
  case stop_session(State) of
    true ->
      Message = "Select an Erlang file",
      Wildcard = "Erlang (*.erl)|*.erl|All files (*.*)|*.*",
      Dialog = wxFileDialog:new(Frame, [{message, Message},
                                        {wildCard, Wildcard},
                                        {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
      case wxDialog:showModal(Dialog) of
        ?wxID_OK ->
          File = wxFileDialog:getPath(Dialog),
          {ok, System} = cauder:load_file(File),
          cauder_wx_statusbar:load_start(filename:basename(File)),
          wxDialog:destroy(Dialog),
          {noreply, refresh(State, State#wx_state{system = System, task = load})};
        _Other ->
          wxDialog:destroy(Dialog),
          {noreply, State}
      end;
    false ->
      {noreply, State}
  end;

handle_event(?MENU_EVENT(?MENU_File_Exit), State) ->
  %% TODO Add confirmation dialog?
  stop_cauder(State);

handle_event(?MENU_EVENT(?MENU_View_ZoomIn), #wx_state{menubar = Menubar} = State) ->
  CodeArea = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_in(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_ZoomOut), #wx_state{menubar = Menubar} = State) ->
  CodeArea = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_out(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Zoom100), #wx_state{menubar = Menubar} = State) ->
  CodeArea = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_reset(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(Item, Check), #wx_state{config = Config} = State) when ?Is_Visibility_Item(Item) ->
  Show = Check =:= 1,
  NewConfig =
    case Item of
      ?MENU_View_Bindings -> Config#config{bindings = Show};
      ?MENU_View_Stack -> Config#config{stack = Show};
      ?MENU_View_Log -> Config#config{log = Show};
      ?MENU_View_History -> Config#config{history = Show}
    end,
  cauder_wx_config:save(NewConfig),
  {noreply, refresh(State, State#wx_state{config = NewConfig})};

handle_event(?MENU_EVENT(Item), #wx_state{config = Config} = State) when ?Is_Bindings_Mode(Item) ->
  NewConfig =
    case Item of
      ?MENU_View_AllBindings -> Config#config{bindings_mode = all};
      ?MENU_View_RelevantBindings -> Config#config{bindings_mode = relevant}
    end,
  cauder_wx_config:save(NewConfig),
  {noreply, refresh(State, State#wx_state{config = NewConfig})};

handle_event(?MENU_EVENT(Item), #wx_state{config = Config} = State) when ?Is_History_Mode(Item) ->
  NewConfig =
    case Item of
      ?MENU_View_FullHistory -> Config#config{history_mode = full};
      ?MENU_View_ConcurrentHistory -> Config#config{history_mode = concurrent}
    end,
  cauder_wx_config:save(NewConfig),
  {noreply, refresh(State, State#wx_state{config = NewConfig})};

handle_event(?MENU_EVENT(?MENU_View_StatusBar, Check), #wx_state{config = Config} = State) ->
  Show = Check =:= 1,
  NewConfig = Config#config{status_bar = Show},
  cauder_wx_config:save(NewConfig),
  {noreply, refresh(State, State#wx_state{config = NewConfig})};

%%%=============================================================================

handle_event(?MENU_EVENT(?MENU_Run_Start), State) ->
  case start_session(State) of
    true ->
      {noreply, refresh(State, State#wx_state{task = start})};
    false ->
      {noreply, State}
  end;

handle_event(?MENU_EVENT(?MENU_Run_Stop), State) ->
  case stop_session(State) of
    true ->
      {noreply, refresh(State, State#wx_state{system = cauder:get_system()})};
    false ->
      {noreply, State}
  end;

%%%=============================================================================

handle_event(?MENU_EVENT(?MENU_Help_ViewHelp), State) ->
  wx_misc:launchDefaultBrowser(?APP_URL),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_Help_About), #wx_state{frame = Frame} = State) ->
  cauder_wx_dialog:about(Frame),
  {noreply, State};

%%%=============================================================================

handle_event(#wx{id = ?CODE_Code_Control, event = #wxStyledText{type = stc_zoom}}, #wx_state{menubar = Menubar} = State) ->
  CodeArea = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

%%%=============================================================================

handle_event(#wx{id = ?ACTION_Process, event = #wxCommand{type = command_choice_selected}}, State) ->
  {noreply, refresh(State, State)};

%%%=============================================================================

handle_event(?BUTTON_EVENT(Button), #wx_state{pid = Pid} = State) when ?Is_Step_Button(Button) andalso Pid =/= undefined ->
  Sem = button_to_semantics(Button),
  {ok, System} = cauder:step(Sem, Pid),
  cauder_wx_statusbar:step_start(Sem),
  {noreply, refresh(State, State#wx_state{system = System, task = step})};

handle_event(?BUTTON_EVENT(Button), #wx_state{pid = Pid} = State) when ?Is_StepOver_Button(Button) andalso Pid =/= undefined ->
  Sem = button_to_semantics(Button),
  {ok, System} = cauder:step_over(Sem, Pid),
  cauder_wx_statusbar:step_start(Sem),
  {noreply, refresh(State, State#wx_state{system = System, task = step_over})};

%%%=============================================================================

handle_event(?BUTTON_EVENT(Button), State) when ?Is_Automatic_Button(Button) ->
  Sem = button_to_semantics(Button),
  Spinner = cauder_wx:find(?ACTION_Automatic_Steps, wxSpinCtrl),
  Steps = wxSpinCtrl:getValue(Spinner),
  {ok, System} = cauder:step_multiple(Sem, Steps),
  cauder_wx_statusbar:step_start(Sem),
  {noreply, refresh(State, State#wx_state{system = System, task = step_multiple})};

%%%=============================================================================

handle_event(?BUTTON_EVENT(?ACTION_Replay_Steps_Button), #wx_state{pid = Pid} = State) when Pid =/= undefined ->
  Spinner = cauder_wx:find(?ACTION_Replay_Steps, wxSpinCtrl),
  Steps = wxSpinCtrl:getValue(Spinner),
  {ok, System} = cauder:replay_steps(Pid, Steps),
  cauder_wx_statusbar:replay_steps_start(),
  {noreply, refresh(State, State#wx_state{system = System, task = replay_steps})};

handle_event(?BUTTON_EVENT(?ACTION_Replay_Spawn_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Replay_Spawn, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Pid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:replay_spawn(Pid),
  cauder_wx_statusbar:replay_spawn_start(Pid),
  {noreply, refresh(State, State#wx_state{system = System, task = replay_spawn})};

handle_event(?BUTTON_EVENT(?ACTION_Replay_Send_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Replay_Send, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Uid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:replay_send(Uid),
  cauder_wx_statusbar:replay_send_start(Uid),
  {noreply, refresh(State, State#wx_state{system = System, task = replay_send})};

handle_event(?BUTTON_EVENT(?ACTION_Replay_Receive_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Replay_Receive, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Uid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:replay_receive(Uid),
  cauder_wx_statusbar:replay_receive_start(Uid),
  {noreply, refresh(State, State#wx_state{system = System, task = replay_receive})};

handle_event(?BUTTON_EVENT(?ACTION_Replay_FullLog_Button), State) ->
  {ok, System} = cauder:replay_full_log(),
  cauder_wx_statusbar:replay_full_log_start(),
  {noreply, refresh(State, State#wx_state{system = System, task = replay_full_log})};

%%%=============================================================================

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Steps_Button), #wx_state{pid = Pid} = State) when Pid =/= undefined ->
  Spinner = cauder_wx:find(?ACTION_Rollback_Steps, wxSpinCtrl),
  Steps = wxSpinCtrl:getValue(Spinner),
  {ok, System} = cauder:rollback_steps(Pid, Steps),
  cauder_wx_statusbar:rollback_steps_start(),
  {noreply, refresh(State, State#wx_state{system = System, task = rollback_steps})};

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Spawn_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Rollback_Spawn, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Pid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:rollback_spawn(Pid),
  cauder_wx_statusbar:rollback_spawn_start(Pid),
  {noreply, refresh(State, State#wx_state{system = System, task = rollback_spawn})};

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Send_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Rollback_Send, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Uid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:rollback_send(Uid),
  cauder_wx_statusbar:rollback_send_start(Uid),
  {noreply, refresh(State, State#wx_state{system = System, task = rollback_send})};

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Receive_Button), State) ->
  Choice = cauder_wx:find(?ACTION_Rollback_Receive, wxChoice),
  Idx = wxChoice:getSelection(Choice),
  Uid = wxChoice:getClientData(Choice, Idx),
  {ok, System} = cauder:rollback_receive(Uid),
  cauder_wx_statusbar:rollback_receive_start(Uid),
  {noreply, refresh(State, State#wx_state{system = System, task = rollback_receive})};

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Variable_Button), State) ->
  TextCtrl = cauder_wx:find(?ACTION_Rollback_Variable, wxTextCtrl),
  case list_to_atom(wxTextCtrl:getValue(TextCtrl)) of
    '' ->
      cauder_wx_statusbar:rollback_variable_fail(),
      {noreply, State};
    Name ->
      {ok, System} = cauder:rollback_variable(Name),
      cauder_wx_statusbar:rollback_variable_start(Name),
      {noreply, refresh(State, State#wx_state{system = System, task = rollback_variable})}
  end;

%%%=============================================================================

handle_event(
    #wx{id = ?PROCESS_Bindings_Control, event = #wxList{type = command_list_item_activated, itemIndex = Idx}},
    #wx_state{frame = Frame, system = #sys{procs = PMap}, pid = Pid} = State
) ->
  #proc{env = Bs} = maps:get(Pid, PMap),
  IdxToKey = ets:lookup_element(?GUI_DB, ?IDX_TO_KEY, 2),
  Key = maps:get(Idx, IdxToKey),
  Value = maps:get(Key, Bs),
  case cauder_wx_dialog:edit_binding(Frame, {Key, Value}) of
    {Key, NewValue} -> ok = cauder:set_binding(Pid, {Key, NewValue});
    cancel -> ok
  end,
  {noreply, refresh(State, State#wx_state{system = cauder:get_system()})};

%%%=============================================================================

%%handle_event(#wx{id = ?ACTION_Automatic_Steps, event = #wxCommand{type = command_text_updated}}, State) ->
%%  {noreply, refresh(State)};

handle_event(#wx{event = #wxCommand{type = command_text_updated}}, State) -> {noreply, State};

handle_event(#wx{event = #wxStyledText{type = stc_updateui}}, #wx_state{position = OldPosition} = State) ->
  CodeControl = find(?CODE_Code_Control, wxStyledTextCtrl),
  case wxStyledTextCtrl:getCurrentPos(CodeControl) of
    OldPosition ->
      {noreply, State};
    NewPosition ->
      Line = wxStyledTextCtrl:lineFromPosition(CodeControl, NewPosition),
      Column = NewPosition - wxStyledTextCtrl:positionFromLine(CodeControl, Line),
      cauder_wx_statusbar:update_position(Line + 1, Column + 1),
      {noreply, State#wx_state{position = NewPosition}}
  end;

%%%=============================================================================

handle_event(#wx{event = #wxDropFiles{files = Files}}, #wx_state{frame = Frame} = State) ->
  case cauder_wx_dialog:drop_files(Frame, Files) of
    {ok, File} ->
      case stop_session(State) of
        true ->
          {ok, System} = cauder:load_file(File),
          cauder_wx_statusbar:load_start(filename:basename(File)),
          {noreply, refresh(State, State#wx_state{system = System, task = load})};
        false -> {noreply, State}
      end;
    false -> {noreply, State}
  end;

%%%=============================================================================

handle_event(#wx{event = #wxClose{}}, State) ->
%% TODO Add confirmation dialog?
  stop_cauder(State);

%%%=============================================================================

handle_event(Event, State) ->
  io:format("Unhandled Event:~n~p~n", [Event]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_call(Request, From, State) -> {reply, Reply, NewState} | {noreply, NewState} when
  Request :: term(),
  From :: {pid(), term()},
  State :: state(),
  Reply :: term(),
  NewState :: state().

handle_call(noreply, _From, State) ->
  {noreply, State};

handle_call(Request, _From, State) ->
  io:format("Unhandled Call:~n~p~n", [Request]),
  {reply, ok, State}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_cast(Request, State) -> {noreply, NewState} when
  Request :: any(),
  State :: state(),
  NewState :: state().

handle_cast(Request, State) ->
  io:format("Unhandled Cast:~n~p~n", [Request]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private

-spec handle_info(Info, State) -> {noreply, NewState} when
  Info :: any(),
  State :: state(),
  NewState :: state().

handle_info({dbg, {finish, {load, File, Module}, Time, System}}, #wx_state{task = load} = State) ->
  {ok, Src, _} = erl_prim_loader:get_file(File),

  CodeCtrl = cauder_wx:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:load_code(CodeCtrl, <<Src/binary, 0:8>>),

  cauder_wx_statusbar:load_finish(Module, Time),

  {noreply, refresh(State, State#wx_state{module = Module, system = System, task = undefined})};

handle_info({dbg, {finish, start, Time, System}}, #wx_state{task = start} = State) ->
  cauder_wx_statusbar:init_finish(Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

%%%=============================================================================

handle_info({dbg, {finish, {step, Sem, Rule}, Time, System}}, #wx_state{task = step} = State) ->
  cauder_wx_statusbar:step_finish(Sem, Rule, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {finish, {step_over, Sem, Steps}, Time, System}}, #wx_state{task = step_over} = State) ->
  cauder_wx_statusbar:step_over_finish(Sem, Steps, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {finish, {step_multiple, Sem, Steps}, Time, System}}, #wx_state{task = step_multiple} = State) ->
  cauder_wx_statusbar:step_multiple_finish(Sem, Steps, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

%%%=============================================================================

handle_info({dbg, {finish, {replay_steps, Steps}, Time, System}}, #wx_state{task = replay_steps} = State) ->
  cauder_wx_statusbar:replay_steps_finish(Steps, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};


handle_info({dbg, {finish, {replay_spawn, Pid}, Time, System}}, #wx_state{task = replay_spawn} = State) ->
  cauder_wx_statusbar:replay_spawn_finish(Pid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, replay_spawn, no_replay}}, #wx_state{task = replay_spawn} = State) ->
  cauder_wx_statusbar:replay_spawn_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, {replay_send, Uid}, Time, System}}, #wx_state{task = replay_send} = State) ->
  cauder_wx_statusbar:replay_send_finish(Uid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, replay_send, no_replay}}, #wx_state{task = replay_send} = State) ->
  cauder_wx_statusbar:replay_send_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, {replay_receive, Uid}, Time, System}}, #wx_state{task = replay_receive} = State) ->
  cauder_wx_statusbar:replay_receive_finish(Uid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, replay_receive, no_replay}}, #wx_state{task = replay_receive} = State) ->
  cauder_wx_statusbar:replay_receive_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, replay_full_log, Time, System}}, #wx_state{task = replay_full_log} = State) ->
  cauder_wx_statusbar:replay_full_log_finish(Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

%%%=============================================================================

handle_info({dbg, {finish, {rollback_steps, Steps}, Time, System}}, #wx_state{task = rollback_steps} = State) ->
  cauder_wx_statusbar:rollback_steps_finish(Steps, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};


handle_info({dbg, {finish, {rollback_spawn, Pid}, Time, System}}, #wx_state{task = rollback_spawn} = State) ->
  cauder_wx_statusbar:rollback_spawn_finish(Pid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, rollback_spawn, no_rollback}}, #wx_state{task = rollback_spawn} = State) ->
  cauder_wx_statusbar:rollback_spawn_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, {rollback_send, Uid}, Time, System}}, #wx_state{task = rollback_send} = State) ->
  cauder_wx_statusbar:rollback_send_finish(Uid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, rollback_send, no_rollback}}, #wx_state{task = rollback_send} = State) ->
  cauder_wx_statusbar:rollback_send_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, {rollback_receive, Uid}, Time, System}}, #wx_state{task = rollback_receive} = State) ->
  cauder_wx_statusbar:rollback_receive_finish(Uid, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, rollback_receive, no_rollback}}, #wx_state{task = rollback_receive} = State) ->
  cauder_wx_statusbar:rollback_receive_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};


handle_info({dbg, {finish, {rollback_variable, Name}, Time, System}}, #wx_state{task = rollback_variable} = State) ->
  cauder_wx_statusbar:rollback_variable_finish(Name, Time),
  {noreply, refresh(State, State#wx_state{system = System, task = undefined})};

handle_info({dbg, {fail, rollback_variable, no_rollback}}, #wx_state{task = rollback_variable} = State) ->
  cauder_wx_statusbar:rollback_variable_fail(),
  {noreply, refresh(State, State#wx_state{task = undefined})};

%%%=============================================================================

% TODO Handle failed tasks when the user did not start the task.
% i.e.: given {fail, Task1, _} and #wx_state{task = Task2} then Task1 != Task2


handle_info(Info, State) ->
  io:format("Unhandled Info:~n~p~n", [Info]),
  {noreply, State}.


%%------------------------------------------------------------------------------
%% @private

-spec terminate(Reason, State) -> ok when
  Reason :: any(),
  State :: state().

terminate(_Reason, #wx_state{frame = Frame}) ->
  wxFrame:destroy(Frame),
  wx:destroy(),
  cauder:stop(),
  ok.


%%------------------------------------------------------------------------------
%% @private

-spec code_change(OldVsn, State, Extra) -> {ok, NewState} when
  OldVsn :: (term() | {down, term()}),
  State :: state(),
  Extra :: term(),
  NewState :: state().

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%--------------------------------------------------------------------
%% @doc Starts a new debugging session.

-spec start_session(State) -> IsStarting when
  State :: state(),
  IsStarting :: boolean().

start_session(#wx_state{module = Module}) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  EntryPoints = cauder:get_entry_points(Module),
  case cauder_wx_dialog:start_session(Frame, EntryPoints) of
    {manual, {Mod, Fun, Args}} ->
      ok = cauder:init_system(Mod, Fun, Args),
      cauder_wx_statusbar:init_start(),
      true;
    {replay, Path} ->
      ok = cauder:init_system(Path),
      cauder_wx_statusbar:init_start(),
      true;
    false -> false
  end.


%%--------------------------------------------------------------------
%% @doc Stops the current debugging session.

-spec stop_session(State) -> Stopped when
  State :: state(),
  Stopped :: boolean().

stop_session(#wx_state{system = undefined}) ->
  true;
stop_session(_State) ->
  Frame = cauder_wx:find(?FRAME, wxFrame),
  case cauder_wx_dialog:stop_session(Frame) of
    true ->
      {ok, _} = cauder:stop_system(),
      cauder_wx_statusbar:stop_finish(),
      true;
    false -> false
  end.


%%--------------------------------------------------------------------
%% @doc Updates the UI to show changes in the state.
%% The first argument is the `OldState' without updating any value, and the
%% second one is the `NewState' with the updated values.
%%
%% For example, if we wanted to update a field named `my_field' with the value
%% `new_value', we would call this function like follows:
%%
%% ```refresh(State, State#wx_state{my_field = new_value})'''
%%
%% This allows to improve UI updates by skipping unchanged data.
%%
%% NOTE: explicit updates the `pid' field are not necessary as it is
%% automatically updated by this function.
%%
%% @see refresh/2

-spec refresh(OldState, NewState) -> State when
  OldState :: state(),
  NewState :: state(),
  State :: state().

refresh(OldState, NewState) ->
  cauder_wx_actions:update_process(OldState, NewState),

  State = NewState#wx_state{pid = cauder_wx_actions:selected_pid()},

  cauder_wx_menu:update(OldState, State),
  cauder_wx_statusbar:update(OldState, State),

  cauder_wx_code:update(OldState, State),
  cauder_wx_actions:update(OldState, State),
  cauder_wx_process:update(OldState, State),
  cauder_wx_system:update(OldState, State),

  State.


%%%=============================================================================


-spec button_to_semantics(ButtonId) -> Semantics when
  ButtonId :: ?ACTION_Manual_Step_Forward_Button | ?ACTION_Manual_Step_Backward_Button |
  ?ACTION_Manual_StepOver_Forward_Button | ?ACTION_Manual_StepOver_Backward_Button |
%%  ?STEP_INTO_FORWARD_BUTTON | ?STEP_INTO_BACKWARD_BUTTON |
  ?ACTION_Automatic_Forward_Button | ?ACTION_Automatic_Backward_Button,
  Semantics :: ?FWD_SEM | ?BWD_SEM.

button_to_semantics(?ACTION_Manual_Step_Forward_Button)      -> ?FWD_SEM;
button_to_semantics(?ACTION_Manual_Step_Backward_Button)     -> ?BWD_SEM;
button_to_semantics(?ACTION_Manual_StepOver_Forward_Button)  -> ?FWD_SEM;
button_to_semantics(?ACTION_Manual_StepOver_Backward_Button) -> ?BWD_SEM;
%%button_to_semantics(?STEP_INTO_FORWARD_BUTTON)  -> ?FWD_SEM;
%%button_to_semantics(?STEP_INTO_BACKWARD_BUTTON) -> ?BWD_SEM;
button_to_semantics(?ACTION_Automatic_Forward_Button)        -> ?FWD_SEM;
button_to_semantics(?ACTION_Automatic_Backward_Button)       -> ?BWD_SEM.


%%%=============================================================================


%%--------------------------------------------------------------------
%% @doc Stops CauDEr in a proper way.

-spec stop_cauder(State) -> {stop, normal, NewState} | {noreply, NewState} when
  State :: state(),
  NewState :: state().

stop_cauder(State) ->
  case stop_mode() of
    normal -> {stop, normal, State};
    init -> init:stop(), {noreply, State};
    {application, Name} -> application:stop(Name), {noreply, State}
  end.


%%--------------------------------------------------------------------
%% @doc Decider the proper way to stop CauDEr.

-spec stop_mode() -> normal | init | {application, atom()}.

stop_mode() ->
  case application:get_application() of
    undefined -> normal;
    {ok, App} ->
      Started = proplists:get_value(started, application:info(), []),
      case proplists:get_value(App, Started) of
        undefined -> normal;
        permanent -> init;
        _ -> {application, App}
      end
  end.
