-module(cauder_wx).

-behaviour(wx_object).

%% API
-export([start/0, start_link/0, stop/1]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MENU_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_menu_selected}}).
-define(BUTTON_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_button_clicked}}).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-record(wx_state, {
  frame :: wxFrame:wxFrame(),
  menubar :: wxMenuBar:wxMenuBar(),
  content :: wxWindow:wxWindow(),
  statusbar :: wxStatusBar:wxStatusBar(),

  module :: atom() | undefined
}).

-type state() :: #wx_state{}.


%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, Pid :: pid(), Window :: wxWindow:wxWindow()} | {error, Reason :: term()}.
start() ->
  case whereis(cauder) of
    undefined -> cauder:start();
    _ -> ok
  end,
  case wx_object:start({local, ?SERVER}, ?MODULE, [], []) of
    {error, _} = E -> E;
    WxObject -> {ok, wx_object:get_pid(WxObject), WxObject}
  end.


-spec start_link() -> {ok, Pid :: pid(), Window :: wxWindow:wxWindow()} | {error, Reason :: term()}.
start_link() ->
  case whereis(cauder) of
    undefined -> cauder:start_link();
    _ -> ok
  end,
  case wx_object:start_link({local, ?SERVER}, ?MODULE, [], []) of
    {error, _} = E -> E;
    WxObject -> {ok, wx_object:get_pid(WxObject), WxObject}
  end.


-spec stop(WxObject :: wxWindow:wxWindow()) -> 'ok'.
stop(WxObject) -> wx_object:stop(WxObject).


%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

-spec init(Args :: list()) -> {wxFrame:wxFrame(), state()}.

init([]) ->
  wx:new(),

  Frame = wxFrame:new(wx:null(), ?FRAME, ?APPNAME, [{size, ?FRAME_SIZE_INIT}]),

  Menubar = cauder_wx_menu:create(Frame),
  Content = cauder_wx_areas:create(Frame),
  StatusBar = cauder_wx_statusbar:create(Frame),

  wxMenuBar:check(Menubar, ?MENU_View_Mailbox, true),
  wxMenuBar:check(Menubar, ?MENU_View_Log, true),
  wxMenuBar:check(Menubar, ?MENU_View_History, true),
  wxMenuBar:check(Menubar, ?MENU_View_Stack, true),
  wxMenuBar:check(Menubar, ?MENU_View_Environment, true),
  wxMenuBar:check(Menubar, ?MENU_View_CurrentExpression, true),

  wxMenuBar:check(Menubar, ?MENU_View_ConcurrentHistory, true),
  wxMenuBar:check(Menubar, ?MENU_View_RelevantEnvironment, true),

  wxMenuBar:check(Menubar, ?MENU_View_StatusBar, true),

  CodeCtrl = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:update_buttons(CodeCtrl, Menubar),

  wxEvtHandler:connect(Frame, close_window),
  wxEvtHandler:connect(Frame, command_button_clicked),
  wxEvtHandler:connect(Frame, command_menu_selected),
  wxEvtHandler:connect(Frame, command_text_updated),

  wxFrame:show(Frame),
  wxFrame:raise(Frame),

  refresh(true),

  {Frame, #wx_state{
    frame     = Frame,
    menubar   = Menubar,
    content   = Content,
    statusbar = StatusBar
  }}.


-spec handle_event(Event :: wx(), State :: state()) -> {'noreply', state()} | {'stop', 'normal', state()}.

%% -------------------- File Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_File_Open), #wx_state{frame = Frame} = State) ->
  Message = "Select an Erlang file",
  Wildcard = "Erlang (*.erl)|*.erl|All files (*.*)|*.*",
  Dialog = wxFileDialog:new(Frame, [{message, Message},
                                    {wildCard, Wildcard},
                                    {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
  State1 =
    case wxDialog:showModal(Dialog) of
      ?wxID_OK ->
        Path = wxFileDialog:getPath(Dialog),
        Module = open_file(Path),
        State#wx_state{module = Module};
      _Other -> State
    end,
  wxDialog:destroy(Dialog),

  {noreply, State1};

handle_event(?MENU_EVENT(?MENU_Run_Start), #wx_state{frame = Frame, module = Module} = State) ->
  EntryPoints = cauder:get_entry_points(Module),
  case cauder_wx_dialog:start_session(Frame, EntryPoints) of
    {manual, {Mod, Fun, Args}} -> start_manual_session(Mod, Fun, Args);
    {replay, TracePath} -> start_replay_session(TracePath);
    false -> ok
  end,
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_Run_Stop), #wx_state{frame = Frame} = State) ->
  case cauder_wx_dialog:stop_session(Frame) of
    true -> stop_session();
    false -> ok
  end,
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_File_Exit), State) ->
  %% TODO Add confirmation dialog?
  {stop, normal, State};

%% -------------------- View Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_View_ZoomIn), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:zoom_in(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_ZoomOut), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:zoom_out(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Zoom100), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:zoom_reset(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(Mode), State) when ?IS_HISTORY_MODE(Mode) ->
  refresh(true),
  {noreply, State};

handle_event(?MENU_EVENT(Mode), State) when ?IS_ENVIRONMENT_MODE(Mode) ->
  refresh(true),
  {noreply, State};

%% -------------------- Help Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_Help_ViewHelp), State) ->
  wx_misc:launchDefaultBrowser(?WEBPAGE),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_Help_About), #wx_state{frame = Frame} = State) ->
  cauder_wx_dialog:about(Frame),
  {noreply, State};

%% -------------------- Code Area -------------------- %%

handle_event(#wx{id = ?CODE_TEXT, event = #wxStyledText{type = stc_zoom}}, #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

%% -------------------- Process Selector -------------------- %%

handle_event(#wx{id = ?PROC_CHOICE, event = #wxCommand{type = command_choice_selected}}, State) ->
  refresh(true),
  {noreply, State};

%% -------------------- Manual Actions -------------------- %%

handle_event(?BUTTON_EVENT(Button), State) when ?IS_STEP_BUTTON(Button) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Sem = button_to_semantics(Button),
      Rule = cauder:eval_step(Sem, Pid),
      cauder_wx_statusbar:step(Sem, Rule),
      refresh(true)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(Button), State) when ?IS_STEP_OVER_BUTTON(Button) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Sem = button_to_semantics(Button),
      case cauder:eval_step_over(Sem, Pid) of
        nomatch ->
          cauder_wx_statusbar:no_match(),
          refresh(false);
        Steps ->
          cauder_wx_statusbar:step_over(Sem, Steps),
          refresh(true)
      end
  end,
  {noreply, State};

%% -------------------- Automatic Actions -------------------- %%

handle_event(?BUTTON_EVENT(Button), State) when ?IS_MULT_BUTTON(Button) ->
  Sem = button_to_semantics(Button),
  Spinner = utils_gui:find(?STEPS_SPIN, wxSpinCtrl),
  Steps = wxSpinCtrl:getValue(Spinner),
  StepsDone = cauder:eval_mult(Sem, Steps),
  cauder_wx_statusbar:multi(Sem, StepsDone, Steps),
  refresh(true),
  {noreply, State};

%% -------------------- Replay Actions -------------------- %%

handle_event(?BUTTON_EVENT(?REPLAY_STEPS_BUTTON), State) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Spinner = utils_gui:find(?REPLAY_STEPS_SPIN, wxSpinCtrl),
      Steps = wxSpinCtrl:getValue(Spinner),
      StepsDone = cauder:eval_replay(Pid, Steps),
      cauder_wx_statusbar:replay(StepsDone, Steps),
      refresh(true)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?REPLAY_SPAWN_BUTTON), State) ->
  TextCtrl = utils_gui:find(?REPLAY_SPAWN_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:replay_spawn(false, none),
      refresh(false);
    {Pid, _} ->
      Success = cauder:eval_replay_spawn(Pid),
      cauder_wx_statusbar:replay_spawn(Success, Pid),
      refresh(Success)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?REPLAY_SEND_BUTTON), State) ->
  TextCtrl = utils_gui:find(?REPLAY_SEND_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:replay_send(false, none),
      refresh(false);
    {Uid, _} ->
      Success = cauder:eval_replay_send(Uid),
      cauder_wx_statusbar:replay_send(Success, Uid),
      refresh(Success)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?REPLAY_REC_BUTTON), State) ->
  TextCtrl = utils_gui:find(?REPLAY_REC_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:replay_rec(false, none),
      refresh(false);
    {Uid, _} ->
      Success = cauder:eval_replay_rec(Uid),
      cauder_wx_statusbar:replay_rec(Success, Uid),
      refresh(Success)
  end,
  {noreply, State};

%% -------------------- Rollback Actions -------------------- %%

handle_event(?BUTTON_EVENT(?ROLL_STEPS_BUTTON), State) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Spinner = utils_gui:find(?ROLL_STEPS_SPIN, wxSpinCtrl),
      Steps = wxSpinCtrl:getValue(Spinner),
      {FocusLog, StepsDone} = cauder:eval_roll(Pid, Steps),
      cauder_wx_statusbar:roll(StepsDone, Steps),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(true)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ROLL_SPAWN_BUTTON), State) ->
  TextCtrl = utils_gui:find(?ROLL_SPAWN_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:roll_spawn(false, none),
      refresh(false);
    {Pid, _} ->
      {Success, FocusLog} = cauder:eval_roll_spawn(Pid),
      cauder_wx_statusbar:roll_spawn(Success, Pid),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(Success)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ROLL_SEND_BUTTON), State) ->
  TextCtrl = utils_gui:find(?ROLL_SEND_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:roll_send(false, none),
      refresh(false);
    {Uid, _} ->
      {Success, FocusLog} = cauder:eval_roll_send(Uid),
      cauder_wx_statusbar:roll_send(Success, Uid),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(Success)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ROLL_REC_BUTTON), State) ->
  TextCtrl = utils_gui:find(?ROLL_REC_TEXT, wxTextCtrl),
  case string:to_integer(wxTextCtrl:getValue(TextCtrl)) of
    % What if error?
    {error, _} ->
      cauder_wx_statusbar:roll_rec(false, none),
      refresh(false);
    {Uid, _} ->
      {Success, FocusLog} = cauder:eval_roll_rec(Uid),
      cauder_wx_statusbar:roll_rec(Success, Uid),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(Success)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ROLL_VAR_BUTTON), State) ->
  TextCtrl = utils_gui:find(?ROLL_VAR_TEXT, wxTextCtrl),
  case list_to_atom(wxTextCtrl:getValue(TextCtrl)) of
    '' ->
      cauder_wx_statusbar:roll_var(false, none),
      refresh(false);
    Name ->
      {Success, FocusLog} = cauder:eval_roll_var(Name),
      cauder_wx_statusbar:roll_var(Success, Name),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(Success)
  end,
  {noreply, State};

%% -------------------- Bindings handler -------------------- %%

handle_event(#wx{id = ?BINDINGS_LIST, event = #wxList{type = command_list_item_activated, itemIndex = Index}, obj = BindingList}, #wx_state{frame = Frame} = State) ->
  Sys = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  {ok, #proc{env = Bs}} = orddict:find(Pid, Sys#sys.procs),
  Nth = wxListCtrl:getItemData(BindingList, Index),
  {Key, Value} = lists:nth(Nth, Bs),
  case cauder_wx_dialog:edit_binding(Frame, {Key, Value}) of
    {Key, NewValue} ->
      cauder:set_binding(Pid, {Key, NewValue}),
      refresh(true);
    cancel -> ok
  end,
  {noreply, State};

%% -------------------- Text handler -------------------- %%

handle_event(#wx{id = ?STEPS_SPIN, event = #wxCommand{type = command_text_updated}}, State) ->
  refresh(false),
  {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_text_updated}}, State) -> {noreply, State};

%% -------------------- Other handler -------------------- %%

handle_event(#wx{event = #wxClose{}}, State) ->
  %% TODO Add confirmation dialog?
  {stop, normal, State};

handle_event(Event, State) ->
  io:format("Unhandled Event:~n~p~n", [Event]),
  {noreply, State}.


-spec handle_call(Request :: any(), From :: any(), State :: state()) -> {reply, ok, state()}.

handle_call(Request, _From, State) ->
  io:format("Unhandled Call:~n~p~n", [Request]),
  {reply, ok, State}.


-spec handle_cast(Request :: any(), State :: state()) -> {'noreply', state()}.

handle_cast(Request, State) ->
  io:format("Unhandled Cast:~n~p~n", [Request]),
  {noreply, State}.


-spec handle_info(Info :: any(), State :: state()) -> {'noreply', state()}.

handle_info(Info, State) ->
  io:format("Unhandled Info:~n~p~n", [Info]),
  {noreply, State}.


-spec terminate(Reason :: any(), State :: state()) -> 'ok'.

terminate(_Reason, #wx_state{frame = Frame}) ->
  wxFrame:destroy(Frame),
  wx:destroy(),
  case whereis(cauder) of
    undefined -> ok;
    _ -> cauder:stop()
  end,
  ok.


-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Loads the specified '.erl' source file

-spec open_file(File :: file:filename()) -> Module :: atom().

open_file(File) ->
  {ok, Module} = cauder:load_file(File),

  {ok, Src, _} = erl_prim_loader:get_file(File),
  CodeCtrl = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:load_code(CodeCtrl, <<Src/binary, 0:8>>),

  cauder_wx_menu:enable(?MENU_Run_Start, true),

  cauder_wx_statusbar:update("Loaded file " ++ File),

  Module.


%%--------------------------------------------------------------------
%% @doc Starts a new manual debugging session with the given function application.

-spec start_manual_session(Module :: atom(), Function :: atom(), Arity :: list(erl_parse:abstract_expr())) -> 'ok'.

start_manual_session(M, F, As) ->
  ok = cauder:init_system(M, F, As),
  start_session(M, F, length(As)).


%%--------------------------------------------------------------------
%% @doc Starts a new replay debugging session using the trace in the given directory.

-spec start_replay_session(LogPath :: file:filename()) -> 'ok'.

start_replay_session(Path) ->
  {M, F, A} = cauder:init_system(Path),
  start_session(M, F, A).


%%--------------------------------------------------------------------
%% @doc Starts a new debugging session.

-spec start_session(Module :: atom(), Function :: atom(), Arity :: arity()) -> 'ok'.

start_session(M, F, A) ->
  put(line, 0),

  refresh(true),

  cauder_wx_menu:enable(?MENU_Run_Start, false),
  cauder_wx_menu:enable(?MENU_Run_Stop, true),

  % Update status bar message
  cauder_wx_statusbar:update(io_lib:format("Started system with ~p:~p/~b fun application!", [M, F, A])).


%%--------------------------------------------------------------------
%% @doc Stops the current debugging session.

-spec stop_session() -> 'ok'.

stop_session() ->
  ok = cauder:stop_system(),

  erase(line),

  refresh(true),

  cauder_wx_menu:enable(?MENU_Run_Start, true),
  cauder_wx_menu:enable(?MENU_Run_Stop, false),

  % Update status bar message
  cauder_wx_statusbar:update("Stopped system!").


%%--------------------------------------------------------------------
%% @doc Refreshes the UI.

-spec refresh(boolean()) -> ok.

refresh(RefreshState) ->
  % TODO Remove argument
  System = cauder:get_system(),
  case RefreshState of
    false -> ok;
    true ->
      % First update actions so a process is selected
      cauder_wx_actions:update(System),
      cauder_wx_system:update(System),

      Pid = cauder_wx_actions:selected_pid(),

      cauder_wx_code:update(System, Pid),
      cauder_wx_process:update(System, Pid)
  end.


-spec button_to_semantics(ButtonId) -> Semantics when
  ButtonId :: ?STEP_FORWARD_BUTTON | ?STEP_BACKWARD_BUTTON |
  ?STEP_OVER_FORWARD_BUTTON | ?STEP_OVER_BACKWARD_BUTTON |
%%  ?STEP_INTO_FORWARD_BUTTON | ?STEP_INTO_BACKWARD_BUTTON |
  ?MULTIPLE_FORWARD_BUTTON | ?MULTIPLE_BACKWARD_BUTTON,
  Semantics :: ?FWD_SEM | ?BWD_SEM.

button_to_semantics(?STEP_FORWARD_BUTTON)       -> ?FWD_SEM;
button_to_semantics(?STEP_BACKWARD_BUTTON)      -> ?BWD_SEM;
button_to_semantics(?STEP_OVER_FORWARD_BUTTON)  -> ?FWD_SEM;
button_to_semantics(?STEP_OVER_BACKWARD_BUTTON) -> ?BWD_SEM;
%%button_to_semantics(?STEP_INTO_FORWARD_BUTTON)  -> ?FWD_SEM;
%%button_to_semantics(?STEP_INTO_BACKWARD_BUTTON) -> ?BWD_SEM;
button_to_semantics(?MULTIPLE_FORWARD_BUTTON)   -> ?FWD_SEM;
button_to_semantics(?MULTIPLE_BACKWARD_BUTTON)  -> ?BWD_SEM.
