-module(cauder_wx).

-behaviour(wx_object).

%% API
-export([start/0, start_link/0, stop/1]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-define(MENU_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_menu_selected}}).
-define(MENU_EVENT(Id, CmdValue), #wx{id = Id, event = #wxCommand{type = command_menu_selected, commandInt = CmdValue}}).
-define(BUTTON_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_button_clicked}}).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").

-record(wx_state, {
  frame :: wxFrame:wxFrame(),
  menubar :: wxMenuBar:wxMenuBar(),
  content :: wxWindow:wxWindow(),
  statusbar :: wxStatusBar:wxStatusBar()
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
  wxMenuBar:check(Menubar, ?MENU_View_Bindings, true),
  wxMenuBar:check(Menubar, ?MENU_View_CurrentExpression, true),

  wxMenuBar:check(Menubar, ?MENU_View_ConcurrentHistory, true),
  wxMenuBar:check(Menubar, ?MENU_View_RelevantBindings, true),

  wxMenuBar:check(Menubar, ?MENU_View_StatusBar, true),

  CodeCtrl = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
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
  case stop_session() of
    false -> ok;
    true ->
      Message = "Select an Erlang file",
      Wildcard = "Erlang (*.erl)|*.erl|All files (*.*)|*.*",
      Dialog = wxFileDialog:new(Frame, [{message, Message},
                                        {wildCard, Wildcard},
                                        {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
      case wxDialog:showModal(Dialog) of
        ?wxID_OK ->
          Path = wxFileDialog:getPath(Dialog),
          Module = open_file(Path),
          put(module, Module);
        _Other -> ok
      end,
      wxDialog:destroy(Dialog)
  end,
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_File_Exit), State) ->
  %% TODO Add confirmation dialog?
  {stop, normal, State};

%% -------------------- View Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_View_ZoomIn), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_in(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_ZoomOut), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_out(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Zoom100), #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:zoom_reset(CodeArea),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Bindings), State) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_bindings(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Stack), State) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_stack(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_Log), State) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_log(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_History), State) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_history(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(Item), State) when ?Is_Bindings_Mode(Item) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_bindings(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(Item), State) when ?Is_History_Mode(Item) ->
  System = cauder:get_system(),
  Pid = cauder_wx_actions:selected_pid(),
  cauder_wx_process:update_history(System, Pid),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_View_StatusBar, Enabled), State) ->
  cauder_wx_statusbar:set_visibility(Enabled =/= 0),
  {noreply, State};

%% -------------------- Run Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_Run_Start), State) ->
  start_session(),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_Run_Stop), State) ->
  stop_session(),
  {noreply, State};

%% -------------------- Help Menu -------------------- %%

handle_event(?MENU_EVENT(?MENU_Help_ViewHelp), State) ->
  wx_misc:launchDefaultBrowser(?WEBPAGE),
  {noreply, State};

handle_event(?MENU_EVENT(?MENU_Help_About), #wx_state{frame = Frame} = State) ->
  cauder_wx_dialog:about(Frame),
  {noreply, State};

%% -------------------- Code Area -------------------- %%

handle_event(#wx{id = ?CODE_Code_Control, event = #wxStyledText{type = stc_zoom}}, #wx_state{menubar = Menubar} = State) ->
  CodeArea = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:update_margin(CodeArea),
  cauder_wx_code:update_buttons(CodeArea, Menubar),
  {noreply, State};

%% -------------------- Process Selector -------------------- %%

handle_event(#wx{id = ?ACTION_Process, event = #wxCommand{type = command_choice_selected}}, State) ->
  refresh(true),
  {noreply, State};

%% -------------------- Manual Actions -------------------- %%

handle_event(?BUTTON_EVENT(Button), State) when ?Is_Step_Button(Button) ->
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

handle_event(?BUTTON_EVENT(Button), State) when ?Is_StepOver_Button(Button) ->
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

handle_event(?BUTTON_EVENT(Button), State) when ?Is_Automatic_Button(Button) ->
  Sem = button_to_semantics(Button),
  Spinner = utils_gui:find(?ACTION_Automatic_Steps, wxSpinCtrl),
  Steps = wxSpinCtrl:getValue(Spinner),
  StepsDone = cauder:eval_mult(Sem, Steps),
  cauder_wx_statusbar:multi(Sem, StepsDone, Steps),
  refresh(true),
  {noreply, State};

%% -------------------- Replay Actions -------------------- %%

handle_event(?BUTTON_EVENT(?ACTION_Replay_Steps_Button), State) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Spinner = utils_gui:find(?ACTION_Replay_Steps, wxSpinCtrl),
      Steps = wxSpinCtrl:getValue(Spinner),
      StepsDone = cauder:eval_replay(Pid, Steps),
      cauder_wx_statusbar:replay(StepsDone, Steps),
      refresh(true)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ACTION_Replay_Spawn_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Replay_Spawn, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Replay_Send_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Replay_Send, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Replay_Receive_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Replay_Receive, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Steps_Button), State) ->
  case cauder_wx_actions:selected_pid() of
    none ->
      cauder_wx_statusbar:no_process(),
      refresh(false);
    Pid ->
      Spinner = utils_gui:find(?ACTION_Rollback_Steps, wxSpinCtrl),
      Steps = wxSpinCtrl:getValue(Spinner),
      {FocusLog, StepsDone} = cauder:eval_roll(Pid, Steps),
      cauder_wx_statusbar:roll(StepsDone, Steps),
      cauder_wx_system:focus_roll_log(FocusLog),
      refresh(true)
  end,
  {noreply, State};

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Spawn_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Rollback_Spawn, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Send_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Rollback_Send, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Receive_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Rollback_Receive, wxTextCtrl),
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

handle_event(?BUTTON_EVENT(?ACTION_Rollback_Variable_Button), State) ->
  TextCtrl = utils_gui:find(?ACTION_Rollback_Variable, wxTextCtrl),
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

%% -------------------- Edit Binding -------------------- %%

handle_event(#wx{id = ?PROCESS_Bindings_Control, event = #wxList{type = command_list_item_activated, itemIndex = Index}, obj = BindingList}, #wx_state{frame = Frame} = State) ->
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

handle_event(#wx{id = ?ACTION_Automatic_Steps, event = #wxCommand{type = command_text_updated}}, State) ->
  refresh(false),
  {noreply, State};

handle_event(#wx{event = #wxCommand{type = command_text_updated}}, State) -> {noreply, State};

%% -------------------- DnD event -------------------- %%

handle_event(#wx{event = #wxDropFiles{files = Files}}, #wx_state{frame = Frame} = State) ->
  case cauder_wx_dialog:drop_files(Frame, Files) of
    {ok, File} ->
      case stop_session() of
        false -> ok;
        true -> open_file(File)
      end;
    false -> ok
  end,
  {noreply, State};

%% -------------------- Close event -------------------- %%

handle_event(#wx{event = #wxClose{}}, State) ->
  %% TODO Add confirmation dialog?
  {stop, normal, State};

%% -------------------- Unhandled events -------------------- %%

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
  CodeCtrl = utils_gui:find(?CODE_Code_Control, wxStyledTextCtrl),
  cauder_wx_code:load_code(CodeCtrl, <<Src/binary, 0:8>>),

  cauder_wx_menu:enable(?MENU_Run_Start, true),

  cauder_wx_statusbar:update("Loaded file " ++ File),

  Module.


%%--------------------------------------------------------------------
%% @doc Starts a new debugging session.

-spec start_session() -> Started :: boolean().

start_session() ->
  Frame = utils_gui:find(?FRAME, wxFrame),
  EntryPoints = cauder:get_entry_points(get(module)),

  MFA =
    case cauder_wx_dialog:start_session(Frame, EntryPoints) of
      {manual, {Mod, Fun, Args}} ->
        ok = cauder:init_system(Mod, Fun, Args),
        {Mod, Fun, length(Args)};
      {replay, Path} ->
        {_, _, _} = cauder:init_system(Path);
      false -> false
    end,

  case MFA of
    false -> false;
    {M, F, A} ->
      put(line, 0),

      refresh(true),

      cauder_wx_menu:enable(?MENU_Run_Start, false),
      cauder_wx_menu:enable(?MENU_Run_Stop, true),

      % Update status bar message
      cauder_wx_statusbar:update(io_lib:format("Started system with ~p:~p/~b fun application!", [M, F, A])),

      true
  end.


%%--------------------------------------------------------------------
%% @doc Stops the current debugging session.

-spec stop_session() -> Stopped :: boolean().

stop_session() ->
  case cauder:get_system() of
    undefined -> true; % Not running
    _ ->
      Frame = utils_gui:find(?FRAME, wxFrame),
      case cauder_wx_dialog:stop_session(Frame) of
        false -> false;
        true ->
          ok = cauder:stop_system(),

          refresh(true),

          cauder_wx_menu:enable(?MENU_Run_Start, true),
          cauder_wx_menu:enable(?MENU_Run_Stop, false),

          % Update status bar message
          cauder_wx_statusbar:update("Stopped system!"),

          true
      end
  end.


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
