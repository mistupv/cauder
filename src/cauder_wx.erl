-module(cauder_wx).

-behaviour(wx_object).

%% API
-export([start/0, start_link/0, stop/0]).

%% wx_object callbacks
-export([init/1, handle_event/2, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

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

-define(MENU_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_menu_selected}}).
-define(BUTTON_EVENT(Id), #wx{id = Id, event = #wxCommand{type = command_button_clicked}}).


%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> wxWindow:wxWindow() | {'error', any()}.
start() -> wx_object:start(?MODULE, [], []).

-spec start_link() -> wxWindow:wxWindow() | {'error', any()}.
start_link() -> wx_object:start_link(?MODULE, [], []).

-spec stop() -> 'ok'.
stop() -> wx_object:stop(?MODULE).


%%%===================================================================
%%% wx_object callbacks
%%%===================================================================

-spec init(Args :: list()) -> {wxFrame:wxFrame(), state()}.

init([]) ->
  %% ----------
  %% TODO Move to 'cauder' process, once fully decoupled
  Db = ets:new(?MODULE, [set, public, named_table]),
  put(db, Db),
  put(status, #status{}),
  %% ----------

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
  EntryPoints = cauder:entry_points(Module),
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
      Rule = cauder:eval_reduce(Sem, Pid),
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
      case cauder:eval_step(Sem, Pid) of
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
  Sys = cauder:system(),
  Pid = cauder_wx_actions:selected_pid(),
  {ok, #proc{env = Bs}} = orddict:find(Pid, Sys#sys.procs),
  Nth = wxListCtrl:getItemData(BindingList, Index),
  {Key, Value} = lists:nth(Nth, Bs),
  case cauder_wx_dialog:edit_binding(Frame, {Key, Value}) of
    {Key, NewValue} ->
      cauder:edit_binding(Pid, {Key, NewValue}),
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

  %% ----------
  %% TODO Move to 'cauder' process, once fully decoupled
  ets:delete(?MODULE),
  %% ----------

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
  %utils_gui:stop_refs(), % TODO

  {ok, Module} = cauder:load_file(File),

  {ok, Src, _} = erl_prim_loader:get_file(File),
  CodeCtrl = utils_gui:find(?CODE_TEXT, wxStyledTextCtrl),
  cauder_wx_code:load_code(CodeCtrl, <<Src/binary, 0:8>>),

  cauder_wx_menu:enable(?MENU_Run_Start, true),

  cauder_wx_statusbar:update("Loaded file " ++ File),

  Module.


%%--------------------------------------------------------------------
%% @doc Starts a new manual debugging session with the given function application.

-spec start_manual_session(Module, Function, Args) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()].

start_manual_session(M, F, As) -> start_session(M, F, As, 1, []).


%%--------------------------------------------------------------------
%% @doc Starts a new replay debugging session using the trace in the given directory.

-spec start_replay_session(file:filename()) -> ok.

start_replay_session(Path) ->
%%  try
  utils:load_replay_data(Path),
  #replay{log_path = Path, call = {Mod, Fun, Args}, main_pid = Pid} = get(replay_data),
  Log = utils:get_log_data(Path, Pid),
  io:format("Log: ~p\n", [Log]),
  start_session(Mod, Fun, Args, Pid, Log).
%%  catch
%%    _:_ ->
%%      Frame = ref_lookup(?FRAME),
%%      wxFrame:setStatusText(Frame, "Error loading replay data")
%%  end.


%%--------------------------------------------------------------------
%% @doc Starts a new debugging session.

-spec start_session(Module, Function, Args, Pid, Log) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()],
  Pid :: cauder_types:proc_id(),
  Log :: cauder_types:log().

start_session(M, F, As, Pid, Log) ->
  cauder:init_system(M, F, As, Pid, Log),

  refresh(true),

  cauder_wx_menu:enable(?MENU_Run_Start, false),
  cauder_wx_menu:enable(?MENU_Run_Stop, true),

  % Update status bar message
  StatusString = "Started system with " ++ atom_to_list(F) ++ "/" ++ integer_to_list(length(As)) ++ " fun application!",
  cauder_wx_statusbar:update(StatusString).


stop_session() ->
  cauder:stop_system(),

  refresh(true),

  cauder_wx_menu:enable(?MENU_Run_Start, true),
  cauder_wx_menu:enable(?MENU_Run_Stop, false).




-spec refresh(boolean()) -> ok.

refresh(RefreshState) ->
  % TODO Remove argument
  System = cauder:system(),
  case RefreshState of
    false -> ok;
    true ->
      % First update actions so a process is selected
      cauder_wx_actions:update(System),
      cauder_wx_system:update(System),

      Process =
        case cauder_wx_actions:selected_pid() of
          none -> undefined;
          Pid ->
            {ok, Proc} = orddict:find(Pid, System#sys.procs),
            Proc
        end,

      cauder_wx_code:update(Process),
      cauder_wx_process:update(Process)
  end.

%%start() ->
%%  InputText = wxTextCtrl:getValue(ref_lookup(?ARGS_TEXT)),
%%  SelectedFun = wxChoice:getStringSelection(ref_lookup(?FUN_CHOICE)),
%%  {M, F, A} = utils:stringToMFA(SelectedFun),
%%  As = utils:stringToArgs(InputText),
%%  case A == length(As) of
%%    true ->
%%      start(M, F, As),
%%      ?LOG("start fun " ++ SelectedFun ++ " with args " ++ InputText);
%%    false ->
%%      utils_gui:update_status_text(?ERROR_NUM_ARGS),
%%      error
%%  end.


-spec button_to_semantics(ButtonId) -> Semantics when
  ButtonId :: ?STEP_FORWARD_BUTTON | ?STEP_BACKWARD_BUTTON |
  ?STEP_OVER_FORWARD_BUTTON | ?STEP_OVER_BACKWARD_BUTTON |
  ?STEP_INTO_FORWARD_BUTTON | ?STEP_INTO_BACKWARD_BUTTON |
  ?MULTIPLE_FORWARD_BUTTON | ?MULTIPLE_BACKWARD_BUTTON,
  Semantics :: ?FWD_SEM | ?BWD_SEM.

button_to_semantics(?STEP_FORWARD_BUTTON)       -> ?FWD_SEM;
button_to_semantics(?STEP_BACKWARD_BUTTON)      -> ?BWD_SEM;
button_to_semantics(?STEP_OVER_FORWARD_BUTTON)  -> ?FWD_SEM;
button_to_semantics(?STEP_OVER_BACKWARD_BUTTON) -> ?BWD_SEM;
button_to_semantics(?STEP_INTO_FORWARD_BUTTON)  -> ?FWD_SEM;
button_to_semantics(?STEP_INTO_BACKWARD_BUTTON) -> ?BWD_SEM;
button_to_semantics(?MULTIPLE_FORWARD_BUTTON)   -> ?FWD_SEM;
button_to_semantics(?MULTIPLE_BACKWARD_BUTTON)  -> ?BWD_SEM.
