-module(cauder_gui).
-export([init/0, ref_add/2, ref_lookup/1]).

-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


init() ->
  ref_start(),
  ref_add(?STATUS, #status{}),

  _ = wx:new(),

  Setup =
    fun() ->
      Frame = wxFrame:new(wx:null(), ?FRAME, ?APP_STRING, [{size, ?FRAME_SIZE_INIT}]),
      ref_add(?FRAME, Frame),

      menu_bar(Frame),
      main_area(Frame),
      status_bar(Frame),

      utils_gui:disable_all_inputs(),
      utils_gui:disable_all_buttons(),

      wxEvtHandler:connect(Frame, close_window),
      wxEvtHandler:connect(Frame, command_button_clicked),
      wxEvtHandler:connect(Frame, command_menu_selected),
      wxEvtHandler:connect(Frame, command_text_updated),

      wxFrame:show(Frame)
    end,

  wx:batch(Setup),

  loop(),

  utils_gui:stop_refs(),
  ref_stop(),
  wx:destroy().


%% ===== Menu Bar ===== %%


menu_bar(Frame) ->
  MenuBar = wxMenuBar:new(),
  wxFrame:setMenuBar(Frame, MenuBar),

  %% --------- File menu ---------- %%
  File = wxMenu:new(),
  wxMenuBar:append(MenuBar, File, "&File"),

  File_Open = wxMenu:append(File, ?OPEN, "&Open\tCtrl+O"),
  wxMenuItem:setHelp(File_Open, ?HELP_OPEN_ITEM),
  File_Replay = wxMenu:append(File, ?LOAD_TRACE, "Load &Trace\tCtrl+T"),
  wxMenuItem:setHelp(File_Replay, ?HELP_REPLAY_ITEM),
  wxMenuItem:enable(File_Replay, [{enable, false}]),
  ref_add(?LOAD_TRACE, File_Replay),

  wxMenu:appendSeparator(File),

  File_Quit = wxMenu:append(File, ?EXIT, "&Quit\tCtrl+Q"),
  wxMenuItem:setHelp(File_Quit, ?HELP_QUIT_ITEM),

  %% --------- View menu ---------- %%
  View = wxMenu:new(),
  wxMenuBar:append(MenuBar, View, "&View"),
  ref_add(?MENU_VIEW, View),

  View_ZoomIn = wxMenu:append(View, ?BUTTON_ZOOM_IN, "Zoom &In\tCtrl++"),
  wxMenuItem:setHelp(View_ZoomIn, ?HELP_ZOOM_IN_ITEM),
  wxMenuItem:enable(View_ZoomIn, [{enable, ?ZOOM_DEFAULT < ?ZOOM_MAX}]),
  ref_add(?BUTTON_ZOOM_IN, View_ZoomIn),
  View_ZoomOut = wxMenu:append(View, ?BUTTON_ZOOM_OUT, "Zoom &Out\tCtrl+-"),
  wxMenuItem:setHelp(View_ZoomOut, ?HELP_ZOOM_OUT_ITEM),
  wxMenuItem:enable(View_ZoomOut, [{enable, ?ZOOM_DEFAULT > ?ZOOM_MIN}]),
  ref_add(?BUTTON_ZOOM_OUT, View_ZoomOut),
  View_Zoom100 = wxMenu:append(View, ?BUTTON_ZOOM_100, "Zoom &100%\tCtrl+0"),
  wxMenuItem:setHelp(View_Zoom100, ?HELP_ZOOM_100_ITEM),
  ref_add(?BUTTON_ZOOM_100, View_Zoom100),

  wxMenu:appendSeparator(View),

  View_ToggleMail = wxMenu:appendCheckItem(View, ?TOGGLE_MAIL, "Show &Mailbox"),
  wxMenuItem:setHelp(View_ToggleMail, ?HELP_TOGGLE_MAIL),
  View_ToggleHist = wxMenu:appendCheckItem(View, ?TOGGLE_HIST, "Show &History"),
  wxMenuItem:setHelp(View_ToggleHist, ?HELP_TOGGLE_HIST),
  View_ToggleLog = wxMenu:appendCheckItem(View, ?TOGGLE_LOG, "Show &Log"),
  wxMenuItem:setHelp(View_ToggleLog, ?HELP_TOGGLE_LOG),
  View_ToggleStack = wxMenu:appendCheckItem(View, ?TOGGLE_STACK, "Show &Stack"),
  wxMenuItem:setHelp(View_ToggleStack, ?HELP_TOGGLE_STACK),
  View_ToggleEnv = wxMenu:appendCheckItem(View, ?TOGGLE_ENV, "Show &Environment"),
  wxMenuItem:setHelp(View_ToggleEnv, ?HELP_TOGGLE_ENV),
  View_ToggleExp = wxMenu:appendCheckItem(View, ?TOGGLE_EXP, "Show E&xpressions"),
  wxMenuItem:setHelp(View_ToggleExp, ?HELP_TOGGLE_EXP),

  wxMenu:appendSeparator(View),

  View_RadioConc = wxMenu:appendRadioItem(View, ?RADIO_CONC_HIST, "Concurrent History"),
  wxMenuItem:setHelp(View_RadioConc, ?HELP_RADIO_CONC),
  View_RadioFull = wxMenu:appendRadioItem(View, ?RADIO_FULL_HIST, "Full History"),
  wxMenuItem:setHelp(View_RadioFull, ?HELP_RADIO_FULL),

  wxMenu:appendSeparator(View),

  View_RadioRelEnv = wxMenu:appendRadioItem(View, ?RADIO_REL_ENV, "Relevant Environment"),
  wxMenuItem:setHelp(View_RadioRelEnv, ?HELP_RADIO_REN_ENV),
  View_RadioFullEnv = wxMenu:appendRadioItem(View, ?RADIO_FULL_ENV, "Full Environment"),
  wxMenuItem:setHelp(View_RadioFullEnv, ?HELP_RADIO_FULL_ENV),

  % Set default values
  wxMenuItem:check(View_ToggleMail),
  wxMenuItem:check(View_ToggleHist),
  wxMenuItem:check(View_ToggleLog),
  wxMenuItem:check(View_ToggleStack),
  wxMenuItem:check(View_ToggleEnv),
  wxMenuItem:check(View_ToggleExp),
  wxMenuItem:check(View_RadioConc),
  wxMenuItem:check(View_RadioRelEnv),

  %% --------- Compile menu ---------- %%
  Compile = wxMenu:new(),
  wxMenuBar:append(MenuBar, Compile, "&Compiler"),
  ref_add(?MENU_COMP, Compile),

  Compile_ToggleComp = wxMenu:appendCheckItem(Compile, ?TOGGLE_COMP, "Compiler &Optimizations"),
  wxMenuItem:setHelp(Compile_ToggleComp, ?HELP_TOGGLE_COMP),
  wxMenuItem:check(Compile_ToggleComp),

  %% --------- Help menu ---------- %%
  Help = wxMenu:new(),
  wxMenuBar:append(MenuBar, Help, "&Help"),

  wxMenu:append(Help, ?ABOUT, "&About").


%% ===== Main Area ===== %%


main_area(Parent) ->
  Panel = wxPanel:new(Parent),

  Border = wxBoxSizer:new(?wxVERTICAL),
  wxWindow:setSizer(Panel, Border),

  Content = wxBoxSizer:new(?wxHORIZONTAL),
  wxSizer:add(Border, Content, [{proportion, 1}, {flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_SMALL}]),

  % ----- Left ----- %

  Left = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Content, Left, [{proportion, 3}, {flag, ?wxEXPAND}]),

  wxSizer:add(Left, cauder_wx_code:code_area(Panel), [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(Left, ?SPACER_MEDIUM),
  wxSizer:add(Left, cauder_wx_process:process_info_area(Panel), [{proportion, 1}, {flag, ?wxEXPAND}]),

  % -----

  wxSizer:addSpacer(Content, ?SPACER_MEDIUM),

  % ----- Right ----- %

  Right = wxBoxSizer:new(?wxVERTICAL),
  wxSizer:add(Content, Right, [{proportion, 2}, {flag, ?wxEXPAND}]),

  wxSizer:add(Right, cauder_wx_actions:actions_area(Panel), [{proportion, 0}, {flag, ?wxEXPAND}]),
  wxSizer:addSpacer(Right, ?SPACER_MEDIUM),
  wxSizer:add(Right, cauder_wx_system:system_info_area(Panel), [{proportion, 1}, {flag, ?wxEXPAND}]),

  Panel.


%% ===== Status Bar ===== %%


status_bar(Frame) -> wxFrame:createStatusBar(Frame, [{id, ?STATUS_BAR}]).


%% =====================================================================


%% Loads the specified '.erl' source file
loadFile(File) ->
  utils_gui:stop_refs(),
  cauder:start_refs(),
  cauder:ref_add(?MODULE_PATH, filename:dirname(File)),

  {ok, Src, _} = erl_prim_loader:get_file(File),

  cauder_wx_code:load_code(ref_lookup(?CODE_TEXT), <<Src/binary, 0:8>>),

%%  FunNames = cauder:load_file(File),

  Status = ref_lookup(?STATUS),
  ref_add(?STATUS, Status#status{loaded = true}),
  wxMenuItem:enable(ref_lookup(?LOAD_TRACE), [{enable, true}]),

%%  utils_gui:set_choices(FunNames),
  utils_gui:disable_all_buttons(),
  utils_gui:clear_texts(),

  utils_gui:update_status_text("Loaded file " ++ File).


-spec loadReplayData(file:filename()) -> ok.

loadReplayData(Path) ->
%%  try
  utils:load_replay_data(Path),
  #replay{log_path = Path, call = {Mod, Fun, Args}, main_pid = Pid} = get(replay_data),
  Log = utils:get_log_data(Path, Pid),
  io:format("Log: ~p\n", [Log]),
  start(Mod, Fun, Args, Pid, Log).
%%  catch
%%    _:_ ->
%%      Frame = ref_lookup(?FRAME),
%%      wxFrame:setStatusText(Frame, "Error loading replay data")
%%  end.

openDialog(Parent) ->
  Message = "Select an Erlang file",
  Wildcard = "Erlang (*.erl)|*.erl|All files (*.*)|*.*",
  Dialog = wxFileDialog:new(Parent, [{message, Message},
                                     {wildCard, Wildcard},
                                     {style, ?wxFD_OPEN bor ?wxFD_FILE_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Dir = wxFileDialog:getDirectory(Dialog),
      Path = wxFileDialog:getPath(Dialog),
      ref_add(?LAST_PATH, Dir),
      loadFile(Path);
    _Other -> continue
  end,
  wxDialog:destroy(Dialog).

openReplayDialog(Parent) ->
  Title = "Select a log folder",
  DefaultPath = ref_lookup(?LAST_PATH),
  Dialog = wxDirDialog:new(Parent, [{title, Title},
                                    {defaultPath, DefaultPath},
                                    {style, ?wxDD_DIR_MUST_EXIST}]),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Path = wxDirDialog:getPath(Dialog),
      loadReplayData(Path);
    _Other -> continue
  end,
  wxDialog:destroy(Dialog).


%%--------------------------------------------------------------------
%% @doc Initializes the system.
%%
%% @param Fun Entry point of the system.
%% @param Args Arguments of the entry point.
%% @param Pid Pid for the new system.
%% @param Log Initial system log.
%% @end
%%--------------------------------------------------------------------
-spec init_system(Module, Function, Args, Pid, Log) -> no_return() when
  Module :: atom(),
  Function :: atom(),
  Args :: [cauder_types:abstract_expr()],
  Pid :: pos_integer(),
  Log :: list(). % TODO

init_system(M, F, As, Pid, Log) ->
  Proc = #proc{
    pid   = Pid,
    log   = Log,
    exprs = [{remote_call, 0, M, F, As}],
    spf   = {M, F, length(As)}
  },
  System = #sys{
    procs  = [{Pid, Proc}],
    ghosts = load_ghosts(Pid)
  },
  ref_add(?SYSTEM, System),

  put(line, 0),

  % Update system status
  Status = ref_lookup(?STATUS),
  NewStatus = Status#status{running = true},
  ref_add(?STATUS, NewStatus).


%% ---------------------------------------------------------------------
%% @doc Loads the replay data for all the processes in the current replay
%% data, except for the one with the MainPid, which has already been loaded.

-spec load_ghosts(MainPid :: pos_integer()) -> cauder_types:process_dict().

load_ghosts(MainPid) ->
  #replay{log_path = Path} = get(replay_data),
  {ok, Filenames} = file:list_dir(Path),
  Ghosts =
    lists:filtermap(
      fun(Filename) ->
        case re:run(Filename, "trace_(\\d+)\\.log", [{capture, [1], list}]) of
          {match, [StrPid]} ->
            case list_to_integer(StrPid) of
              MainPid -> false;
              Pid ->
                Proc = #proc{
                  pid = Pid,
                  log = utils:get_log_data(Path, Pid)
                },
                {true, {Pid, Proc}}
            end;
          nomatch -> false
        end
      end, Filenames),
  orddict:from_list(Ghosts).

%%--------------------------------------------------------------------
%% @doc Starts a new system.
%%
%% @param Fun Entry point of the system.
%% @param Args Arguments of the entry point.
%% @end
%%--------------------------------------------------------------------

-spec start(Module, Function, Args) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()].

start(M, F, As) -> start(M, F, As, 1, []).


%%--------------------------------------------------------------------
%% @doc Starts a new system.
%%
%% @param Fun Entry point function of the system.
%% @param Args Arguments of the entry point.
%% @param Pid Initial pid for the new system.
%% @param Log Initial system log.
%% @end
%%--------------------------------------------------------------------

-spec start(Module, Function, Args, Pid, Log) -> ok when
  Module :: atom(),
  Function :: atom(),
  Args :: [erl_parse:abstract_expr()],
  Pid :: pos_integer(),
  Log :: list(). % TODO

start(M, F, As, Pid, Log) ->
  cauder:reset_fresh_refs(Pid),
  init_system(M, F, As, Pid, Log),
  refresh(true),

  % Update status bar message
  StatusString = "Started system with " ++ atom_to_list(F) ++ "/" ++ integer_to_list(length(As)) ++ " fun application!",
  utils_gui:update_status_text(StatusString).


refresh_buttons(Opts) ->
  ?LOG("full options: " ++ ?TO_STRING(utils_gui:sort_opts(Opts))),

  case utils_gui:current_process() of
    none ->
      utils_gui:disable_controls([?SINGLE_BACKWARD_BUTTON, ?SINGLE_FORWARD_BUTTON]);
    #proc{pid = Pid} ->
      Buttons = lists:map(fun utils_gui:option_to_button_label/1, utils:filter_options(Opts, Pid)),
      utils_gui:set_button_label_from(?SINGLE_BACKWARD_BUTTON, Buttons),
      utils_gui:set_button_label_from(?SINGLE_FORWARD_BUTTON, Buttons)
  end,

  HasFwd = utils:has_fwd(Opts),
  HasBwd = utils:has_bwd(Opts),

  utils_gui:enable_control_if(?STEPS_SPIN, HasFwd orelse HasBwd),
  utils_gui:enable_control_if(?MULTIPLE_FORWARD_BUTTON, HasFwd),
  utils_gui:enable_control_if(?MULTIPLE_BACKWARD_BUTTON, HasBwd).


-spec refresh(boolean()) -> ok.

refresh(RefreshState) ->
  case utils_gui:is_app_running() of
    false -> ok;
    true ->
      System = ref_lookup(?SYSTEM),
      Options = cauder:eval_opts(System),
      Update =
        fun() ->
          if
            RefreshState ->
              utils_gui:update_process_choices(System),
              utils_gui:update_code_line(),
              cauder_wx_system:update_system_info(),
              cauder_wx_process:update_process_info();
            true -> ok
          end,
          refresh_buttons(Options),
          utils_gui:enable_replay(), % TODO Enable only if it is possible to replay
          utils_gui:enable_roll() % TODO Enable only if it is possible to rollback
        end,

      wx:batch(Update)
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


%% ==================== Manual evaluation ==================== %%


-spec exec_with(?SINGLE_FORWARD_BUTTON | ?SINGLE_BACKWARD_BUTTON) -> ok.

exec_with(Button) ->
  case utils_gui:current_process() of
    none -> ok;
    #proc{pid = Pid} ->
      Sys0 = ref_lookup(?SYSTEM),
      Opt0 = utils_gui:button_to_option(Button),
      Opt1 = Opt0#opt{id = Pid},
      Sys1 = cauder:eval_step(Sys0, Opt1),
      ref_add(?SYSTEM, Sys1),
      ok
  end.


%% ==================== Automatic evaluation ==================== %%


-spec eval_mult(Button) -> {StepsDone, Steps} when
  Button :: ?MULTIPLE_FORWARD_BUTTON | ?MULTIPLE_BACKWARD_BUTTON,
  StepsDone :: non_neg_integer(),
  Steps :: pos_integer().

eval_mult(Button) ->
  Steps = wxSpinCtrl:getValue(ref_lookup(?STEPS_SPIN)),
  Sys0 = ref_lookup(?SYSTEM),
  Dir =
    case Button of
      ?MULTIPLE_FORWARD_BUTTON -> ?MULT_FWD;
      ?MULTIPLE_BACKWARD_BUTTON -> ?MULT_BWD
    end,
  {Sys1, StepsDone} = cauder:eval_mult(Sys0, Dir, Steps),
  ref_add(?SYSTEM, Sys1),
  {StepsDone, Steps}.


%% ==================== Replay evaluation ==================== %%


-spec eval_replay() -> {StepsDone :: non_neg_integer(), Steps :: non_neg_integer()}.

eval_replay() ->
  case utils_gui:current_process() of
    none -> {0, 0};
    #proc{pid = Pid} ->
      Steps = wxSpinCtrl:getValue(ref_lookup(?REPLAY_STEPS_SPIN)),
      Sys0 = ref_lookup(?SYSTEM),
      {Sys1, StepsDone} = cauder:eval_replay(Sys0, Pid, Steps),
      ref_add(?SYSTEM, Sys1),
      {StepsDone, Steps}
  end.


-spec eval_replay_spawn() -> {Success :: boolean(), SpawnPid :: none | string()}.

eval_replay_spawn() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?REPLAY_SPAWN_TEXT)),
  case string:to_integer(PidText) of
    % What if error?
    {error, _} -> {false, none};
    {SpawnPid, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {Success, Sys1} = cauder:eval_replay_spawn(Sys0, SpawnPid),
      ref_add(?SYSTEM, Sys1),
      {Success, PidText}
  end.


-spec eval_replay_send() -> {Success :: boolean(), Id :: none | string()}.

eval_replay_send() ->
  IdText = wxTextCtrl:getValue(ref_lookup(?REPLAY_SEND_TEXT)),
  case string:to_integer(IdText) of
    % What if error?
    {error, _} -> {false, none};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {Success, Sys1} = cauder:eval_replay_send(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {Success, IdText}
  end.


-spec eval_replay_rec() -> {Success :: boolean(), Id :: none | string()}.

eval_replay_rec() ->
  IdText = wxTextCtrl:getValue(ref_lookup(?REPLAY_REC_TEXT)),
  case string:to_integer(IdText) of
    % What if error?
    {error, _} -> {false, none};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {Success, Sys1} = cauder:eval_replay_rec(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {Success, IdText}
  end.


%% ==================== Rollback evaluation ==================== %%


-spec eval_roll() -> {FocusLog, StepsDone, Steps} when
  FocusLog :: boolean(),
  StepsDone :: non_neg_integer(),
  Steps :: pos_integer().

eval_roll() ->
  case utils_gui:current_process() of
    none -> {false, 0, 0};
    #proc{pid = Pid} ->
      Steps = wxSpinCtrl:getValue(ref_lookup(?ROLL_STEPS_SPIN)),
      Sys0 = ref_lookup(?SYSTEM),
      {FocusLog, Sys1, StepsDone} = cauder:eval_roll(Sys0, Pid, Steps),
      ref_add(?SYSTEM, Sys1),
      {FocusLog, StepsDone, Steps}
  end.


-spec eval_roll_spawn() -> {CanRoll, SpawnPid, FocusLog} when
  CanRoll :: boolean(),
  SpawnPid :: string() | none,
  FocusLog :: boolean().

eval_roll_spawn() ->
  PidText = wxTextCtrl:getValue(ref_lookup(?ROLL_SPAWN_TEXT)),
  case string:to_integer(PidText) of
    {error, _} -> {false, none, false};
    {Pid, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_spawn(Sys0, Pid),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, PidText, FocusLog}
  end.


-spec eval_roll_send() -> {CanRoll, Id, FocusLog} when
  CanRoll :: boolean(),
  Id :: string() | none,
  FocusLog :: boolean().

eval_roll_send() ->
  IdTextCtrl = ref_lookup(?ROLL_SEND_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case string:to_integer(IdText) of
    {error, _} -> {false, none, false};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_send(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, IdText, FocusLog}
  end.


-spec eval_roll_rec() -> {CanRoll, Id, FocusLog} when
  CanRoll :: boolean(),
  Id :: string() | none,
  FocusLog :: boolean().

eval_roll_rec() ->
  IdTextCtrl = ref_lookup(?ROLL_REC_TEXT),
  IdText = wxTextCtrl:getValue(IdTextCtrl),
  case string:to_integer(IdText) of
    {error, _} -> {false, none, false};
    {Id, _} ->
      Sys0 = ref_lookup(?SYSTEM),
      {CanRoll, FocusLog, Sys1} = cauder:eval_roll_rec(Sys0, Id),
      ref_add(?SYSTEM, Sys1),
      {CanRoll, IdText, FocusLog}
  end.


-spec eval_roll_var() -> {CanRoll, Name, FocusLog} when
  CanRoll :: boolean(),
  Name :: string() | none,
  FocusLog :: boolean().

eval_roll_var() ->
  TextCtrl = ref_lookup(?ROLL_VAR_TEXT),
  case wxTextCtrl:getValue(TextCtrl) of
    "" -> {false, none, false};
    Text ->
      System = ref_lookup(?SYSTEM),
      VarName = list_to_atom(Text),
      {CanRoll, FocusLog, NewSystem} = cauder:eval_roll_var(System, VarName),
      ref_add(?SYSTEM, NewSystem),
      {CanRoll, Text, FocusLog}
  end.

focus_roll_log(false) -> ok;
focus_roll_log(true) ->
  RBotNotebook = ref_lookup(?SYSTEM_INFO_NOTEBOOK),
  wxNotebook:setSelection(RBotNotebook, ?PAGEPOS_ROLL).


loop() ->
  Result =
    receive
    %% ---------- File menu ---------- %%
      #wx{id = ?OPEN, event = #wxCommand{type = command_menu_selected}} -> openDialog(ref_lookup(?FRAME));
      #wx{id = ?LOAD_TRACE, event = #wxCommand{type = command_menu_selected}} -> openReplayDialog(ref_lookup(?FRAME));
      #wx{id = ?EXIT, event = #wxCommand{type = command_menu_selected}} -> exit;

    %% ---------- View menu ---------- %%
      #wx{id = ?BUTTON_ZOOM_IN, event = #wxCommand{type = command_menu_selected}} -> cauder_wx_code:zoom_in(ref_lookup(?CODE_TEXT));
      #wx{id = ?BUTTON_ZOOM_OUT, event = #wxCommand{type = command_menu_selected}} -> cauder_wx_code:zoom_out(ref_lookup(?CODE_TEXT));
      #wx{id = ?BUTTON_ZOOM_100, event = #wxCommand{type = command_menu_selected}} -> cauder_wx_code:zoom_reset(ref_lookup(?CODE_TEXT));
%%      #wx{id = ?TOGGLE_MAIL, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
%%      #wx{id = ?TOGGLE_LOG, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
%%      #wx{id = ?TOGGLE_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
%%      #wx{id = ?TOGGLE_STACK, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
%%      #wx{id = ?TOGGLE_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
%%      #wx{id = ?TOGGLE_EXP, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
      #wx{id = ?RADIO_CONC_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
      #wx{id = ?RADIO_FULL_HIST, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
      #wx{id = ?RADIO_REL_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);
      #wx{id = ?RADIO_FULL_ENV, event = #wxCommand{type = command_menu_selected}} -> refresh(true);

    %% ---------- Compiler menu ---------- %%
      #wx{id = ?TOGGLE_COMP, event = #wxCommand{type = command_menu_selected}} -> utils_gui:sttext_comp();

    %% ---------- Help menu ---------- %%
      #wx{id = ?ABOUT, event = #wxCommand{type = command_menu_selected}} ->
        Caption = "About " ++ ?APP_STRING,
        Dialog = wxMessageDialog:new(ref_lookup(?FRAME), ?INFO_TEXT, [{style, ?wxOK}, {caption, Caption}]),
        wxDialog:showModal(Dialog),
        wxWindow:destroy(Dialog);

    %% ---------- Code area ---------- %%
      #wx{id = ?CODE_TEXT, event = #wxStyledText{type = stc_zoom}, obj = CodeArea} ->
        cauder_wx_code:update_margin(CodeArea),
        cauder_wx_code:update_zoom_buttons(CodeArea);

    %% ---------- Start button ---------- %%
%%      #wx{id = ?START_BUTTON, event = #wxCommand{type = command_button_clicked}} -> start();

    %% ---------- Process selector ---------- %%
      #wx{id = ?PROC_CHOICE, event = #wxCommand{type = command_choice_selected}} -> refresh(true);

    %% ---------- Manual panel buttons ---------- %%
      #wx{id = Button, event = #wxCommand{type = command_button_clicked}}
        when (Button =:= ?SINGLE_FORWARD_BUTTON) orelse (Button =:= ?SINGLE_BACKWARD_BUTTON) ->
        utils_gui:disable_all_buttons(),
        exec_with(Button),
        utils_gui:sttext_single(Button),
        refresh(true);

    %% ---------- Automatic panel buttons ---------- %%
      #wx{id = Button, event = #wxCommand{type = command_button_clicked}}
        when (Button =:= ?MULTIPLE_FORWARD_BUTTON) orelse (Button =:= ?MULTIPLE_BACKWARD_BUTTON) ->
        utils_gui:disable_all_buttons(),
        {StepsDone, TotalSteps} = eval_mult(Button),
        utils_gui:sttext_mult(StepsDone, TotalSteps),
        refresh(true);

    %% ---------- Replay panel buttons ---------- %%
      #wx{id = ?REPLAY_STEPS_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {StepsDone, TotalSteps} = eval_replay(),
        utils_gui:sttext_replay(StepsDone, TotalSteps),
        refresh(true);
      #wx{id = ?REPLAY_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasReplayed, SpawnPid} = eval_replay_spawn(),
        utils_gui:sttext_replay_spawn(HasReplayed, SpawnPid),
        refresh(HasReplayed);
      #wx{id = ?REPLAY_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasReplayed, SendId} = eval_replay_send(),
        utils_gui:sttext_replay_send(HasReplayed, SendId),
        refresh(HasReplayed);
      #wx{id = ?REPLAY_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasReplayed, RecId} = eval_replay_rec(),
        utils_gui:sttext_replay_rec(HasReplayed, RecId),
        refresh(HasReplayed);

    %% ---------- Rollback panel buttons ---------- %%
      #wx{id = ?ROLL_STEPS_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {MustFocus, StepsDone, TotalSteps} = eval_roll(),
        utils_gui:sttext_roll(StepsDone, TotalSteps),
        focus_roll_log(MustFocus),
        refresh(true);
      #wx{id = ?ROLL_SPAWN_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasRolled, SpawnPid, MustFocus} = eval_roll_spawn(),
        utils_gui:sttext_roll_spawn(HasRolled, SpawnPid),
        focus_roll_log(MustFocus),
        refresh(HasRolled);
      #wx{id = ?ROLL_SEND_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasRolled, SendId, MustFocus} = eval_roll_send(),
        utils_gui:sttext_roll_send(HasRolled, SendId),
        focus_roll_log(MustFocus),
        refresh(HasRolled);
      #wx{id = ?ROLL_REC_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasRolled, RecId, MustFocus} = eval_roll_rec(),
        utils_gui:sttext_roll_rec(HasRolled, RecId),
        focus_roll_log(MustFocus),
        refresh(HasRolled);
      #wx{id = ?ROLL_VAR_BUTTON, event = #wxCommand{type = command_button_clicked}} ->
        utils_gui:disable_all_buttons(),
        {HasRolled, VarId, MustFocus} = eval_roll_var(),
        utils_gui:sttext_roll_var(HasRolled, VarId),
        focus_roll_log(MustFocus),
        refresh(HasRolled);

    %% ---------- Bindings handler ---------- %%
      #wx{id = ?BINDINGS_LIST, event = #wxList{type = command_list_item_activated, itemIndex = Index}} ->
        P0 = #proc{pid = Pid, env = Bs0, exprs = Es} = utils_gui:current_process(),
        {Key, Value} =
          case wxMenu:isChecked(ref_lookup(?MENU_VIEW), ?RADIO_FULL_ENV) of
            true -> lists:nth(Index + 1, Bs0);
            false -> lists:nth(Index + 1, pretty_print:relevant_bindings(Bs0, Es))
          end,
        case cauder_wx_dialog:edit_binding(ref_lookup(?FRAME), {Key, Value}) of
          {Key, NewValue} ->
            Bs1 = orddict:store(Key, NewValue, Bs0),
            Sys0 = ref_lookup(?SYSTEM),
            PDict0 = Sys0#sys.procs,
            P1 = P0#proc{env = Bs1},
            PDict1 = orddict:store(Pid, P1, PDict0),
            Sys1 = Sys0#sys{procs = PDict1},
            ref_add(?SYSTEM, Sys1),
            refresh(true);
          cancel -> ok
        end;

    %% ---------- Text handlers ---------- %%
      #wx{id = ?STEPS_SPIN, event = #wxCommand{type = command_text_updated}} -> refresh(false);
      #wx{id = _RestIds, event = #wxCommand{type = command_text_updated}} -> ok;

    %% ---------- Other handlers ---------- %%
      #wx{id = ?FRAME, event = #wxClose{type = close_window}} -> exit;

    %% ---------- Non-supported events ---------- %%
      Other -> io:format("main loop does not implement ~p~n", [Other])
    end,
  case Result of
    ok -> loop();
    exit -> wxFrame:destroy(ref_lookup(?FRAME))
  end.

ref_start() ->
  ?GUI_REF = ets:new(?GUI_REF, [set, public, named_table]),
  ok.

ref_stop() ->
  true = ets:delete(?GUI_REF),
  ok.

ref_add(Id, Ref) ->
  true = ets:insert(?GUI_REF, {Id, Ref}),
  ok.

ref_lookup(Id) -> ets:lookup_element(?GUI_REF, Id, 2).
