-module(cauder_wx_dialog).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").

%% API
-export([start_session/2, stop_session/1]).
-export([edit_binding/2]).
-export([about/1]).


-spec start_session(Parent, EntryPoints) -> Result when
  Parent :: wxWindow:wxWindow(),
  EntryPoints :: list({Module, Function, Arity}),
  Result :: {manual, {Module, Function, Args}} | {replay, TracePath} | false,
  Module :: atom(),
  Function :: atom(),
  Arity :: arity(),
  Args :: [erl_parse:abstract_expr()],
  TracePath :: file:filename().

start_session(Parent, MFAs) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, ?DIALOG_StartSession_Title),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxDialog:setSizer(Dialog, Content),

  %% ----- Content ----- %%

  Grid = wxFlexGridSizer:new(2, 2, ?SPACER_LARGE, ?SPACER_MEDIUM),
  wxBoxSizer:add(Content, Grid, [{flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_LARGE}]),

  %% Manual Radio

  ManualRadio = wxRadioButton:new(Dialog, ?wxID_ANY, "Manual"),
  wxFlexGridSizer:add(Grid, ManualRadio, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

  %% Manual Panel

  ManualPanel = wxPanel:new(Dialog),
  wxFlexGridSizer:add(Grid, ManualPanel),

  ManualSizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(ManualPanel, ManualSizer),

  FunChoice = wxChoice:new(ManualPanel, ?wxID_ANY, [{size, {250, -1}}]),
  wxBoxSizer:add(ManualSizer, FunChoice),

  lists:foreach(fun({M, F, A} = MFA) -> wxChoice:append(FunChoice, io_lib:format("~p:~p/~b", [M, F, A]), MFA) end, MFAs),

  case lists:keyfind(start, 2, MFAs) of
    {M, F, A} -> wxChoice:setStringSelection(FunChoice, io_lib:format("~p:~p/~b", [M, F, A]));
    false ->
      case lists:keyfind(main, 2, MFAs) of
        {M, F, A} -> wxChoice:setStringSelection(FunChoice, io_lib:format("~p:~p/~b", [M, F, A]));
        false -> wxChoice:setSelection(FunChoice, 0)
      end
  end,

  wxBoxSizer:addSpacer(ManualSizer, ?SPACER_SMALL),

  ArgsCtrl = wxTextCtrl:new(ManualPanel, ?wxID_ANY, [{size, {250, -1}}]),
  wxBoxSizer:add(ManualSizer, ArgsCtrl),

  Index = wxChoice:getSelection(FunChoice),
  {_, _, Arity} = wxChoice:getClientData(FunChoice, Index),
  wxTextCtrl:enable(ArgsCtrl, [{enable, Arity > 0}]),

  %% Replay Radio

  ReplayRadio = wxRadioButton:new(Dialog, ?wxID_ANY, "Replay"),
  wxFlexGridSizer:add(Grid, ReplayRadio, [{flag, ?wxALIGN_CENTER_VERTICAL}]),

  %% Replay Panel

  ReplayPanel = wxPanel:new(Dialog),
  wxFlexGridSizer:add(Grid, ReplayPanel, [{flag, ?wxEXPAND}]),

  ReplaySizer = wxBoxSizer:new(?wxHORIZONTAL),
  wxPanel:setSizer(ReplayPanel, ReplaySizer),

  BasePath = cauder:get_path(),
  TracePath = filename:join(BasePath, "trace"),
  PickerPath =
    case filelib:is_dir(TracePath) of
      true -> TracePath;
      false -> BasePath
    end,
  PickerOpts = [{message, "Select a log folder"},
                {style, ?wxDIRP_DIR_MUST_EXIST bor ?wxDIRP_USE_TEXTCTRL}],

  TracePicker = wxDirPickerCtrl:new(ReplayPanel, ?wxID_ANY, [{path, PickerPath} | PickerOpts]),
  wxBoxSizer:add(ReplaySizer, TracePicker, [{proportion, 1}, {flag, ?wxEXPAND}]),
  wxDirPickerCtrl:setTextCtrlGrowable(TracePicker),

  %% -----

  Separator = wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]),
  wxBoxSizer:add(Content, Separator, [{flag, ?wxEXPAND}]),

  %% ----- Buttons ----- %%

  ReturnPid = self(),

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Buttons, [{flag, ?wxALIGN_RIGHT bor ?wxALL}, {border, ?SPACER_LARGE}]),

  StartButton = wxButton:new(Dialog, ?wxID_OK, [{label, "Start"}]),
  wxBoxSizer:add(Buttons, StartButton),

  wxButton:connect(
    StartButton,
    command_button_clicked,
    [{callback,
      fun(_, _) ->
        case wxRadioButton:getValue(ManualRadio) of
          true ->
            {M1, F1, A1} = wxChoice:getClientData(FunChoice, wxChoice:getSelection(FunChoice)),
            case utils:stringToExprs(wxTextCtrl:getValue(ArgsCtrl)) of
              error ->
                Message = ?DIALOG_BadArgs_Message,
                Options = [{caption, ?DIALOG_BadArgs_Title}, {style, ?wxICON_ERROR}],
                wxMessageDialog:showModal(wxMessageDialog:new(Dialog, Message, Options));
              Args ->
                case length(Args) of
                  A1 ->
                    ReturnPid ! {manual, {M1, F1, Args}};
                  Count ->
                    Message = io_lib:format(?DIALOG_StartSession_ArgCount_Message, [A1, Count]),
                    Options = [{caption, ?DIALOG_StartSession_ArgCount_Title}, {style, ?wxICON_WARNING}],
                    wxMessageDialog:showModal(wxMessageDialog:new(Dialog, Message, Options))
                end
            end;
          false ->
            ReturnPid ! {replay, wxDirPickerCtrl:getPath(TracePicker)}
        end
      end}]),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  CancelButton = wxButton:new(Dialog, ?wxID_CANCEL, [{label, "Cancel"}]),
  wxBoxSizer:add(Buttons, CancelButton),

  wxButton:connect(CancelButton, command_button_clicked, [{callback, fun(_, _) -> ReturnPid ! false end}]),

  %% -----

  fun_args_event_handler(FunChoice, ArgsCtrl),
  radio_event_handler(orddict:from_list([{ManualRadio, ManualPanel}, {ReplayRadio, ReplayPanel}])),

  wxWindow:enable(ReplayPanel, [{enable, false}]),

  %% -----

  wxDialog:fit(Dialog),
  wxDialog:show(Dialog),
  receive
    Return ->
      wxDialog:destroy(Dialog),
      Return
  end.


fun_args_event_handler(Choice, TextCtrl) ->
  Callback =
    fun(_, _) ->
      Index = wxChoice:getSelection(Choice),
      {_, _, Arity} = wxChoice:getClientData(Choice, Index),
      wxTextCtrl:enable(TextCtrl, [{enable, Arity > 0}])
    end,
  wxChoice:connect(Choice, command_choice_selected, [{callback, Callback}]).


radio_event_handler(RadioPanels) ->
  lists:foreach(
    fun({Radio, ThisPanel}) ->
      {ThisPanel, OtherPanels} = orddict:take(Radio, RadioPanels),
      Callback =
        fun(_, _) ->
          wxWindow:enable(ThisPanel, [{enable, true}]),
          lists:foreach(fun({_, Panel}) -> wxWindow:enable(Panel, [{enable, false}]) end, OtherPanels)
        end,
      wxRadioButton:connect(Radio, command_radiobutton_selected, [{callback, Callback}])
    end,
    RadioPanels
  ).


-spec stop_session(Parent :: wxWindow:wxWindow()) -> boolean().

stop_session(Parent) ->
  Options = [{style, ?wxICON_EXCLAMATION bor ?wxYES_NO bor ?wxNO_DEFAULT}, {caption, ?DIALOG_StopSession_Title}],
  Dialog = wxMessageDialog:new(Parent, ?DIALOG_StopSession_Message, Options),

  case wxDialog:showModal(Dialog) of
    ?wxID_YES -> true;
    _ -> false
  end.


-spec edit_binding(Parent, Binding) -> NewBinding | cancel when
  Parent :: wxWindow:wxWindow(),
  Binding :: cauder_types:binding(),
  NewBinding :: cauder_types:binding().

edit_binding(Parent, {Key, Value}) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Edit binding"),

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxDialog:setSizer(Dialog, Content),

  %% ----- Input ----- %%

  Input = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Input, [{flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_LARGE}]),

  %% Name

  NameSizer = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(Input, NameSizer, [{proportion, 1}]),

  NameStatic = wxStaticText:new(Dialog, ?wxID_ANY, "Name"),
  wxBoxSizer:add(NameSizer, NameStatic),

  wxBoxSizer:addSpacer(NameSizer, ?SPACER_SMALL),

  NameText = wxTextCtrl:new(Dialog, ?wxID_ANY, [{value, atom_to_list(Key)}, {style, ?wxTE_READONLY}]),
  wxBoxSizer:add(NameSizer, NameText, [{flag, ?wxEXPAND}]),

  %% -----

  wxBoxSizer:addSpacer(Input, ?SPACER_MEDIUM),

  %% Value

  ValueSizer = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(Input, ValueSizer, [{proportion, 2}]),

  ValueStatic = wxStaticText:new(Dialog, ?wxID_ANY, "Value"),
  wxBoxSizer:add(ValueSizer, ValueStatic),

  wxBoxSizer:addSpacer(ValueSizer, ?SPACER_SMALL),

  ValueText = wxTextCtrl:new(Dialog, ?wxID_ANY, [{value, io_lib:format("~p", [Value])}]),
  wxBoxSizer:add(ValueSizer, ValueText, [{flag, ?wxEXPAND}]),

  %% -----

  Separator = wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]),
  wxBoxSizer:add(Content, Separator, [{flag, ?wxEXPAND}]),

  %% ----- Buttons ----- %%

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Content, Buttons, [{flag, ?wxALIGN_RIGHT bor ?wxALL}, {border, ?SPACER_LARGE}]),

  SaveButton = wxButton:new(Dialog, ?wxID_OK, [{label, "Save"}]),
  wxBoxSizer:add(Buttons, SaveButton),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  CancelButton = wxButton:new(Dialog, ?wxID_CANCEL, [{label, "Cancel"}]),
  wxBoxSizer:add(Buttons, CancelButton),

  %% -----

  wxDialog:fit(Dialog),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Str = wxTextCtrl:getValue(ValueText),
      case utils:stringToExprs(Str) of
        [{value, _, NewValue}] -> {Key, NewValue};
        _ -> cancel
      end;
    _ -> cancel
  end.


-spec about(Parent :: wxWindow:wxWindow()) -> 'ok'.

about(Parent) ->
  Caption = "About " ++ ?APPNAME,
  Dialog = wxMessageDialog:new(Parent, ?DIALOG_About, [{style, ?wxOK}, {caption, Caption}]),
  wxDialog:showModal(Dialog),
  wxWindow:destroy(Dialog).
