-module(cauder_wx_dialog).

%% API
-export([start_session/2, stop_session/1]).
-export([edit_binding/2]).
-export([choose_message/2]).
-export([drop_files/2]).
-export([about/1]).

-include_lib("wx/include/wx.hrl").
-include("cauder.hrl").
-include("cauder_wx.hrl").


%%------------------------------------------------------------------------------
%% @doc Shows a dialog where the use can choose the execution mode, with the
%% required information in each case.

-spec start_session(Parent, EntryPoints) -> {manual, {Module, Function, NodeName, Args}} | {replay, TracePath} | false when
  Parent :: wxWindow:wxWindow(),
  EntryPoints :: [mfa()],
  Module :: module(),
  Function :: atom(),
  NodeName :: cauder_types:net_node(),
  Args :: cauder_types:af_args(),
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

  wxRadioButton:setValue(ManualRadio, true),

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

  ArgsLabel = wxStaticText:new(ManualPanel, ?wxID_ANY, "Arguments: "),
  wxBoxSizer:add(ManualSizer, ArgsLabel),

  ArgsCtrl = wxTextCtrl:new(ManualPanel, ?wxID_ANY, [{size, {250, -1}}]),
  wxBoxSizer:add(ManualSizer, ArgsCtrl),

  wxBoxSizer:addSpacer(ManualSizer, ?SPACER_SMALL),

  NodeLabel = wxStaticText:new(ManualPanel, ?wxID_ANY, "Node name: "),
  wxBoxSizer:add(ManualSizer, NodeLabel),

  NodeName = wxTextCtrl:new(ManualPanel, ?wxID_ANY, [{size, {250, -1}}]),
  wxBoxSizer:add(ManualSizer, NodeName),

  
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
  TracePath = filename:join(BasePath, "results"),
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
            case cauder_utils:stringToExpressions(wxTextCtrl:getValue(ArgsCtrl)) of
              error ->
                Message = ?DIALOG_BadArgs_Message,
                Options = [{caption, ?DIALOG_BadArgs_Title}, {style, ?wxICON_ERROR}],
                wxMessageDialog:showModal(wxMessageDialog:new(Dialog, Message, Options));
              Args ->
                case length(Args) of
                  A1 ->
                    N1 = wxTextCtrl:getValue(NodeName),
                    case cauder_utils:checkNodeName(N1) of
                      ok -> ReturnPid ! {manual, {M1, F1, N1, Args}};
                      not_provided -> ReturnPid ! {manual, {M1, F1, "nonode@nohost", Args}};
                      error ->
                        Message = io_lib:format(?DIALOG_StartSession_NodeName_Message, []),
                        Options = [{caption, ?DIALOG_StartSession_NodeName_Title}, {style, ?wxICON_WARNING}],
                        wxMessageDialog:showModal(wxMessageDialog:new(Dialog, Message, Options))
                    end;
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

  event_handler_entry_point(FunChoice, ArgsCtrl),
  event_handler_start_mode(#{ManualRadio => ManualPanel, ReplayRadio => ReplayPanel}),

  wxWindow:enable(ReplayPanel, [{enable, false}]),

  %% -----

  wxDialog:fit(Dialog),
  wxDialog:show(Dialog),
  receive
    Return ->
      wxDialog:destroy(Dialog),
      Return
  end.


event_handler_entry_point(Choice, TextCtrl) ->
  Callback =
    fun(_, _) ->
      Index = wxChoice:getSelection(Choice),
      {_, _, Arity} = wxChoice:getClientData(Choice, Index),
      wxTextCtrl:enable(TextCtrl, [{enable, Arity > 0}])
    end,
  wxChoice:connect(Choice, command_choice_selected, [{callback, Callback}]).


event_handler_start_mode(RadioPanels) ->
  lists:foreach(
    fun(Radio) ->
      {ThisPanel, OtherPanels} = maps:take(Radio, RadioPanels),
      Callback =
        fun(_, _) ->
          wxWindow:enable(ThisPanel, [{enable, true}]),
          lists:foreach(fun(Panel) -> wxWindow:enable(Panel, [{enable, false}]) end, maps:values(OtherPanels))
        end,
      wxRadioButton:connect(Radio, command_radiobutton_selected, [{callback, Callback}])
    end,
    maps:keys(RadioPanels)
  ).


%%------------------------------------------------------------------------------
%% @doc Shows a dialog warning the user that s/he is about to stop the session,
%% loosing any unsaved data.

-spec stop_session(Parent) -> Result when
  Parent :: wxWindow:wxWindow(),
  Result :: boolean().

stop_session(Parent) ->
  Options = [{style, ?wxICON_EXCLAMATION bor ?wxYES_NO bor ?wxNO_DEFAULT}, {caption, ?DIALOG_StopSession_Title}],
  Dialog = wxMessageDialog:new(Parent, ?DIALOG_StopSession_Message, Options),

  case wxDialog:showModal(Dialog) of
    ?wxID_YES -> true;
    _ -> false
  end.


%%------------------------------------------------------------------------------
%% @doc Shows a dialog that allows the user to change the value of the given
%% binding.

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
      case cauder_utils:stringToExpressions(Str) of
        [{value, _, NewValue}] -> {Key, NewValue};
        _ -> cancel
      end;
    _ -> cancel
  end.


%%------------------------------------------------------------------------------
%% @doc Shows a dialog that allows the user to choose a messages from a lists of
%% messages.

-spec choose_message(Parent, {Receiver, Messages}) -> {ok, MessageId} | cancel when
  Parent :: wxWindow:wxWindow(),
  Receiver :: cauder_types:proc_id(),
  Messages :: [cauder_mailbox:message()],
  MessageId :: cauder_mailbox:uid().

choose_message(Parent, {Receiver, Messages}) ->
  Dialog = wxDialog:new(Parent, ?wxID_ANY, "Choose a message"),

  Sizer = wxBoxSizer:new(?wxVERTICAL),
  wxDialog:setSizer(Dialog, Sizer),

  %% ----- Content ----- %%

  Content = wxBoxSizer:new(?wxVERTICAL),
  wxBoxSizer:add(Sizer, Content, [{flag, ?wxEXPAND bor ?wxALL}, {border, ?SPACER_LARGE}]),

  HeaderText = wxStaticText:new(Dialog, ?wxID_ANY, io_lib:format("Choose the message to be received by process ~p.", [Receiver])),
  wxBoxSizer:add(Content, HeaderText, [{proportion, 0}]),

  %% -----

  wxBoxSizer:addSpacer(Content, ?SPACER_LARGE),

  %% -----

  MessageList = wxListCtrl:new(Dialog, [{style, ?wxLC_REPORT bor ?wxLC_SINGLE_SEL}]),
  wxBoxSizer:add(Content, MessageList, [{proportion, 1}, {flag, ?wxEXPAND}]),

  Item = wxListItem:new(),
  Font = wxFont:new(9, ?wxTELETYPE, ?wxNORMAL, ?wxNORMAL),

  wxListItem:setText(Item, "UID"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MessageList, 0, Item),

  wxListItem:setText(Item, "Value"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MessageList, 1, Item),

  wxListItem:setText(Item, "Source"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MessageList, 2, Item),

  wxListItem:setText(Item, "Destination"),
  wxListItem:setFont(Item, Font),
  wxListCtrl:insertColumn(MessageList, 3, Item),

  wxListItem:destroy(Item),

  wxListCtrl:setColumnWidth(MessageList, 0, 50),
  wxListCtrl:setColumnWidth(MessageList, 1, 200),
  wxListCtrl:setColumnWidth(MessageList, 2, 75),
  wxListCtrl:setColumnWidth(MessageList, 3, 75),

  lists:foldl(
    fun(#message{uid = Uid, value = Value, src = Src, dest = Dest}, Row) ->
      wxListCtrl:insertItem(MessageList, Row, ""),
      wxListCtrl:setItemFont(MessageList, Row, Font),
      wxListCtrl:setItem(MessageList, Row, 0, cauder_pp:to_string(Uid)),
      wxListCtrl:setItem(MessageList, Row, 1, cauder_pp:to_string(Value)),
      wxListCtrl:setItem(MessageList, Row, 2, cauder_pp:to_string(Src)),
      wxListCtrl:setItem(MessageList, Row, 3, cauder_pp:to_string(Dest)),
      Row + 1
    end, 0, Messages),

  %% -----

  Separator = wxStaticLine:new(Dialog, [{style, ?wxLI_HORIZONTAL}]),
  wxBoxSizer:add(Sizer, Separator, [{flag, ?wxEXPAND}]),

  %% ----- Buttons ----- %%

  Buttons = wxBoxSizer:new(?wxHORIZONTAL),
  wxBoxSizer:add(Sizer, Buttons, [{flag, ?wxALIGN_RIGHT bor ?wxALL}, {border, ?SPACER_LARGE}]),

  ReceiveButton = wxButton:new(Dialog, ?wxID_OK, [{label, "Receive"}]),
  wxButton:disable(ReceiveButton),
  wxBoxSizer:add(Buttons, ReceiveButton),

  wxBoxSizer:addSpacer(Buttons, ?SPACER_MEDIUM),

  CancelButton = wxButton:new(Dialog, ?wxID_CANCEL, [{label, "Cancel"}]),
  wxBoxSizer:add(Buttons, CancelButton),

  %% -----

  ItemFocusedCallback =
    fun
      (#wx{event = #wxList{itemIndex = Idx}}, _) ->
        wxButton:enable(ReceiveButton, [{enable, Idx =/= ?wxNOT_FOUND}])
    end,

  ItemActivatedCallback =
    fun
      (#wx{event = #wxList{itemIndex = Idx}}, _) when Idx =/= ?wxNOT_FOUND ->
        wxDialog:endModal(Dialog, ?wxID_OK)
    end,

  wxListCtrl:connect(MessageList, command_list_item_focused, [{callback, ItemFocusedCallback}]),
  wxListCtrl:connect(MessageList, command_list_item_activated, [{callback, ItemActivatedCallback}]),

  wxDialog:fit(Dialog),
  case wxDialog:showModal(Dialog) of
    ?wxID_OK ->
      Idx = wxListCtrl:getNextItem(MessageList, -1, [{state, ?wxLIST_STATE_SELECTED}]),
      Message = lists:nth(Idx + 1, Messages),
      {ok, Message#message.uid};
    _ -> cancel
  end.


%%------------------------------------------------------------------------------
%% @doc If the given list of files has more than one file, or any unsupported
%% file, a dialog will be shown to inform the user.

-spec drop_files(Parent, Files) -> {ok, File} | false when
  Parent :: wxWindow:wxWindow(),
  Files :: [unicode:chardata()],
  File :: unicode:chardata().

drop_files(Parent, Files) ->
  {ErlFiles, NonErlFiles} = lists:partition(fun(File) -> filename:extension(File) =:= ".erl" end, Files),

  case NonErlFiles =/= [] of
    true ->
      Options = [{style, ?wxICON_ERROR bor ?wxOK}, {caption, ?DIALOG_DropFiles_Unsupported_Title}],
      Dialog = wxMessageDialog:new(Parent, ?DIALOG_DropFiles_Unsupported_Message, Options),
      wxMessageDialog:showModal(Dialog),
      false;
    false ->
      case ErlFiles of
        [File] -> {ok, File};
        _ ->
          Dialog = wxSingleChoiceDialog:new(Parent, ?DIALOG_DropFiles_Multiple_Message, ?DIALOG_DropFiles_Multiple_Title, ErlFiles),
          case wxSingleChoiceDialog:showModal(Dialog) of
            ?wxID_OK -> {ok, wxSingleChoiceDialog:getStringSelection(Dialog)};
            _ -> false
          end
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Shows a dialog with some basic information about CauDEr.

-spec about(Parent :: wxWindow:wxWindow()) -> ok.

about(Parent) ->
  Caption = "About " ++ ?APP_NAME,
  Dialog = wxMessageDialog:new(Parent, ?DIALOG_About, [{style, ?wxOK}, {caption, Caption}]),
  wxDialog:showModal(Dialog),
  wxWindow:destroy(Dialog).
