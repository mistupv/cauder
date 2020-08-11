-module(cauder_wx_dialog).

-export([edit_binding/2]).


-include("cauder.hrl").
-include("cauder_gui.hrl").
-include_lib("wx/include/wx.hrl").


%% ===== Binding editor ===== %%

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
