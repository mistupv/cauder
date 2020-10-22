-module(cauder_load).

%% API
-export([file/1]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Loads the given file (module) and stores it into the database.

-spec file(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: module().

file(File) -> store_module(File).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


-spec store_module(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: module().

store_module(File) ->
  {ok, Forms0} = epp:parse_file(File, [], []),
  Forms1 = epp:interpret_file_attribute(Forms0),
  Forms = erl_expand_records:module(Forms1, []),

  [Module] = [M || {attribute, _, module, M} <- Forms],
  Exports = sets:union([sets:from_list(FAs) || {attribute, _, export, FAs} <- Forms]),

  % TODO Check module name matches filename

  put(var_count, 0),
  put(fun_count, 0),
  store_forms(Module, Forms, Exports),
  erase(var_count),
  erase(fun_count),
  erase(current_function),

  {ok, Module}.


-spec store_forms(Module, Forms, Exports) -> ok when
  Module :: module(),
  Forms :: [erl_parse:abstract_form()],
  Exports :: sets:set({atom(), arity()}).

store_forms(Mod, [{function, _, Name, Arity, Cs0} | Fs], Exp) ->
  FA = {Name, Arity},
  put(current_function, FA),
  Cs = cauder_syntax:clauses(Cs0),
  Exported = sets:is_element(FA, Exp),
  ets:insert(?APP_DB, {{Mod, Name, Arity, Exported}, Cs}),
  store_forms(Mod, Fs, Exp);
store_forms(Mod, [_ | Fs], Exp) -> store_forms(Mod, Fs, Exp);
store_forms(_, [], _)           -> ok.
