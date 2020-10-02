-module(cauder_load).

-export([file/1]).

-spec file(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: atom().

file(File) -> store_module(File).


-spec store_module(File) -> {ok, Module} when
  File :: file:filename(),
  Module :: atom().

store_module(File) ->
  {ok, Forms0} = epp:parse_file(File, [], []),
  Forms1 = epp:interpret_file_attribute(Forms0),
  Forms = erl_expand_records:module(Forms1, []),

  [Module] = [M || {attribute, _, module, M} <- Forms],
  Exp = sets:union([sets:from_list(FAs) || {attribute, _, export, FAs} <- Forms]),

  % TODO Check module name matches filename

  put(var_count, 0),
  put(fun_count, 0),
  store_forms(Module, Forms, Exp),
  erase(var_count),
  erase(fun_count),
  erase(current_function),

  {ok, Module}.


-type abstract_form() :: af_function_decl() | erl_parse:abstract_form().
-type af_function_decl() :: {'function', erl_anno:anno(), atom(), arity(), [erl_parse:abstract_clause(), ...]}.


-spec store_forms(atom(), [abstract_form()], sets:set({atom(), arity()})) -> ok.

store_forms(Mod, [{function, _, Name, Arity, Cs0} | Fs], Exp) ->
  FA = {Name, Arity},
  put(current_function, FA),
  Cs = cauder_syntax:clauses(Cs0),
  Exported = sets:is_element(FA, Exp),
  ets:insert(get(db), {{Mod, Name, Arity, Exported}, Cs}),
  store_forms(Mod, Fs, Exp);
store_forms(Mod, [_ | Fs], Exp) -> store_forms(Mod, Fs, Exp);
store_forms(_, [], _)           -> ok.
