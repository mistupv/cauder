-module(cauder_load).

-export([load_module/2]).

-spec load_module(atom(), file:filename()) -> ok.

load_module(Mod, File) -> store_module(Mod, File).


-spec store_module(atom(), file:filename()) -> ok.

store_module(Mod, File) ->
  {ok, Forms0} = epp:parse_file(File, [], []),
  Forms1 = epp:interpret_file_attribute(Forms0),
  Forms = erl_expand_records:module(Forms1, []),

  [Mod] = [M || {attribute, _, module, M} <- Forms],
  Exp = sets:union([sets:from_list(FAs) || {attribute, _, export, FAs} <- Forms]),

  put(var_count, 0),
  put(fun_count, 0),
  store_forms(Mod, Forms, Exp),
  erase(var_count),
  erase(fun_count),
  erase(current_function).


-type abstract_form() :: af_function_decl() | erl_parse:abstract_form().
-type af_function_decl() :: {'function', erl_anno:anno(), atom(), arity(), [erl_parse:abstract_clause(), ...]}.


-spec store_forms(atom(), [abstract_form()], sets:set({atom(), arity()})) -> ok.

store_forms(Mod, [{function, _, Name, Arity, Cs0} | Fs], Exp) ->
  FA = {Name, Arity},
  put(current_function, FA),
  Cs = cauder_syntax:clauses(Cs0),
  Exported = sets:is_element(FA, Exp),
  cauder:ref_add({Mod, Name, Arity, Exported}, Cs),
  store_forms(Mod, Fs, Exp);
store_forms(Mod, [_ | Fs], Exp) -> store_forms(Mod, Fs, Exp);
store_forms(_, [], _)           -> ok.
