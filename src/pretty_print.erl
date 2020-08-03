%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for cauder systems
%%% @end
%%%-------------------------------------------------------------------
-module(pretty_print).

-export([process/1, log_entry/1, history_entry/1, stack_entry/1, relevant_bindings/2, expression/1, trace_entry/1]).
-export([pid/1, to_string/1, is_conc_item/1]).


-include("cauder.hrl").
-include_lib("wx/include/wx.hrl").


-spec process(cauder_types:process()) -> string().

process(#proc{pid = Pid, spf = {M, F, A}}) -> lists:flatten(io_lib:format("PID: ~p - ~s:~s/~p", [Pid, M, F, A])).


%% ===== Log ===== %%


-spec log_entry(cauder_types:log_entry()) -> string().

log_entry({spawn, SpawnPid})  -> "spawn(" ++ green(to_string(SpawnPid)) ++ ")";
log_entry({send, Stamp})      -> "send(" ++ red(to_string(Stamp)) ++ ")";
log_entry({'receive', Stamp}) -> "rec(" ++ blue(to_string(Stamp)) ++ ")".


%% ===== History ===== %%


-spec is_conc_item(cauder_types:history_entry()) -> boolean().

is_conc_item({spawn, _Bs, _Es, _Stk, _Pid}) -> true;
is_conc_item({send, _Bs, _Es, _Stk, _Msg})  -> true;
is_conc_item({rec, _Bs, _Es, _Stk, _Msg})   -> true;
is_conc_item(_)                             -> false.


-spec history_entry(cauder_types:history_entry()) -> string().

history_entry({tau, _Bs, _Es, _Stk})                                -> "seq";
history_entry({self, _Bs, _Es, _Stk})                               -> "self";
history_entry({spawn, _Bs, _Es, _Stk, Pid})                         -> "spawn(" ++ green(to_string(Pid)) ++ ")";
history_entry({send, _Bs, _Es, _Stk, #msg{val = Val, time = Time}}) -> "send(" ++ to_string(Val) ++ "," ++ red(to_string(Time)) ++ ")";
history_entry({rec, _Bs, _Es, _Stk, #msg{val = Val, time = Time}})  -> "rec(" ++ to_string(Val) ++ "," ++ blue(to_string(Time)) ++ ")".


%% ===== Stack ===== %%


-spec stack_entry(cauder_types:stack_entry()) -> string().

stack_entry({{M, F, A}, _Bs, _Es, _Var}) -> io_lib:format("~s:~s/~b", [M, F, A]);
stack_entry({Type, _Es, _Var})           -> atom_to_list(Type).


%% ===== Environment ===== %%


-spec relevant_bindings(cauder_types:environment(), [cauder_types:abstract_expr()]) -> cauder_types:environment().

relevant_bindings(Bs, Es) ->
  Es1 = cauder_syntax:to_abstract_expr(Es),
  Vs = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es1)),
  lists:filter(fun({V, _}) -> sets:is_element(V, Vs) end, Bs).


%% ===== Expressions ===== %%


-spec expression(cauder_types:abstract_expr()) -> string().

expression(Expr) -> lists:flatten(erl_prettypr:format(cauder_syntax:to_abstract_expr(Expr))).


%% ===== Trace ===== %%


-spec trace_entry(cauder_types:trace()) -> [string()].

trace_entry(#trace{type = ?RULE_SEND, from = From, to = To, val = Val, time = Time}) ->
  [pid(From), " sends ", to_string(Val), " to ", pid(To), " (", to_string(Time), ")"];
trace_entry(#trace{type = ?RULE_SPAWN, from = From, to = To}) ->
  [pid(From), " spawns ", pid(To)];
trace_entry(#trace{type = ?RULE_RECEIVE, from = From, val = Val, time = Time}) ->
  [pid(From), " receives ", to_string(Val), " (", to_string(Time), ")"].


%% ===== Utils ===== %%


-spec pid(pos_integer()) -> string().

pid(Pid) -> "Proc. " ++ to_string(Pid).


-spec to_string(term()) -> string().

to_string(Term) -> io_lib:format("~p", [Term]).


red(Text) -> [{?wxRED, Text}].
green(Text) -> [{?CAUDER_GREEN, Text}].
blue(Text) -> [{?wxBLUE, Text}].
