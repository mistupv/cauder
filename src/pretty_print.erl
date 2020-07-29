%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for cauder systems
%%% @end
%%%-------------------------------------------------------------------
-module(pretty_print).

-export([system/2, system_trace/1, roll_log/1]).
-export([pid/1, expression/1, to_string/1]).

-include("cauder.hrl").
-include_lib("wx/include/wx.hrl").

-type print_options() :: [{cauder_types:print_option(), boolean()}].

%% =====================================================================
%% @doc Pretty-prints a given System

-spec system(cauder_types:system(), print_options()) -> string().

system(#sys{mail = Msgs, procs = Procs}, Opts) ->
  messages(Msgs, Opts) ++ "\n" ++ processes(Procs, Opts).


-spec messages([cauder_types:message()], print_options()) -> string().

messages(Mail, Opts) ->
  case proplists:get_value(?PRINT_MAIL, Opts) of
    false -> "";
    true -> "GM : " ++ messages_1(Mail) ++ "\n"
  end.


-spec messages_1([cauder_types:message()]) -> string().

messages_1(Msgs) ->
  MsgsList = [message(Msg) || Msg <- Msgs],
  "[" ++ string:join(MsgsList, ",") ++ "]".


-spec message(cauder_types:message()) -> string().

message(#msg{dest = DestPid, val = MsgValue, time = Time}) ->
  "(" ++ to_string(DestPid) ++ ",{" ++ to_string(MsgValue) ++ "," ++ red(to_string(Time)) ++ "})".


-spec processes([cauder_types:process()], print_options()) -> string().

processes(Procs, Opts) ->
  SortProcs = lists:sort(fun(P1, P2) -> P1#proc.pid < P2#proc.pid end, Procs),
  ProcsList = [process(Proc, Opts) || Proc <- SortProcs],
  string:join(ProcsList, "\n").


-spec process(cauder_types:process(), print_options()) -> string().

process(#proc{pid = Pid, log = Log, hist = Hist, stack = Stack, env = Env, exprs = Es0, spf = MFA}, Opts) ->
  Es = cauder_syntax:to_abstract_expr(Es0),

  header(Pid, MFA) ++
    log(Log, Opts) ++
    history(Hist, Opts) ++
    stack(Stack, Opts) ++
    environment(Env, Es, Opts) ++
    expressions(Es, Opts).


%% ==================== Header ==================== %%


-spec header(pos_integer(), undefined | {atom(), atom(), arity()}) -> string().

header(Pid, MFA) -> "=============== " ++ pid(Pid) ++ ": " ++ function(MFA) ++ " ===============\n".


-spec pid(pos_integer()) -> string().

pid(Pid) -> "Proc. " ++ to_string(Pid).


-spec function(undefined | {atom(), arity()}) -> string().

function(undefined) -> "";
function({M, F, A}) -> io_lib:format("~s:~s/~p", [M, F, A]).


%% ==================== Log ==================== %%


-spec log(cauder_types:log(), print_options()) -> string().

log(Log, Opts) ->
  case proplists:get_value(?PRINT_LOG, Opts) of
    false -> "";
    true -> "LOG: " ++ log_1(Log) ++ "\n"
  end.


-spec log_1(cauder_types:log()) -> string().

log_1(Log) ->
  Entries = lists:map(fun log_entry/1, Log),
  "[" ++ lists:flatten(lists:join(",", Entries)) ++ "]".


-spec log_entry(cauder_types:log_entry()) -> string().

log_entry({spawn, SpawnPid})  -> "spawn(" ++ green(to_string(SpawnPid)) ++ ")";
log_entry({send, Stamp})      -> "send(" ++ red(to_string(Stamp)) ++ ")";
log_entry({'receive', Stamp}) -> "rec(" ++ blue(to_string(Stamp)) ++ ")".


%% ==================== History ==================== %%


-spec history(cauder_types:history(), print_options()) -> string().

history(Hist, Opts) ->
  case proplists:get_value(?PRINT_HIST, Opts) of
    false -> "";
    true -> "H  : " ++ history_1(Hist, Opts) ++ "\n"
  end.


-spec history_1(cauder_types:history(), print_options()) -> string().

history_1(Hist, Opts) ->
  FilteredHist =
    case proplists:get_value(?FULL_HIST, Opts) of
      false -> lists:filter(fun is_conc_item/1, Hist);
      true -> Hist
    end,
  Entries = lists:map(fun history_entry/1, FilteredHist),
  "[" ++ lists:flatten(lists:join(",", Entries)) ++ "]".


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


%% ==================== Stack ==================== %%


-spec stack(cauder_types:stack(), print_options()) -> string().

stack(Stack, Opts) ->
  case proplists:get_value(?PRINT_STACK, Opts) of
    false -> "";
    true -> "STK: " ++ stack_1(Stack) ++ "\n"
  end.


-spec stack_1(cauder_types:stack()) -> string().

stack_1(Stack) ->
  Entries = lists:map(fun stack_entry/1, Stack),
  "[" ++ lists:flatten(lists:join(",", Entries)) ++ "]".


-spec stack_entry(cauder_types:stack_entry()) -> string().

stack_entry({{M, F, A}, _Bs, _Es, _Var}) -> io_lib:format("~s:~s/~b", [M, F, A]);
stack_entry({Type, _Es, _Var})           -> atom_to_list(Type).


%% ==================== Environment ==================== %%


-spec environment(cauder_types:environment(), [cauder_types:abstract_expr()], print_options()) -> string().

environment(Env, Exprs, Opts) ->
  case proplists:get_value(?PRINT_ENV, Opts) of
    false -> "";
    true -> "ENV: " ++ environment_1(Env, Exprs, Opts) ++ "\n"
  end.


-spec environment_1(cauder_types:environment(), [cauder_types:abstract_expr()], print_options()) -> string().

environment_1(Env, Exprs, Opts) ->
  Bindings =
    case proplists:get_value(?FULL_ENV, Opts) of
      true -> Env;
      false -> relevant_bindings(Env, Exprs)
    end,
  PairsList = lists:map(fun binding/1, Bindings),
  "{" ++ lists:flatten(lists:join(",", PairsList)) ++ "}".


-spec relevant_bindings(cauder_types:environment(), [cauder_types:abstract_expr()]) -> cauder_types:environment().

relevant_bindings(Bs, Es) ->
  Vs = sets:union(lists:map(fun erl_syntax_lib:variables/1, Es)),
  lists:filter(fun({V, _}) -> sets:is_element(V, Vs) end, Bs).


-spec binding(cauder_types:binding()) -> string().

binding({Var, Val}) -> io_lib:format("~s -> ~p", [Var, Val]).


%% ==================== Expressions ==================== %%


-spec expressions([erl_parse:abstract_expr()], print_options()) -> string().

expressions(Exprs, Opts) ->
  case proplists:get_value(?PRINT_EXP, Opts) of
    false -> "";
    true -> "EXP:\n" ++ expressions_1(Exprs) ++ "\n"
  end.


-spec expressions_1([erl_parse:abstract_expr()]) -> string().

expressions_1(Exprs) -> lists:flatten(lists:join(",\n", lists:map(fun expression/1, Exprs))).


-spec expression(erl_parse:abstract_expr()) -> string().

expression(Expr) -> lists:flatten(erl_prettypr:format(Expr)).


%% =====================================================================
%% @doc Pretty-prints a given system trace

-spec system_trace(cauder_types:system()) -> string().

system_trace(#sys{trace = Trace}) ->
  % Trace is built as a stack (newest item is first)
  % and we must reverse it to print it
  RevTrace = lists:reverse(Trace),
  TraceStr = [trace(Item) || Item <- RevTrace],
  lists:flatten(lists:join("\n", TraceStr)).


-spec trace(cauder_types:trace()) -> [string()].

trace(#trace{type = ?RULE_SEND, from = From, to = To, val = Val, time = Time}) ->
  [pid(From), " sends ", to_string(Val), " to ", pid(To), " (", to_string(Time), ")"];
trace(#trace{type = ?RULE_SPAWN, from = From, to = To}) ->
  [pid(From), " spawns ", pid(To)];
trace(#trace{type = ?RULE_RECEIVE, from = From, val = Val, time = Time}) ->
  [pid(From), " receives ", to_string(Val), " (", to_string(Time), ")"].


%% =====================================================================
%% @doc Prints a given system roll log

-spec roll_log(cauder_types:system()) -> string().

roll_log(#sys{roll = RollLog}) -> lists:flatten(lists:join("\n", RollLog)).


%% ==================== Utils ==================== %%


-spec to_string(term()) -> string().

to_string(Term) -> io_lib:format("~p", [Term]).


red(Text) -> [{?wxRED, Text}].
green(Text) -> [{?CAUDER_GREEN, Text}].
blue(Text) -> [{?wxBLUE, Text}].
