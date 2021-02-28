%%%-------------------------------------------------------------------
%%% @doc Pretty printing utility functions for CauDEr systems.
%%% @end
%%%-------------------------------------------------------------------

-module(cauder_pp).

-export([process/1, log_entry/1, history_entry/1, stack_entry/1, expression/1, trace_entry/1]).
-export([pid/1, to_string/1]).

-include("cauder.hrl").
-include("cauder_wx.hrl").
-include_lib("wx/include/wx.hrl").


-spec process(Process) -> String when
  Process :: cauder_types:process(),
  String :: string().

process(#proc{pid = Pid, spf = {M, F, A}} = Proc) ->
  Icon =
    case cauder_utils:is_dead(Proc) of
      true -> ?ICON_DEAD;
      false -> ?ICON_ALIVE
    end,
  lists:flatten(Icon ++ io_lib:format(" PID: ~p - ~s:~s/~p", [Pid, M, F, A])).


%%%=============================================================================


-spec log_entry(LogEntry) -> String when
  LogEntry :: cauder_types:log_entry(),
  String :: string().

log_entry({spawn, Pid})     -> "spawn(" ++ green(to_string(Pid)) ++ ")";
log_entry({send, Uid})      -> "send(" ++ red(to_string(Uid)) ++ ")";
log_entry({'receive', Uid}) -> "rec(" ++ blue(to_string(Uid)) ++ ")".


%%%=============================================================================


-spec history_entry(HistoryEntry) -> String when
  HistoryEntry :: cauder_types:history_entry(),
  String :: string().

history_entry({tau, _Bs, _Es, _Stk})                                -> "seq";
history_entry({self, _Bs, _Es, _Stk})                               -> "self";
history_entry({spawn, _Bs, _Es, _Stk, Pid})                         -> "spawn(" ++ green(to_string(Pid)) ++ ")";
history_entry({send, _Bs, _Es, _Stk, #message{value = Val, uid = Uid}}) -> "send(" ++ to_string(Val) ++ "," ++ red(to_string(Uid)) ++ ")";
history_entry({rec, _Bs, _Es, _Stk, #message{value = Val, uid = Uid}, _QPos})  -> "rec(" ++ to_string(Val) ++ "," ++ blue(to_string(Uid)) ++ ")".


%%%=============================================================================


-spec stack_entry(StackEntry) -> String when
  StackEntry :: cauder_types:stack_entry(),
  String :: string().

stack_entry({{M, F, A}, _Bs, _Es, _Var}) -> io_lib:format("~s:~s/~b", [M, F, A]);
stack_entry({Type, _Es, _Var})           -> atom_to_list(Type).


%%%=============================================================================


-spec expression(Expression) -> String when
  Expression :: cauder_types:abstract_expr(),
  String :: string().

expression(Expr) -> lists:flatten(erl_prettypr:format(cauder_syntax:to_abstract_expr(Expr), [{paper, 120}, {ribbon, 120}])).


%%%=============================================================================


-spec trace_entry(Trace) -> String when
  Trace :: cauder_types:trace(),
  String :: string().

trace_entry(#trace{type = ?RULE_SEND, from = From, to = To, val = Val, time = Uid}) ->
  io_lib:format("~s send ~p to ~s (~p)", [pid(From), Val, pid(To), Uid]);
trace_entry(#trace{type = ?RULE_SPAWN, from = From, to = To}) ->
  io_lib:format("~s spawns ~s", [pid(From), pid(To)]);
trace_entry(#trace{type = ?RULE_RECEIVE, from = From, val = Val, time = Uid}) ->
  io_lib:format("~s receives ~p (~p)", [pid(From), Val, Uid]).


%%%=============================================================================


-spec pid(Pid) -> String when
  Pid :: cauder_types:proc_id(),
  String :: string().

pid(Pid) -> "Proc. " ++ to_string(Pid).


-spec to_string(Term) -> String when
  Term :: term(),
  String :: string().

to_string(Term) -> io_lib:format("~p", [Term]).


red(Text) -> [{?wxRED, Text}].
green(Text) -> [{?CAUDER_GREEN, Text}].
blue(Text) -> [{?wxBLUE, Text}].
