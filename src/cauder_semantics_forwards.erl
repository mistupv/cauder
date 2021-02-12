%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([step/4, options/2]).

-ifdef(EUNIT).
-export([rdep/2]).
-endif.

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid, MessageScheduler, Mode) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  MessageScheduler :: cauder_types:message_scheduler(),
  Mode :: normal | replay,
  NewSystem :: cauder_types:system().

step(#sys{mail = Mail, logs = LMap0, trace = Trace} = Sys, Pid, Sched, Mode) ->
  {#proc{pid = Pid, hist = Hist, stack = Stk0, env = Bs0, exprs = Es0} = P0, PMap} = maps:take(Pid, Sys#sys.procs),

  #result{env = Bs, exprs = Es, stack = Stk, label = Label} = cauder_eval:seq(Bs0, Es0, Stk0),

  case Label of
    tau ->
      P = P0#proc{
        hist  = [{tau, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        procs = PMap#{Pid => P}
      };
    {self, VarPid} ->
      P = P0#proc{
        hist  = [{self, Bs0, Es0, Stk0} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, Pid)
      },
      Sys#sys{
        procs = PMap#{Pid => P}
      };
    {spawn, VarPid, Msg, F, As} ->
      {SpawnPid, NewLog} =
        case LMap0 of
          #{Pid := [{spawn, LogPid} | RestLog]} ->
            {LogPid, RestLog};
          _ ->
            {cauder_utils:fresh_pid(), []}
        end,

      P1 = P0#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
      },
      P2 = #proc{
        pid   = SpawnPid,
        exprs = [cauder_syntax:remote_call(Msg, F, lists:map(fun cauder_eval:abstract/1, As))],
        spf   = {Msg, F, length(As)}
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = PMap#{Pid => P1, SpawnPid => P2},
        logs  = LMap0#{Pid => NewLog},
        trace = [T | Trace]
      };
    {send, Dest, Value} ->
      {Msg, Log} =
        case LMap0 of
          #{Pid := [{send, Uid} | RestLog]} ->
            {#message{uid = Uid, value = Value, src = Pid, dest = Dest}, RestLog};
          _ ->
            {#message{value = Value, src = Pid, dest = Dest}, []}
        end,

      P = P0#proc{
        hist  = [{send, Bs0, Es0, Stk0, Msg} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = Dest,
        val  = Value,
        time = Msg#message.uid
      },
      Sys#sys{
        mail  = cauder_mailbox:add(Msg, Mail),
        procs = PMap#{Pid => P},
        logs  = LMap0#{Pid => Log},
        trace = [T | Trace]
      };
    {rec, VarBody, Cs} when Es == [VarBody] ->
      {{Bs1, Es1, {Msg, QPos}, Mail1}, LMap1} =
        case Mode of
          normal ->
            {_, _, {#message{uid = Uid}, _}, _} = Match = cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, Sched, Sys),
            LMap =
              case maps:get(Pid, LMap0, []) of
                % If the chosen message is the same specified in the log don't invalidate the log
                [{'receive', Uid} | RestLog] -> maps:put(Pid, RestLog, LMap0);
                _ -> rdep(Pid, LMap0)
              end,
            {Match, LMap};
          replay ->
            #{Pid := [{'receive', LogUid} | RestLog]} = LMap0,
            Match = cauder_eval:match_rec_uid(Cs, Bs, LogUid, Mail),
            LMap = LMap0#{Pid => RestLog},
            {Match, LMap}
        end,

      P = P0#proc{
        hist  = [{rec, Bs0, Es0, Stk0, Msg, QPos} | Hist],
        stack = Stk,
        env   = cauder_utils:merge_bindings(Bs, Bs1),
        exprs = Es1
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Msg#message.value,
        time = Msg#message.uid
      },
      Sys#sys{
        mail  = Mail1,
        procs = PMap#{Pid => P},
        logs  = LMap1,
        trace = [T | Trace]
      }
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System, Mode) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()],
  Mode :: normal | replay.

options(#sys{mail = Mail, procs = PMap, logs = LMap} = Sys, Mode) ->
  lists:foldl(
    fun
      (#proc{pid = Pid, stack = Stk, env = Bs, exprs = Es}, Opts) ->
        Log = maps:get(Pid, LMap, []),
        case expression_option(Pid, Es, Bs, Stk, Log, Mail, Sys, Mode) of
          ?NOT_EXP -> Opts;
          Rule ->
            Opt = #opt{
              sem  = ?MODULE,
              pid  = Pid,
              rule = Rule
            },
            [Opt | Opts]
        end
    end,
    [],
    maps:values(PMap)
  ).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================

rdep(Pid, LMap0) ->
  LMap = remove_dependents_spawn(Pid, LMap0),
  DropEmpty =
    fun
      (_, []) -> false;
      (_, _) -> true
    end,
  maps:filter(DropEmpty, LMap).

remove_dependents_spawn(Pid0, LMap0) when is_map_key(Pid0, LMap0) ->
  lists:foldl(
    fun entry_dependents/2,
    maps:remove(Pid0, LMap0),
    maps:get(Pid0, LMap0));
remove_dependents_spawn(_Pid, LMap) -> LMap.

remove_dependents_receive(Uid0, LMap0) ->
  RemoveAfterReceive =
    fun(Pid0, LMap1) ->
      {Independent, Dependent} = lists:splitwith(fun(Entry) -> Entry =/= {'receive', Uid0} end, maps:get(Pid0, LMap1)),
      case Dependent of
        [] -> LMap1;
        _ ->
          LMap = lists:foldl(
            fun entry_dependents/2,
            maps:put(Pid0, Independent, LMap1),
            Dependent),
          throw(LMap)
      end
    end,
  try
    lists:foldl(
      RemoveAfterReceive,
      LMap0,
      maps:keys(LMap0))
  catch
    throw:LMap -> LMap
  end.

entry_dependents({spawn, Pid}, LMap)      -> remove_dependents_spawn(Pid, LMap);
entry_dependents({send, Uid}, LMap)       -> remove_dependents_receive(Uid, LMap);
entry_dependents({'receive', _Uid}, LMap) -> LMap.


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.
%%
%% @todo Refactor arguments and return value

-spec expression_option(Pid, Expressions, Environment, Stack, Log, Mail, Sys, Mode) -> Rule when
  Pid :: cauder_types:proc_id(),
  Expressions :: cauder_types:abstract_expr() | [cauder_types:abstract_expr()],
  Environment :: cauder_types:environment(),
  Stack :: cauder_types:stack(),
  Log :: cauder_types:log(),
  Mail :: cauder_mailbox:mailbox(),
  Sys :: cauder_types:system(),
  Mode :: normal | replay,
  Rule :: cauder_types:rule() | ?NOT_EXP.

expression_option(Pid, E, Bs, Stk, Log, Mail, Sys, Mode) when not is_list(E) ->
  expression_option(Pid, [E], Bs, Stk, Log, Mail, Sys, Mode);
expression_option(Pid, [E0 | Es0], Bs, Stk, Log, Mail, Sys, Mode) ->
  case is_reducible(E0, Bs) of
    false ->
      case {Es0, Stk} of
        {[], []} -> ?NOT_EXP;
        _ -> ?RULE_SEQ
      end;
    true ->
      case E0 of
        {var, _, _} -> ?RULE_SEQ;
        {cons, _, H, T} ->
          case is_reducible(H, Bs) of
            true -> expression_option(Pid, H, Bs, Stk, Log, Mail, Sys, Mode);
            false -> expression_option(Pid, T, Bs, Stk, Log, Mail, Sys, Mode)
          end;
        {tuple, _, Es} -> expression_option(Pid, Es, Bs, Stk, Log, Mail, Sys, Mode);
        {'if', _, _} -> ?RULE_SEQ;
        {'case', _, E, _} ->
          case is_reducible(E, Bs) of
            true -> expression_option(Pid, E, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SEQ
          end;
        {'receive', _, Cs} ->
          case Mode of
            normal ->
              % TODO Refactor to only check for match, don't returns unnecessary information
              case cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail, ?SCHEDULER_RoundRobin, Sys) of
                nomatch -> ?NOT_EXP;
                _ -> ?RULE_RECEIVE
              end;
            replay ->
              [{'receive', Uid} | _] = Log,
              % TODO Refactor to only check for match, don't returns unnecessary information
              case cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) of
                nomatch -> ?NOT_EXP;
                _ -> ?RULE_RECEIVE
              end
          end;
        {'make_fun', _, _, _} -> ?RULE_SEQ;
        {bif, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SEQ
          end;
        {self, _} -> ?RULE_SELF;
        {spawn, _, F} ->
          case is_reducible(F, Bs) of
            true -> expression_option(Pid, F, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SPAWN
          end;
        {spawn, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_option(Pid, M, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(F, Bs) of
                true -> expression_option(Pid, F, Bs, Stk, Log, Mail, Sys, Mode);
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
                    false -> ?RULE_SPAWN
                  end
              end
          end;
        {Send, _, L, R} when Send =:= 'send' orelse Send =:= 'send_op' ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(R, Bs) of
                true -> expression_option(Pid, R, Bs, Stk, Log, Mail, Sys, Mode);
                false -> ?RULE_SEND
              end
          end;
        {local_call, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SEQ
          end;
        {remote_call, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SEQ
          end;
        {apply, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_option(Pid, M, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(F, Bs) of
                true -> expression_option(Pid, F, Bs, Stk, Log, Mail, Sys, Mode);
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
                    false -> ?RULE_SEQ
                  end
              end
          end;
        {apply_fun, _, Fun, As} ->
          case is_reducible(Fun, Bs) of
            true -> expression_option(Pid, Fun, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(As, Bs) of
                true -> expression_option(Pid, As, Bs, Stk, Log, Mail, Sys, Mode);
                false -> ?RULE_SEQ
              end
          end;
        {match, _, P, E} ->
          case is_reducible(E, Bs) of
            true -> expression_option(Pid, E, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(P, Bs) of
                true -> expression_option(Pid, P, Bs, Stk, Log, Mail, Sys, Mode);
                false -> ?RULE_SEQ
              end
          end;
        {op, _, _, Es} ->
          case is_reducible(Es, Bs) of
            true -> expression_option(Pid, Es, Bs, Stk, Log, Mail, Sys, Mode);
            false -> ?RULE_SEQ
          end;
        {Op, _, L, R} when Op =:= 'andalso'; Op =:= 'orelse' ->
          case is_reducible(L, Bs) of
            true -> expression_option(Pid, L, Bs, Stk, Log, Mail, Sys, Mode);
            false ->
              case is_reducible(R, Bs) of
                true -> expression_option(Pid, R, Bs, Stk, Log, Mail, Sys, Mode);
                false -> ?RULE_SEQ
              end
          end
      end
  end.
