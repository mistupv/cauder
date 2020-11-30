%%%-----------------------------------------------------------------------------
%%% @doc Forwards (reversible) semantics for Erlang.
%%% This module includes two functions, one to get the evaluation options for a
%%% given system and one to perform an evaluation step in a process of a given
%%% system.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_forwards).

%% API
-export([can_step/2, can_step_over/2, can_step_multiple/1]).
-export([step/2, step_over/2, options/1]).

-import(cauder_eval, [is_reducible/2]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Checks whether a single forwards step can be performed in the process
%% with the given pid in the given system, or not.

-spec can_step(System, Pid) -> CanStep when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanStep :: boolean().

can_step(System, Pid) -> lists:any(fun(Opt) -> Opt#opt.pid =:= Pid end, options(System)).


%%------------------------------------------------------------------------------
%% @doc Checks whether a forwards step over can be performed in the process
%% with the given pid in the given system, or not.

-spec can_step_over(System, Pid) -> CanStep when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanStep :: boolean().

can_step_over(#sys{procs = PMap} = System, Pid) ->
  case can_step(System, Pid) of
    false -> false;
    true ->
      case maps:get(Pid, PMap) of
        % Enter 'if' block
        #proc{exprs = [{'if', _, _} | _]} -> true;
        % Enter 'case' block
        #proc{exprs = [{'case', _, _, _} | _]} -> true;
        % Enter 'receive' block
        #proc{exprs = [{'receive', _, _} | _]} -> true;
        % Exit block
        #proc{stack = [{_, [_ | _], _} | _], exprs = [_]} -> true;
        % Goto next line
        #proc{exprs = [_, _ | _]} -> true;
        % Cannot step over
        #proc{} -> false
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Checks whether at least one forwards step can be performed in the given
%% system, or not.

-spec can_step_multiple(System) -> CanStep when
  System :: cauder_types:system(),
  CanStep :: boolean().

can_step_multiple(System) -> options(System) =/= [].


%%------------------------------------------------------------------------------
%% @doc Performs a single forwards step in the process with the given Pid in the
%% given System.

-spec step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

step(#sys{mail = Ms, logs = LMap, trace = Trace} = Sys, Pid) ->
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
    {spawn, VarPid, {value, Line, Fun} = FunLiteral} ->
      {SpawnPid, NewLog} =
        case LMap of
          #{Pid := [{spawn, LogPid} | RestLog]} ->
            {LogPid, RestLog};
          _ ->
            {cauder_utils:fresh_pid(), []}
        end,

      {env, [{{M, F}, _, _}]} = erlang:fun_info(Fun, env),
      {arity, A} = erlang:fun_info(Fun, arity),

      P1 = P0#proc{
        hist  = [{spawn, Bs0, Es0, Stk0, SpawnPid} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = cauder_syntax:replace_variable(Es, VarPid, SpawnPid)
      },
      P2 = #proc{
        pid   = SpawnPid,
        exprs = [{apply_fun, Line, FunLiteral, []}],
        spf   = {M, F, A}
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = PMap#{Pid => P1, SpawnPid => P2},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      };
    {spawn, VarPid, M, F, As} ->
      {SpawnPid, NewLog} =
        case LMap of
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
        exprs = [cauder_syntax:remote_call(M, F, lists:map(fun cauder_eval:abstract/1, As))],
        spf   = {M, F, length(As)}
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = SpawnPid
      },
      Sys#sys{
        procs = PMap#{Pid => P1, SpawnPid => P2},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      };
    {send, Dest, Val} ->
      {Uid, NewLog} =
        case LMap of
          #{Pid := [{send, LogUid} | RestLog]} ->
            {LogUid, RestLog};
          _ ->
            {cauder_utils:fresh_uid(), []}
        end,

      M = #msg{
        dest = Dest,
        val  = Val,
        uid  = Uid
      },
      P = P0#proc{
        hist  = [{send, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SEND,
        from = Pid,
        to   = Dest,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = [M | Ms],
        procs = PMap#{Pid => P},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      };
    {rec, VarBody, Cs} when Es == [VarBody] ->
      {{Bs1, Es1, M = #msg{dest = Pid, val = Val, uid = Uid}, Ms1}, NewLog} =
        case LMap of
          #{Pid := [{'receive', LogUid} | RestLog]} ->
            {cauder_eval:match_rec_uid(Cs, Bs, LogUid, Ms), RestLog};
          _ ->
            {cauder_eval:match_rec_pid(Cs, Bs, Pid, Ms), []}
        end,

      P = P0#proc{
        hist  = [{rec, Bs0, Es0, Stk0, M} | Hist],
        stack = Stk,
        env   = cauder_utils:merge_bindings(Bs, Bs1),
        exprs = Es1
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      Sys#sys{
        mail  = Ms1,
        procs = PMap#{Pid => P},
        logs  = LMap#{Pid => NewLog},
        trace = [T | Trace]
      }
  end.


%%------------------------------------------------------------------------------
%% @doc Performs a forwards step over in the process with the given Pid in the
%% given System.

-spec step_over(System, Pid) -> {NewSystem, StepsDone} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system(),
  StepsDone :: non_neg_integer().

step_over(Sys, Pid) ->
  P = maps:get(Pid, Sys#sys.procs),
  Step =
    fun Step(Sys0, Steps) ->
      case step(Sys0, Pid) of
%%        suspend ->
%%          #sys{procs = #{Pid := P} = Ps} = Sys0,
%%          Sys1 =
%%            case Sem of
%%              ?FWD_SEM -> Sys0#sys{procs = Ps#{Pid => P#proc{suspend_info = {step_over, Sem, tl(Es)}}}};
%%              ?BWD_SEM -> erlang:error(not_implemented)
%%            end,
%%          throw({Sys1, Steps});
        Sys1 ->
          P1 = maps:get(Pid, Sys1#sys.procs),
          case {P, P1} of
            % Fwd Step Over: Standard
            {#proc{stack = Stk, exprs = [_ | Es]},
             #proc{stack = Stk, exprs = Es}} ->
              throw({Sys1, Steps + 1});
            % Fwd Step Over: Enter block
            {#proc{stack = Stk, exprs = [_ | Es]},
             #proc{stack = [{_, [_ | Es], _} | Stk]}} ->
              throw({Sys1, Steps + 1});
            % Fwd Step Over: Exit block
            {#proc{stack = [{_, [_ | Es], _} | Stk]},
             #proc{stack = Stk, exprs = Es}} ->
              throw({Sys1, Steps + 1});
            {#proc{},
             #proc{}} ->
              continue
          end,
          Step(Sys1, Steps + 1)
      end
    end,
  try
    Step(Sys, 0)
  catch
    throw:{#sys{}, N} = Ret when is_integer(N) -> Ret
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the forwards evaluation options for the given System.

-spec options(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys) ->
  maps:fold(
    fun
      (Pid, Proc, Opts) ->
        case expression_rule(Sys, Proc) of
          ?NOT_EXP -> Opts;
          Rule -> [#opt{sem = ?MODULE, pid = Pid, rule = Rule} | Opts]
        end
    end,
    [],
    PMap).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Expression, in the given
%% System.

-spec expression_rule(System, Process) -> Rule when
  System :: cauder_types:system(),
  Process :: cauder_types:process(),
  Rule :: cauder_types:rule() | ?NOT_EXP.

expression_rule(#sys{mail = Mail, logs = LMap} = Sys, #proc{pid = Pid, stack = Stk, env = Bs, exprs = [E0 | Es0]} = Proc) ->
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
            true -> expression_rule(Sys, Proc#proc{exprs = [H]});
            false -> expression_rule(Sys, Proc#proc{exprs = [T]})
          end;
        {tuple, _, Es} -> expression_rule(Sys, Proc#proc{exprs = Es});
        {'if', _, _} -> ?RULE_SEQ;
        {'case', _, E, _} ->
          case is_reducible(E, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [E]});
            false -> ?RULE_SEQ
          end;
        {'receive', _, Cs} ->
          IsMatch =
            case maps:get(Pid, LMap, []) of
              [] -> cauder_eval:match_rec_pid(Cs, Bs, Pid, Mail) =/= nomatch;
              [{'receive', Uid} | _] -> cauder_eval:match_rec_uid(Cs, Bs, Uid, Mail) =/= nomatch
            end,
          case IsMatch of
            true -> ?RULE_RECEIVE;
            false -> ?NOT_EXP
          end;
        {'make_fun', _, _, _} -> ?RULE_SEQ;
        {bif, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = As});
            false -> ?RULE_SEQ
          end;
        {self, _} -> ?RULE_SELF;
        {spawn, _, F} ->
          case is_reducible(F, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [F]});
            false -> ?RULE_SPAWN
          end;
        {spawn, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [M]});
            false ->
              case is_reducible(F, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = [F]});
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_rule(Sys, Proc#proc{exprs = [As]}); % This list of arguments is represented as an abstract list
                    false -> ?RULE_SPAWN
                  end
              end
          end;
        {Send, _, L, R} when Send =:= 'send' orelse Send =:= 'send_op' ->
          case is_reducible(L, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [L]});
            false ->
              case is_reducible(R, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = [R]});
                false -> ?RULE_SEND
              end
          end;
        {local_call, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = As});
            false -> ?RULE_SEQ
          end;
        {remote_call, _, _, _, As} ->
          case is_reducible(As, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = As});
            false -> ?RULE_SEQ
          end;
        {apply, _, M, F, As} ->
          case is_reducible(M, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [M]});
            false ->
              case is_reducible(F, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = [F]});
                false ->
                  case is_reducible(As, Bs) of
                    true -> expression_rule(Sys, Proc#proc{exprs = As});
                    false -> ?RULE_SEQ
                  end
              end
          end;
        {apply_fun, _, Fun, As} ->
          case is_reducible(Fun, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [Fun]});
            false ->
              case is_reducible(As, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = As});
                false -> ?RULE_SEQ
              end
          end;
        {match, _, P, E} ->
          case is_reducible(E, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [E]});
            false ->
              case is_reducible(P, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = [P]});
                false -> ?RULE_SEQ
              end
          end;
        {op, _, _, Es} ->
          case is_reducible(Es, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = Es});
            false -> ?RULE_SEQ
          end;
        {Op, _, L, R} when Op =:= 'andalso'; Op =:= 'orelse' ->
          case is_reducible(L, Bs) of
            true -> expression_rule(Sys, Proc#proc{exprs = [L]});
            false ->
              case is_reducible(R, Bs) of
                true -> expression_rule(Sys, Proc#proc{exprs = [R]});
                false -> ?RULE_SEQ
              end
          end
      end
  end.
