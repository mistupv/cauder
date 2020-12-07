%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([can_step/2, can_step_over/2, can_step_multiple/1]).
-export([step/2, step_over/2, options/1]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Checks whether a single backwards step can be performed in the process
%% with the given pid in the given system, or not.

-spec can_step(System, Pid) -> CanStep when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  CanStep :: boolean().

can_step(System, Pid) -> lists:any(fun(Opt) -> Opt#opt.pid =:= Pid end, options(System)).


%%------------------------------------------------------------------------------
%% @doc Checks whether a backwards step over can be performed in the process
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
        % Exit 'if' block
        #proc{hist = [Entry | _], stack = [{'if', _, _} | Stk]} when element(4, Entry) =:= Stk -> true;
        % Exit 'case' block
        #proc{hist = [Entry | _], stack = [{'case', _, _} | Stk]} when element(4, Entry) =:= Stk -> true;
        % Exit 'receive' block
        #proc{hist = [Entry | _], stack = [{'receive', _, _} | Stk]} when element(4, Entry) =:= Stk -> true;
        % Goto previous line
        #proc{hist = [Entry | _], stack = Stk, exprs = Es} when tl(element(3, Entry)) =:= Es, element(4, Entry) =:= Stk -> true;
        % Cannot step
        #proc{} -> false
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Checks whether at least one backwards step can be performed in the given
%% system, or not.

-spec can_step_multiple(System) -> CanStep when
  System :: cauder_types:system(),
  CanStep :: boolean().

can_step_multiple(System) -> options(System) =/= [].


%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> {ok, NewSystem} when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

step(#sys{mail = Ms, logs = LMap, trace = Trace} = Sys, Pid) ->
  {#proc{pid = Pid, hist = [Entry | RestHist]} = P0, PMap} = maps:take(Pid, Sys#sys.procs),

  case Entry of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      {ok, Sys#sys{
        mail  = Ms,
        procs = PMap#{Pid => P}
      }};
    {spawn, Bs, Es, Stk, Gid} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_SPAWN,
        from = Pid,
        to   = Gid
      },
      {ok, Sys#sys{
        mail  = Ms,
        procs = maps:remove(Gid, PMap#{Pid => P}),
        logs  = maps:update_with(Pid, fun(Log) -> [{spawn, Gid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      }};
    {send, Bs, Es, Stk, #msg{dest = Dest, val = Val, uid = Uid}} ->
      {_Msg, OldMsgs} = cauder_utils:take_message(Ms, Uid),
      P = P0#proc{
        hist  = RestHist,
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
      {ok, Sys#sys{
        mail  = OldMsgs,
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{send, Uid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      }};
    {rec, Bs, Es, Stk, M = #msg{dest = Pid, val = Val, uid = Uid}} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Val,
        time = Uid
      },
      {ok, Sys#sys{
        mail  = [M | Ms],
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{'receive', Uid} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      }}
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
        % TODO
        {ok, Sys1} ->
          P0 = maps:get(Pid, Sys0#sys.procs),
          P1 = maps:get(Pid, Sys1#sys.procs),
          case {P, P0, P1} of
            % Bwd Step Over: Standard
            {#proc{stack = Stk, exprs = [_ | Es]},
             #proc{stack = Stk, exprs = [_, _ | Es]},
             #proc{stack = Stk, exprs = [_, _, _ | Es]}} ->
              throw({Sys0, Steps});
            % Bwd Step Over: To first expression in block/function
            {#proc{stack = [Entry | Stk], exprs = [_ | Es]},
             #proc{stack = [Entry | Stk], exprs = [_, _ | Es]},
             #proc{stack = Stk}} ->
              throw({Sys0, Steps});
            % Bwd Step Over: Enter block with one expression
            {#proc{stack = Stk, exprs = Es},
             #proc{stack = [{_, [_ | Es], _} | Stk]},
             #proc{stack = Stk, exprs = [_ | Es]}} ->
              throw({Sys0, Steps});
            % Bwd Step Over: Enter block with more than one expression
            {#proc{stack = Stk, exprs = Es},
             #proc{stack = [{_, [_ | Es], _} | Stk], exprs = BlockEs},
             #proc{stack = [{_, [_ | Es], _} | Stk], exprs = [_ | BlockEs]}} ->
              throw({Sys0, Steps});
            % Bwd Step Over: Exit block
            {#proc{stack = [{_, [_ | Es], _} | Stk]},
             #proc{stack = Stk, exprs = [_ | Es]},
             #proc{stack = Stk, exprs = [_, _ | Es]}} ->
              throw({Sys0, Steps});
            % Bwd Step Over: Exit block to first expression in block/function
            {#proc{stack = [{_, [_ | Es], _}, Entry | Stk]},
             #proc{stack = [Entry | Stk], exprs = [_ | Es]},
             #proc{stack = Stk}} ->
              throw({Sys0, Steps});
            {#proc{},
             #proc{},
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
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

options(#sys{procs = PMap} = Sys) ->
  maps:fold(
    fun
      (Pid, Proc, Opts) ->
        case process_option(Sys#sys{procs = maps:without([Pid], PMap)}, Proc) of
          ?NULL_OPT -> Opts;
          Opt -> [Opt | Opts]
        end
    end,
    [],
    PMap
  ).


%%%=============================================================================
%%% Internal functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Returns the evaluation option for the given Process, in the given
%% System.

-spec process_option(System, Process) -> Option when
  System :: cauder_types:system(),
  Process :: cauder_types:process(),
  Option :: cauder_types:option() | ?NULL_OPT.

process_option(_, #proc{hist = []}) ->
  ?NULL_OPT;
process_option(_, #proc{pid = Pid, hist = [{tau, _Bs, _Es, _Stk} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEQ};
process_option(_, #proc{pid = Pid, hist = [{self, _Bs, _Es, _Stk} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SELF};
process_option(#sys{procs = PMap}, #proc{pid = Pid, hist = [{spawn, _Bs, _Es, _Stk, SpawnPid} | _]}) ->
  #proc{hist = Hist} = maps:get(SpawnPid, PMap),
  case Hist of
    [] -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
    _ -> ?NULL_OPT
  end;
process_option(#sys{mail = Mail}, #proc{pid = Pid, hist = [{send, _Bs, _Es, _Stk, #msg{uid = Uid}} | _]}) ->
  case cauder_utils:find_message(Mail, Uid) of
    {value, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
    false -> ?NULL_OPT
  end;
process_option(_, #proc{pid = Pid, hist = [{rec, _Bs, _Es, _Stk, _Msg} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}.
