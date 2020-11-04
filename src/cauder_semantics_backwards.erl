%%%-----------------------------------------------------------------------------
%%% @doc Backwards (reversible) semantics for Erlang.
%%% @end
%%%-----------------------------------------------------------------------------

-module(cauder_semantics_backwards).

%% API
-export([step/2, options/1]).

-include("cauder.hrl").


%%%=============================================================================
%%% API
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Performs a single backwards step in the process with the given Pid in
%% the given System.

-spec step(System, Pid) -> NewSystem when
  System :: cauder_types:system(),
  Pid :: cauder_types:proc_id(),
  NewSystem :: cauder_types:system().

step(Sys, Pid) ->
  #sys{mail = Ms, logs = Logs, trace = Trace} = Sys,
  {P0, PDict0} = orddict:take(Pid, Sys#sys.procs),
  #proc{pid = Pid, hist = [CurHist | RestHist]} = P0,
  Log =
    case orddict:find(Pid, Logs) of
      {ok, L} -> L;
      error -> []
    end,
  case CurHist of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        mail  = Ms,
        procs = orddict:store(Pid, P, PDict0)
      };
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
      Sys#sys{
        mail  = Ms,
        procs = orddict:store(Pid, P, orddict:erase(Gid, PDict0)),
        logs  = orddict:store(Pid, [{spawn, Gid} | Log], Logs),
        trace = lists:delete(T, Trace)
      };
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
      Sys#sys{
        mail  = OldMsgs,
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, [{send, Uid} | Log], Logs),
        trace = lists:delete(T, Trace)
      };
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
      Sys#sys{
        mail  = [M | Ms],
        procs = orddict:store(Pid, P, PDict0),
        logs  = orddict:store(Pid, [{'receive', Uid} | Log], Logs),
        trace = lists:delete(T, Trace)
      }
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the backwards evaluation options for the given System.

-spec options(System) -> Options when
  System :: cauder_types:system(),
  Options :: [cauder_types:option()].

options(Sys = #sys{procs = PDict0}) ->
  lists:filtermap(
    fun({_, #proc{pid = Pid}}) ->
      {P, PDict1} = orddict:take(Pid, PDict0),
      case process_option(Sys#sys{procs = PDict1}, P) of
        ?NULL_OPT -> false;
        Opt -> {true, Opt}
      end
    end,
    PDict0
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

process_option(#sys{mail = Mail, procs = PDict}, #proc{pid = Pid, hist = Hist}) ->
  Rule =
    case Hist of
      [] -> ?NULL_RULE;
      [CurHist | _] ->
        case CurHist of
          {tau, _Bs, _Es, _Stk} -> ?RULE_SEQ;
          {self, _Bs, _Es, _Stk} -> ?RULE_SELF;
          {spawn, _Bs, _Es, _Stk, SpawnPid} ->
            {SpawnProc, _} = orddict:take(SpawnPid, PDict),
            case SpawnProc#proc.hist of
              [] -> ?RULE_SPAWN;
              _ -> ?NULL_RULE
            end;
          {send, _Bs, _Es, _Stk, #msg{uid = Uid}} ->
            case cauder_utils:find_message(Mail, Uid) of
              false -> ?NULL_RULE;
              {value, _} -> ?RULE_SEND
            end;
          {rec, _Bs, _Es, _Stk, _Msg} -> ?RULE_RECEIVE
        end
    end,
  case Rule of
    ?NULL_RULE -> ?NULL_OPT;
    OtherRule -> #opt{sem = ?MODULE, pid = Pid, rule = OtherRule}
  end.
