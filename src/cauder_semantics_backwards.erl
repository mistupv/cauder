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

step(#sys{mail = Mail, logs = LMap, trace = Trace} = Sys, Pid) ->
  {#proc{pid = Pid, hist = [Entry0 | RestHist]} = P0, PMap} = maps:take(Pid, Sys#sys.procs),

  case Entry0 of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        mail  = Mail,
        procs = PMap#{Pid => P}
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
      Entry1 = {spawn, Gid},
      Sys#sys{
        mail  = Mail,
        procs = maps:remove(Gid, PMap#{Pid => P}),
        logs  = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
        trace = lists:delete(T, Trace)
      };
    {send, Bs, Es, Stk, #message{uid = Uid, value = Value, dest = Dest} = Msg} ->
      {_, OldMail} = cauder_mailbox:delete(Msg, Mail),
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
        val  = Value,
        time = Uid
      },
      Entry1 = {send, Uid},
      Sys#sys{
        mail  = OldMail,
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
        trace = lists:delete(T, Trace)
      };
    {rec, Bs, Es, Stk, M = #message{uid = Uid, value = Value, dest = Pid}, QPos} ->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      T = #trace{
        type = ?RULE_RECEIVE,
        from = Pid,
        val  = Value,
        time = Uid
      },
      Entry1 = {'receive', Uid},
      Sys#sys{
        mail  = cauder_mailbox:insert(M, QPos, Mail),
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [Entry1 | Log] end, [Entry1], LMap),
        trace = lists:delete(T, Trace)
      }
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
process_option(#sys{mail = Mail}, #proc{pid = Pid, hist = [{send, _Bs, _Es, _Stk, #message{uid = Uid}} | _]}) ->
  case cauder_mailbox:uid_member(Uid, Mail) of
    true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
    false -> ?NULL_OPT
  end;
process_option(_, #proc{pid = Pid, hist = [{rec, _Bs, _Es, _Stk, _Msg, _QPos} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}.
