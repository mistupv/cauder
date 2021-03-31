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

step(#sys{nodes = Nodes, mail = Ms, logs = LMap, trace = Trace} = Sys, Pid) ->
  {#proc{pid = Pid, hist = [Entry | RestHist]} = P0, PMap} = maps:take(Pid, Sys#sys.procs),
  case Entry of
    {Label, Bs, Es, Stk} when Label =:= tau orelse Label =:= self orelse Label =:= node->
      P = P0#proc{
        hist  = RestHist,
        stack = Stk,
        env   = Bs,
        exprs = Es
      },
      Sys#sys{
        mail  = Ms,
        procs = PMap#{Pid => P}
      };
    {nodes, Bs, Es, Stk, HistNodes} ->
      P = P0#proc{
            hist  = RestHist,
            stack = Stk,
            env   = Bs,
            exprs = Es
           },
      Sys#sys{
        mail  = Ms,
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{nodes, {HistNodes}} | Log] end, [], LMap)
       };
    {spawn, Bs, Es, Stk, Node, Gid} ->
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
      Result = case cauder_utils:process_node(PMap, Gid) of
                 false -> fail;
                 _ -> succ
               end,
      Sys#sys{
        mail  = Ms,
        procs = maps:remove(Gid, PMap#{Pid => P}),
        logs  = maps:update_with(Pid, fun(Log) -> [{spawn, {Node, Result, Gid}} | Log] end, [], LMap),
        trace = lists:delete(T, Trace)
      };
    {start, success, Bs, Es, Stk, Node} ->
      P = P0#proc{
            hist  = RestHist,
            stack = Stk,
            env   = Bs,
            exprs = Es
           },
      T = #trace{
             type = ?RULE_START,
             from = Pid,
             res  = succ,
             node = Node
            },
      Sys#sys{
        nodes = Nodes -- [Node],
        mail  = Ms,
        procs = PMap#{Pid => P},
        trace = lists:delete(T, Trace),
        logs  = maps:update_with(Pid, fun(Log) -> [{start, {succ, Node}} | Log] end, [], LMap)
       };
    {start, fail, Bs, Es, Stk, Node} ->
      P = P0#proc{
            hist  = RestHist,
            stack = Stk,
            env   = Bs,
            exprs = Es
           },
      T = #trace{
             type = ?RULE_START,
             from = Pid,
             res  = fail,
             node = Node
            },
      Sys#sys{
        mail  = Ms,
        procs = PMap#{Pid => P},
        trace = lists:delete(T, Trace),
        logs  = maps:update_with(Pid, fun(Log) -> [{start, {fail, Node}} | Log] end, [], LMap)
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
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{send, Uid} | Log] end, [], LMap),
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
        procs = PMap#{Pid => P},
        logs  = maps:update_with(Pid, fun(Log) -> [{'receive', Uid} | Log] end, [], LMap),
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
process_option(_, #proc{pid = Pid, hist = [{node, _Bs, _Es, _Stk} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODE};
process_option(#sys{ nodes = SysNodes}, #proc{node = Node, pid = Pid, hist = [{nodes, _Bs, _Es, _Stk, Nodes} | _]}) ->
  ProcViewOfNodes = SysNodes -- [Node],
  case ProcViewOfNodes =:= Nodes of
    true -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_NODES};
    false -> ?NULL_OPT
  end;
process_option(#sys{procs = PMap}, #proc{pid = Pid, hist = [{spawn, _Bs, _Es, _Stk, _Node,SpawnPid} | _]}) ->
  try maps:get(SpawnPid, PMap) of
    Proc ->
      #proc{hist = Hist} = Proc,
      case Hist of
        [] -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN};
        _ -> ?NULL_OPT
      end
  catch
    error:{badkey, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SPAWN} % this case covers the scenario of a failed spawn
  end;
process_option(#sys{procs = PMap}, #proc{pid = Pid, hist = [{start, success, _Bs, _Es, _Stk, Node} | _]}) ->
  ProcWithFailedStart = cauder_utils:find_process_with_failed_start(PMap, Node),
  ProcOnNode = cauder_utils:find_process_on_node(PMap, Node),
  ProcWithRead = cauder_utils:find_process_with_read(PMap, Node),
  case {ProcWithFailedStart, ProcOnNode, ProcWithRead} of
    {false, false, false} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
    _ -> ?NULL_OPT
  end;
process_option(_, #proc{pid = Pid, hist = [{start, fail, _Bs, _Es, _Stk, _Node} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_START};
process_option(#sys{mail = Mail}, #proc{pid = Pid, hist = [{send, _Bs, _Es, _Stk, #msg{uid = Uid}} | _]}) ->
  case cauder_utils:find_message(Mail, Uid) of
    {value, _} -> #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_SEND};
    false -> ?NULL_OPT
  end;
process_option(_, #proc{pid = Pid, hist = [{rec, _Bs, _Es, _Stk, _Msg} | _]}) ->
  #opt{sem = ?MODULE, pid = Pid, rule = ?RULE_RECEIVE}.
