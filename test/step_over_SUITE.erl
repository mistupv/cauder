-module(step_over_SUITE).

-include("cauder.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([
  test_simple/1,
  test_function/1,
  test_case1/1,
  test_case2/1,
  test_case3/1,
  test_case4/1,
  test_case5/1,
  test_case6/1,
  test_nested_case/1
]).


all() ->
  [
    test_simple,
    test_function,
    test_case1,
    test_case2,
    test_case3,
    test_case4,
    test_case5,
    test_case6,
    test_nested_case
  ].


init_per_testcase(_, Config) ->
  {ok, _} = cauder:start_link(),
  TrapFlag = process_flag(trap_exit, true),
  ok = cauder:subscribe(),
  Dir = ?config(data_dir, Config),
  File = filename:join(Dir, "sample.erl"),

  {ok, undefined} = cauder:load_file(File),
  _ = receive_or_fail({load, File, sample}),

  [{trap_flag, TrapFlag} | Config].

end_per_testcase(_, Config) ->
  cauder:unsubscribe(),
  flush(),
  cauder:stop(),
  process_flag(trap_exit, ?config(trap_flag, Config)).


test_simple(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_simple, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {value, _, 42}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys2,

  % Backward

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_function(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_function, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {local_call, _, 'foo', []}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys2,

  % Backward

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case1(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case1, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {'case', _, _, _}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 7}]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {value, _, 42}}, _]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys3,

  % Backward

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case2(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case2, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {'case', _, _, _}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, good}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 7}]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {value, _, 42}}, _]}}} = Sys3,

  {ok, Sys3} = cauder:step_over(?FWD_SEM, Pid),
  Sys4 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys4,

  % Backward

  {ok, Sys4} = cauder:step_over(?BWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case3(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case3, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {'case', _, _, _}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 42}]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys3,

  % Backward

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case4(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case4, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {'case', _, _, _}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, good}, _]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 42}]}}} = Sys3,

  {ok, Sys3} = cauder:step_over(?FWD_SEM, Pid),
  Sys4 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{op, _, '+', [{var, _, 'A'}, {var, _, 'B'}]}]}}} = Sys4,

  % Backward

  {ok, Sys4} = cauder:step_over(?BWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case5(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case5, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {'value', _, 42}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{'case', _, _, _}]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 49}]}}} = Sys3,

  % Backward

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_case6(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_case6, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'A'}, {value, _, 7}}, _, _]}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{match, _, {var, _, 'B'}, {value, _, 42}}, _]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{'case', _, _, _}]}}} = Sys2,

  {ok, Sys2} = cauder:step_over(?FWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, lucky_answer}, _]}}} = Sys3,

  {ok, Sys3} = cauder:step_over(?FWD_SEM, Pid),
  Sys4 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{value, _, 49}]}}} = Sys4,

  % Backward

  {ok, Sys4} = cauder:step_over(?BWD_SEM, Pid),
  Sys3 = receive_or_fail(step_over),

  {ok, Sys3} = cauder:step_over(?BWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


test_nested_case(_Config) ->
  % Init system

  ok = cauder:init_system(sample, test_nested_case, []),
  #sys{procs = PMap} = receive_or_fail(start),

  [Pid] = maps:keys(PMap),

  {ok, _} = cauder:step(?FWD_SEM, Pid),
  Sys0 = receive_or_fail(step),
  #sys{procs = #{Pid := #proc{exprs = [{'case', _, _, _}], stack = Stk0}}} = Sys0,

  % Forward

  {ok, Sys0} = cauder:step_over(?FWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{'case', _, _, _}], stack = [{'case', _, _} | Stk0]}}} = Sys1,

  {ok, Sys1} = cauder:step_over(?FWD_SEM, Pid),
  Sys2 = receive_or_fail(step_over),
  #sys{procs = #{Pid := #proc{exprs = [{'op', _, '+', [{value, _, 7}, {value, _, 42}]}], stack = [{'case', _, _}, {'case', _, _} | Stk0]}}} = Sys2,

  % Backward

  {ok, Sys2} = cauder:step_over(?BWD_SEM, Pid),
  Sys1 = receive_or_fail(step_over),

  {ok, Sys1} = cauder:step_over(?BWD_SEM, Pid),
  Sys0 = receive_or_fail(step_over),

  ok.


%%%=============================================================================


receive_or_fail(Task) ->
  receive
    {dbg, {finish, Task, _, System}} -> System;
    {dbg, {finish, Reply, _, System}} when is_atom(Task), is_tuple(Reply), element(1, Reply) =:= Task -> System;
    {dbg, {fail, Task, Reason, [Where | _]}} -> error({Reason, Where})
  after 100 ->
    error(timeout)
  end.


flush() ->
  receive
    _ -> flush()
  after 0 ->
    ok
  end.
