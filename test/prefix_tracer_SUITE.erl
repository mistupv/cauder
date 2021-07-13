-module(prefix_tracer_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([purchase/1]).

-include("cauder.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [purchase].

init_per_testcase(Module, Config) ->
    DataDir = ?config(data_dir, Config),
    RootDir = filename:join(lists:takewhile(fun(Dir) -> Dir =/= "_build" end, filename:split(DataDir))),
    SrcDir = filename:join([RootDir, "case-studies", Module]),
    [{src_dir, SrcDir} | Config].

end_per_testcase(_, _Config) ->
    ok.

%%%=============================================================================

purchase(Config) ->
    SrcDir = ?config(src_dir, Config),

    #trace_info{
        node = 'nonode@nohost',
        pid = 0,
        call = {?FUNCTION_NAME, main, []},
        tracing = success,
        return = {value, false},
        comp = _,
        exec = _,
        trace = #{
            0 := Actions0,
            1 := Actions1,
            2 := Actions2,
            3 := Actions3,
            4 := Actions4
        }
    } = prefix_tracer:trace(?FUNCTION_NAME, main, [], #{dir => SrcDir}),

    {
        [
            {spawn, {'nonode@nohost', 1}, success},
            {spawn, {'nonode@nohost', 2}, success},
            {spawn, {'nonode@nohost', 3}, success},
            {spawn, {'nonode@nohost', 4}, success}
        ],
        [
            {deliver, Pid2},
            {'receive', Pid2}
        ]
    } = lists:partition(
        fun
            ({spawn, _, _}) -> true;
            (_) -> false
        end,
        Actions0
    ),

    [{deliver, Pid0}, {deliver, Pid1}, {deliver, Pid3}] = lists:filter(
        fun
            ({deliver, _}) -> true;
            (_) -> false
        end,
        Actions1
    ),
    % Receive comes after deliver
    true = lists:member({'receive', Pid0}, lists:dropwhile(fun(Action) -> Action =/= {deliver, Pid0} end, Actions1)),
    true = lists:member({send, Pid2}, Actions1),

    [{send, Pid0}] = Actions2,
    [{send, Pid1}] = Actions3,
    [{send, Pid3}] = Actions4.
