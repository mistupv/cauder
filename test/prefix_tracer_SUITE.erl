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
    [{examples_dir, SrcDir} | Config].

end_per_testcase(_, _Config) ->
    ok.

%%%=============================================================================

purchase(Config) ->
    ExamplesDir = ?config(examples_dir, Config),
    SrcFile = filename:join(ExamplesDir, ?FUNCTION_NAME),

    {ok, ?FUNCTION_NAME} = prefix_tracer:instrument(SrcFile),

    #trace_info{
        node = 'nonode@nohost',
        pid = 0,
        call = {?FUNCTION_NAME, main, []},
        tracing = success,
        return = {value, false},
        trace = #{
            0 := Actions0,
            1 := Actions1,
            2 := Actions2,
            3 := Actions3,
            4 := Actions4
        }
    } = prefix_tracer:trace(?FUNCTION_NAME, main, [], #{}),

    {
        [
            {spawn, {'nonode@nohost', 1}, success},
            {spawn, {'nonode@nohost', 2}, success},
            {spawn, {'nonode@nohost', 3}, success},
            {spawn, {'nonode@nohost', 4}, success}
        ],
        [
            {deliver, Uid2},
            {'receive', Uid2}
        ]
    } = lists:partition(
        fun
            ({spawn, _, _}) -> true;
            (_) -> false
        end,
        Actions0
    ),

    [{deliver, Uid0}, {deliver, Uid1}, {deliver, Uid3}] = lists:filter(
        fun
            ({deliver, _}) -> true;
            (_) -> false
        end,
        Actions1
    ),
    % Receive comes after deliver
    true = lists:member({'receive', Uid0}, lists:dropwhile(fun(Action) -> Action =/= {deliver, Uid0} end, Actions1)),
    true = lists:member({send, Uid2, 0}, Actions1),

    [{send, Uid0, 1}] = Actions2,
    [{send, Uid1, 1}] = Actions3,
    [{send, Uid3, 1}] = Actions4.
