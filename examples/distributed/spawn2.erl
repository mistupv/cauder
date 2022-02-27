-module(spawn2).
-export([start/0, child1/0, child2/0, greet/0]).

start() ->
    spawn(?MODULE, child1, []),
    spawn(?MODULE, child2, []).

child1() ->
    register(pippo, self()),
    spawn('host@node', ?MODULE, greet, []).

child2() ->
    slave:start(node, host).

greet() ->
    register(pippo, self()),
    io:format("Hello world!~n").
