-module(start).
-export([start/0, child/1, helloWorld/0]).

start() ->
    Child = spawn(?MODULE, child, [self()]),
    slave:start('coolHost', 'coolName'),
    slave:start('coolHost', 'coolName'),
    U = nodes(),
    U.

child(_Parent) ->
    U = nodes(),
    register(pippo, self()),
    spawn('doesnt@exist', ?MODULE, helloWorld, []),
    slave:start('anotherCoolHost', 'coolName'),
    U.

helloWorld() ->
    register(pippo, self()),
    io:format("Hello world.~n").
