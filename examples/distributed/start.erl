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
  spawn('doesnt@exist', ?MODULE, helloWorld, []),
  slave:start('anotherCoolHost', 'coolName'),
  U.



helloWorld() ->
  io:format("Hello world.~n").
