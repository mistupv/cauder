-module(spawn).
-export([start/0]).


start() ->
  Pid = spawn('doesnt@exist', ?MODULE, child, []),
  Pid ! ciao,
  L = 3 * 3,
  io:format("L's value is: ~p~n", [L]).






child() ->
  io:format("Hello world.~n").
