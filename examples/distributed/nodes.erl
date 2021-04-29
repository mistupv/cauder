-module(nodes).
-export([start/0, child/1]).


start() ->
  Child = spawn(?MODULE, child, [self()]),
  U = nodes(),
  U.


child(_P) ->
  slave:start('mac', 'coolName').
