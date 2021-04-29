-module(test_roll_start).
-export([main/0, second/0, hello/0]).

main() ->
  Pid = erlang:spawn(?MODULE, second, []),
  slave:start('another', 'node'),
  Pid ! start.

hello() ->
  io:format("Hello world!~n").

second()->
  receive start -> ok end,
  Nodes = nodes(),
  [FirstNode|_] = Nodes,
  erlang:spawn(FirstNode, ?MODULE, hello, []).
