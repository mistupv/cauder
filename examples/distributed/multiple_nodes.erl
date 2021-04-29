-module(multiple_nodes).

-export([start/0, main/1, ack/1]).

start() ->
  main(10).

ack(Pid)->
  Pid ! node().

client(MainPid, Node) ->
  {ok, SpawnedNode} = slave:start('mac',Node),
  spawn(SpawnedNode,?MODULE, ack, [MainPid]).


main(0) ->
  io:format("The network is now composed by: ~p~n", [nodes()]);
main(N) ->
  Pid = self(),
  Node = list_to_atom("node"++ integer_to_list(N)),
  client(Pid, Node),
  receive NodeName ->
      io:format("~p~n",[NodeName])
  end,
  main(N-1).



