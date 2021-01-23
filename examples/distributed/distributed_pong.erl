-module(distributed_pong).

-export([main/0, pong/0]).

pong() ->
  receive
    {ping, Pid} ->
      Pid ! {pong, self()},
      io:format("Pong has just received ping~n"),
      pong()
  end.

ping(Pid) ->
  Pid ! {ping, self()},
  receive
    {pong, Pid} ->
      io:format("Ping has just received pong~n"),
      ping(Pid)
  end.

main() ->
  slave:start('mac','pong'),
  Pid = spawn('pong@mac',?MODULE, pong, []),
  ping(Pid).
