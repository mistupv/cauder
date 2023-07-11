-module(math_server).
-export([main/1, server/0, logger/2, square/0, log/0, adder/0, sendRequest/1]).
%[{square, 10}, {adder, 20}, {log, 100}, {adder, 30}, {adder, 100}]
%casestudy:main([{square, 10}, {adder, 20}, {log, 100}, {adder, 30}, {adder, 100}]).
%casestudy:sendRequest([{square, 10}, {adder, 20}, {log, 100}, {adder, 30}, {adder, 100}]).


main(A)->
    register(server,spawn(?MODULE, server,[])),
    register(log,spawn(?MODULE, logger,[0,[]])),
    sendRequest(A).

server() ->
    receive
        {logged,{Res,Time}} ->
          io:format("LOGGED ~p TIME:~p\n", [Res,Time]);
        {reply, Res} ->
          io:format("RESULT:~p\n", [Res]),
          log ! Res;
        {Atom, Val} ->
          io:format("SEND REQUEST:~p\n", [{Atom, Val}]),
          case whereis(Atom) of
              undefined ->
                  register(Atom,spawn(?MODULE, Atom,[])),
                  Atom ! Val;
              _ ->
                  Atom ! Val
          end
    end,
    server().

logger(N,L)->
    receive
      Val ->
        server ! {logged,{Val, N}}, logger(N+1,L++[Val])
    end.

square() ->
  receive
    N ->
      server ! {reply,{square,N*N}}, square()
  end.

log() ->
  receive
    N ->
      server ! {reply,{log,math:log10(N)}}, log()
  end.

adder() -> adder(0).
adder(N)->
  receive
    Val ->
      server ! {reply,{adder,Val + N}}, adder(Val + N)
  end.

sendRequest([]) -> ok;
sendRequest([El | T]) ->
  server ! El, sendRequest(T).
