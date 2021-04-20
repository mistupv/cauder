-module(provaMod).
-export([main/0, inc/1]).

main() ->
    X = prova:quad(5),
    inc(X).

inc(X) ->
    X + 1.
