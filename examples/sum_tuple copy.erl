-module(sum_tuple).
-export([main/2, sum_tup/1]).

main(X, Y) ->
    Tup = {X, Y},
    sum_tup(Tup).

sum_tup({X, Y}) -> X + Y.
