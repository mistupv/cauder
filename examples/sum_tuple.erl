-module(sum_tuple).
-export([main/2, sum_tuple/1]).

main(X, Y) ->
    Tuple = {X, Y},
    spawn(?MODULE, sum_tuple, [Tuple, Y]).

sum_tuple({X, Y}) -> X + Y.

sum_tuple({X, Y}, Z) -> X + Y + Z.
