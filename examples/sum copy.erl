-module(sum).
-export([main/0]).

main() ->
    {X, Y} = {1, 2},
    X + Y.
