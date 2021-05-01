-module(ex).
-export([ex/0]).

ex() ->
    X = 1,
    Y = 1,
    case X of
        Y -> true;
        _ -> false
    end.
