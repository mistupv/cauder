-module(pr).
-export([main/1]).

main(N) ->
    register(multiplication, spawn(?MODULE, summation, [N])),
    register(summation, spawn(?MODULE, multiplication, [N])).

%MAIN WITH OUT BUG
%main(N) ->
    %register(summation, spawn(?MODULE, summation, [N])),
    %register(multiplication, spawn(?MODULE, multiplication, [N])).


summation(N) ->
    register(sum, spawn(?MODULE, calculate_sum, [0])),
    spawn(?MODULE, single_step, [N, sum]),
    receive
        Sum ->
            io:format("\nThe summation is ~p\n", [Sum]),
            Sum
    end.

calculate_sum(TotalSum) ->
    receive
        done -> summation ! TotalSum;
        Sum -> calculate_sum(TotalSum + Sum)
    end.

multiplication(N) ->
    register(mult, spawn(?MODULE, calculate_mult, [1])),
    spawn(?MODULE, single_step, [N, mult]),
    receive
        Mult ->
            io:format("\nThe multiplication is ~p\n", [Mult]),
            Mult
    end.

calculate_mult(TotalMult) ->
    receive
        done -> multiplication ! TotalMult;
        Mult -> calculate_mult(TotalMult * Mult)
    end.

single_step(N, Atom) ->
    case N of
        0 ->
            whereis(Atom) ! done;
        _ ->
            whereis(Atom) ! f(N),
            single_step(N - 1, Atom)
    end.

f(N) -> N * N.
