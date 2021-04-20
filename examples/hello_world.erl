-module(hello_world).
-export([main/0, target/0, echo/0]).

main() ->
    P2 = spawn(?MODULE, echo, []),
    P3 = spawn(?MODULE, target, []),
    P3 ! hello,
    P2 ! {P3, world}.

target() ->
    receive
        A ->
            receive
                B -> {A, B}
            end
    end.

echo() ->
    receive
        {P, M} -> P ! M
    end.
