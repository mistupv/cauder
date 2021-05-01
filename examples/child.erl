-module(child).
-export([main/0, child/0]).

main() ->
    Child = spawn(?MODULE, child, []),
    Child ! {self(), hello},
    receive
        hello2 -> ok
    end.

child() ->
    receive
        {Parent, hello} -> Parent ! hello2
    end,
    main().
