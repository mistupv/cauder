-module(auto).
-export([main/0]).

main() ->
    self() ! hello,
    receive
        hello -> ok;
        _ -> not_ok
    end.
