-module(jump1).
-export([main/0]).

main() ->
    jump2:main(),
    local_main().

local_main() -> ok1.
