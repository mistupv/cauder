-module(acknowledge).
-export([main/0, other_main/0]).

main() ->
    MyPid = self(),
    OtherPid = spawn(?MODULE, other_main, []),
    OtherPid ! MyPid,
    receive
        ack -> ok;
        _ -> not_ok
    end.

other_main() ->
    receive
        Pid -> Pid ! ack
    end.
