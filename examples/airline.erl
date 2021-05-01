%airline example

-module(airline).
-export([main/0, seatManager/1, booker/1]).

main() ->
    Pid = spawn(?MODULE, seatManager, [3]),
    spawn(?MODULE, booker, [Pid]),
    spawn(?MODULE, booker, [Pid]).

seatManager(NumOfSeats) ->
    receive
        {askForNum, Pid} ->
            Pid ! {num, NumOfSeats},
            seatManager(NumOfSeats);
        {book, Pid} ->
            io:format("Booked seat number ~w ~n", [NumOfSeats]),
            Pid ! {booked, NumOfSeats},
            seatManager(NumOfSeats - 1)
    end.

booker(Pid) ->
    Pid ! {askForNum, self()},
    receive
        {num, NumOfSeats} when
            NumOfSeats > 0
        ->
            Pid ! {book, self()},
            receive
                {booked, N} -> booker(Pid)
            end;
        {num, 0} ->
            0
    end.
