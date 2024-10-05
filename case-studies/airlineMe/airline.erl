-module(airline).
-export([main/0, agent/3, meManager/0]).

main() ->
    Main = self(),
    MePid = spawn(?MODULE, meManager, []),
    spawn(?MODULE, agent, [1, Main, MePid]),
    spawn(?MODULE, agent, [2, Main, MePid]),
    seats(3).

seats(Num) ->
    receive
        {numOfSeats, Pid} ->
            Pid ! {seats, Num},
            seats(Num);
        {sell, Pid} ->
            io:format("Seat ~p sold!~n",[Num]),
            Pid ! {booked, Num},
            seats(Num - 1)
    end.

agent(NAg, Pid, MePid) ->
    MePid ! {requestMe, self()},
    receive
        grantMe -> Pid ! {numOfSeats, self()},
		   receive
		       {seats, Num} when Num > 0 ->
			   Pid ! {sell, self()},
			   MePid ! {releaseMe},
			   receive
			       {booked, _} -> agent(NAg, Pid, MePid)
			   end;
		       _ ->
			   MePid ! {releaseMe},
			   io:format("Agent~p done!~n", [NAg])
		   end
    end.

meManager() ->
    receive
        {requestMe, Pid} -> Pid ! grantMe
    end,
    receive
        {releaseMe} -> meManager()
    end.
