-module(sleeping_barber).
-export([run/1, barber/1, client/1, create_clients/1, room/2]).

-define(CUT_DURATION, 20).

run(RoomSize) ->
  % create barber
  slave:start(mac,barber),
  BPid = spawn('barber@mac',?MODULE, barber, [?CUT_DURATION]),
  % run simulartor with barber PID
  slave:start(mac, room),
  RoomPid = spawn('room@mac',?MODULE, room, [{BPid, free},{RoomSize, [], 0}]),
  % spawn clients generator
  slave:start(mac,clients),
  spawn('clients@mac',?MODULE, create_clients, [RoomPid]).


barber(Duration) ->
  receive
    {client, Room, Client} ->
      timer:sleep(Duration),
      Client ! {barber, done},
      Room ! {barber, done},
      barber(Duration)
  end.

create_clients(Room) ->
  Rnd = 7 + rand:uniform(40), % XXX: you can play with constants
  timer:sleep(Rnd),
  spawn(?MODULE, client, [Room]),
  create_clients(Room).

client(Room) ->
  Room ! {new_client, self()},
  receive
    {room, no_space} ->
      ok;
    {barber, done} ->
      ok
  end.

room({BPid, Status}, {RoomSize, Clients, NumClient}) ->
  receive
    {new_client, ClientPid} when Status == free ->
      io:format("Client goes to barber~n"),
      BPid ! {client, self(), ClientPid},
      room({BPid, busy},{RoomSize, [], 0});
    {new_client, ClientPid} when Status == busy, RoomSize > NumClient ->
      io:format("Client goes to room, new size (~p)~n", [NumClient+1]),
      ClientPid ! {room, no_space},
      room({BPid, busy},{RoomSize, [ClientPid | Clients], length(Clients)+1});
    {new_client, ClientPid} ->
      io:format("No free space for client, room size (~p)~n", [NumClient]),
      room({BPid, busy},{RoomSize, Clients, length(Clients)});
    {barber, done} when NumClient > 0 ->
      Client = lists:nth(length(Clients), Clients),
      io:format("Take client ~p from room, new size (~p), room(~p)~n", [Client, NumClient-1, Clients]),
      RemainingClients = Clients -- [Client],
      BPid ! {client, self(), Client},
      room({BPid, busy},{RoomSize, RemainingClients, length(RemainingClients)});
    {barber, done} ->
      io:format("Barber finished, idle~n", []),
      room({BPid, free},{RoomSize, [], 0})
    end.
