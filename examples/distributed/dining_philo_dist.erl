-module(dining_philo_dist).
-export([main/0, waiter/0, fork/1, philo/2]).



main() ->
  spawn_nodes(5),
  io:format("Nodes: ~p~n", [nodes()]),
  erlang:spawn(?MODULE, waiter, []).

spawn_nodes(0) ->
  ok;
spawn_nodes(N) ->
  slave:start('mac', string:concat("philo",integer_to_list(N))),
  spawn_nodes(N-1).

spawn_forks([], Dict) ->
  Dict;
spawn_forks([Node | Nodes], Dict) ->
  N = length([Node | Nodes]),
  Pair = {N, erlang:spawn(Node, ?MODULE, fork, [free])},
  spawn_forks(Nodes, [Pair] ++ Dict).

spawn_philos([], Dict, _) ->
  Dict;
spawn_philos([Node | Nodes], Dict, Pid) ->
  N = length([Node | Nodes]),
  Pair = {erlang:spawn(Node, ?MODULE, philo, [Pid, N]), N},
  spawn_philos(Nodes, [Pair] ++ Dict, Pid).

waiter() ->
  ListOfNodes = nodes(),
  ForkDict = spawn_forks(ListOfNodes, []),
  PhiloDict = spawn_philos(ListOfNodes, [], self()),
  waiter_1(ForkDict, PhiloDict).

waiter_1(ForkDict, PhiloDict) ->
  receive
    {eaten, PhiloPid} ->
      PhiloId = proplists:get_value(PhiloPid, PhiloDict),
      LeftForkId = PhiloId,
      RightForkId =  1 + (LeftForkId rem 5),    % Correct version
      % RightForkId =  1 + (5 rem LeftForkId),  % Buggy version
      LeftPid = proplists:get_value(LeftForkId, ForkDict),
      RightPid = proplists:get_value(RightForkId, ForkDict),
      set_state(LeftPid, free),
      set_state(RightPid, free);
    {hungry, PhiloPid} ->
      PhiloId = proplists:get_value(PhiloPid, PhiloDict),
      LeftForkId = PhiloId,
      RightForkId =  1 + (LeftForkId rem 5),
      LeftPid = proplists:get_value(LeftForkId, ForkDict),
      RightPid = proplists:get_value(RightForkId, ForkDict),
      LeftForkState = ask_state(LeftPid),
      RightForkState = ask_state(RightPid),
      case {LeftForkState, RightForkState} of
        {free, free} ->
          set_state(LeftPid, used),
          set_state(RightPid, used),
          PhiloPid ! eat;
        _ ->
          PhiloPid ! think
      end
  end,
  waiter_1(ForkDict, PhiloDict).

ask_state(Pid) ->
  Pid ! {get_state, self()},
  receive
    {state, State, _} -> State
  end.

set_state(Pid, State) ->
  Pid ! {set_state, State, self()},
  receive
    {been_set, _} -> ok
  end.

philo(WaiterPid, PhiloId) ->
  think(PhiloId),
  request_until_eaten(WaiterPid, PhiloId),
  philo(WaiterPid, PhiloId).

think(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is thinking~n"),
  ThinkTime = rand:uniform(1000),
  timer:sleep(ThinkTime),
  ok.

eat(PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " has eaten~n"),
  timer:sleep(1000),
  ok.

request_until_eaten(WaiterPid, PhiloId) ->
  io:fwrite("Philo " ++ integer_to_list(PhiloId) ++ " is hungry~n"),
  WaiterPid ! {hungry, self()},
  receive
    think ->
      think(PhiloId),
      request_until_eaten(WaiterPid, PhiloId);
    eat ->
      eat(PhiloId),
      WaiterPid ! {eaten, self()}
  end.

fork(State) ->
  receive
    {get_state, WaiterPid} ->
      WaiterPid ! {state, State, self()},
      fork(State);
    {set_state, NewState, WaiterPid} ->
      WaiterPid ! {been_set, self()},
      fork(NewState)
  end. 
