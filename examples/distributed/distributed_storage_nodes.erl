-module(distributed_storage_nodes).
-export([start/0, server/4, storageManager/1, client/1]).



start() ->
  slave:start('domain_a.1',storageNode1),
  StorageNode = spawn('storageNode1@domain_a.1', ?MODULE, storageManager, [[]]),
  ServerPid = spawn(?MODULE, server, [['domain_a.1','domain_b.1','domain_c.1'],1,StorageNode,1]),
  spawn(?MODULE, client, [ServerPid]).


server(Domains, DomainIndex, StorageServer, StorageCounter) ->
  receive
    {new_data, D, Client} ->
      StorageServer ! {store, D, self()},
      receive
        ok ->
          Client ! ok,
          server(Domains, DomainIndex, StorageServer, StorageCounter);
        {ok, full} ->
          SelectedDomain = lists:nth(DomainIndex, Domains),
          case StorageCounter rem 5 =:= 0 of
            true ->
              [Domain, Counter] =  string:split(atom_to_list(SelectedDomain), ".", trailing),
              {CounterInt, _} = string:to_integer(Counter),
              slave:start(SelectedDomain,list_to_atom(atom_to_list(storageNode)++ "1")), %bug
              NewStorageNode = list_to_atom(atom_to_list(storageNode)++ "1@" ++ atom_to_list(SelectedDomain)),
              NewStorageManager = spawn(NewStorageNode,?MODULE, storageManager, [[]]),
              NewDomainIndex = DomainIndex rem 3 + 1,
              NewDomain = list_to_atom(Domain ++ "." ++ (lists:flatten(io_lib:format("~p", [CounterInt+1])))),
              NewDomains = lists:sublist(Domains,DomainIndex-1) ++ [NewDomain] ++ lists:sublist(Domains,DomainIndex+1,length(Domains)+1),
              Client ! ok,
              server(NewDomains, NewDomainIndex, NewStorageManager, 1);
            false ->
              {ok, NewNode} = slave:start(list_to_atom(atom_to_list(SelectedDomain)), list_to_atom(atom_to_list(storageNode)++ (lists:flatten(io_lib:format("~p", [StorageCounter+1]))))),
              NewStorageManager = spawn(NewNode, ?MODULE, storageManager, [[]]),
              Client ! ok,
              server(Domains, DomainIndex, NewStorageManager, StorageCounter+1)
          end;
        error ->
          throw(error)
      end
  end.


storageManager(Data) ->
  receive
    {store, D, Server} ->
      case length(Data) + 1 =:= 5 of
        true ->
          Server ! {ok, full};
        false ->
          Server ! ok
      end,
  storageManager(Data ++ [D]);
    {retrieve, _Index} ->
      ok
  end.


client(Server) ->
  timer:sleep(100),
  Server ! {new_data, a, self()},
  receive
    ok ->
      client(Server);
    error ->
      error
  end.

