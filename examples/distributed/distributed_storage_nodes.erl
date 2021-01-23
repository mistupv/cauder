-module(test).
-export([start/0, server/3, storageServer/1, client/1]).



start() ->
  slave:start('domain_a.1',storageNode),
  StorageServer = spawn('storageNode@domain_a.1', ?MODULE, storageServer, [[]]),
  ServerPid = spawn(?MODULE, server, [['domain_a.1','domain_b.1','domain_c.1'],1,StorageServer]),
  spawn(?MODULE, client, [ServerPid]).


server(Domains, DomainIndex, StorageServer) ->
  receive
    {new_data, D, Client} ->
      StorageServer ! {store, D, self()},
      receive
        ok ->
          Client ! ok;
        error ->
          throw(error)
      end,
  server(Domains, DomainIndex, StorageServer);
    {storage, full} ->
      SelectedDomain = lists:nth(DomainIndex, Domains),
      [Domain, Counter] =  string:split(atom_to_list(SelectedDomain), ".", trailing),
      {CounterInt, _} = string:to_integer(Counter),
      case CounterInt rem 5 =:= 0 of
        true ->
          slave:start(SelectedDomain, storageNode), %bug
          Address = list_to_atom(atom_to_list(storageNode) ++ "@" ++ atom_to_list(SelectedDomain)),
          NewStorageServer = spawn(Address,?MODULE, storageServer, [[]]),
          NewDomainIndex = DomainIndex rem 3 + 1,
          NewDomain = list_to_atom(Domain ++ "." ++ (lists:flatten(io_lib:format("~p", [CounterInt+1])))),
          NewDomains = lists:sublist(Domains,DomainIndex-1) ++ [NewDomain] ++ lists:sublist(Domains,DomainIndex+1,length(Domains)+1),
          server(NewDomains, NewDomainIndex, NewStorageServer);
        false ->
          NewDomain = list_to_atom(Domain ++ "." ++ (lists:flatten(io_lib:format("~p", [CounterInt+1])))),
          NewDomains = lists:sublist(Domains,DomainIndex-1) ++ [NewDomain] ++ lists:sublist(Domains,DomainIndex+1,length(Domains)+1),
          slave:start(NewDomain,storageNode),
          Address = list_to_atom(atom_to_list(storageNode) ++ "@" ++ atom_to_list(NewDomain)),
          NewStorageServer = spawn(Address, ?MODULE, storageServer, [[]]),
          io:format("~n~n~p~n~n",[NewDomains]),
          server(NewDomains, DomainIndex, NewStorageServer)
      end
  end.


storageServer(Data) ->
  receive
    {store, D, Server} ->
      Server ! ok,
      case length(Data) + 1 =:= 5 of
        true ->
          Server ! {storage, full};
        false -> ok
      end,
  storageServer(Data ++ [D]);
    {retrieve, _Index} ->
      ok
  end.


client(Server) ->
  timer:sleep(100),
  Data = rand:uniform(100),
  Server ! {new_data, Data, self()},
  receive
    ok ->
      client(Server);
    error ->
      error
  end.

