% sleeping barber problem (with a bug)

-module(barber).
-export([main/0, open_barber_shop/0, barber/1, customer/3]).

main() ->
    io:format("~nCustomers: John and Joe~n~n"),
    ShopPid = spawn(?MODULE, open_barber_shop, []),
    spawn(?MODULE, customer, [ShopPid, 'John', self()]),
    spawn(?MODULE, customer, [ShopPid, 'Joe', self()]),
    receive
        {Name1, State1} -> io:format("~p ~p~n", [Name1, State1])
    end,
    receive
        {Name2, State2} -> io:format("~p ~p~n", [Name2, State2])
    end,
    ShopPid ! stop.

%%customer
customer(ShopPid, Name, MainPid) ->
    ShopPid ! {new, {self(), Name}},
    receive
        X -> MainPid ! {Name, X}
    end.

%% barber %%
barber(ShopPid) ->
    ShopPid ! ready,
    receive
        wakeup ->
            barber(ShopPid);
        {customer, Customer} ->
            ShopPid ! {cut, Customer},
            barber(ShopPid)
    end.

%% shop %%
open_barber_shop() ->
    BarberPid = spawn(?MODULE, barber, [self()]),
    barber_shop(BarberPid, []).

%% main loop
barber_shop(BarberPid, CustomersInChairs) ->
    receive
        {cut, {CustomerPid, _}} ->
            CustomerPid ! finished,
            barber_shop(BarberPid, CustomersInChairs);
        ready ->
            respond_to_barber(BarberPid, CustomersInChairs);
        {new, CustomerInfo} ->
            add_customer_if_available(BarberPid, CustomerInfo, CustomersInChairs);
        stop ->
            stop
    end.

respond_to_barber(BarberPid, []) ->
    barber_shop(BarberPid, []);
respond_to_barber(BarberPid, List) ->
    BarberPid ! {customer, last(List)},
    barber_shop(BarberPid, removeCustomer(List)).

% assuming 2 chairs
add_customer_if_available(BarberPid, {CustomerPid, _CustomerName}, [X, Y | R]) ->
    CustomerPid ! no_room,
    barber_shop(BarberPid, [X, Y | R]);
add_customer_if_available(BarberPid, {CustomerPid, CustomerName}, List) ->
    BarberPid ! wakeup,
    barber_shop(BarberPid, [{CustomerPid, CustomerName} | List]).

last([A]) -> A;
last([_A | R]) -> last(R).

removeCustomer([_A | R]) -> R.
