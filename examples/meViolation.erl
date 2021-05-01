%code showing a mutual exclusion violation
%the two concurrent increments of the variable managed by varManager
%may lead to final value 1 instead of 2 under some schedulings

-module(meViolation).
-export([main/0, meManager/0, varManager/1, incrementer/2]).

main() ->
    MePid = spawn(?MODULE, meManager, []),
    XPid = spawn(?MODULE, varManager, [0]),
    spawn(?MODULE, incrementer, [MePid, XPid]),
    spawn(?MODULE, incrementer, [MePid, XPid]).

meManager() ->
    receive
        {request, Pid} -> Pid ! answer
    end,
    receive
        {release} -> meManager()
    end.

varManager(Val) ->
    io:format("Variable value:~b~n", [Val]),
    receive
        {write, NewVal} -> varManager(NewVal);
        {read, Pid} -> Pid ! Val
    end,
    varManager(Val).

incrementer(MePid, XPid) ->
    MePid ! {request, self()},
    receive
        answer ->
            XPid ! {read, self()},
            receive
                X ->
                    XPid ! {write, X + 1},
                    MePid ! {release}
            end
    end.
