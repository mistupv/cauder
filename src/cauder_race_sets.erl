-module(cauder_race_sets).

%% API
-export([race_sets/1, race_set/2]).

% TODO Remove
-ignore_xref([race_set/2]).

-include("cauder.hrl").

%%------------------------------------------------------------------------------
%% @doc Returns the race sets for all the processes.

-spec race_sets(Trace) -> RaceSets when
    Trace :: cauder_types:trace(),
    RaceSets :: cauder_types:race_sets().

race_sets(Trace) ->
    % TODO: OTP 24 - maps:filtermap/2
    RaceSets = maps:map(
        fun(Pid, Actions) ->
            lists:foldl(
                fun(Action, Acc) ->
                    case Action of
                        {'receive', Uid} ->
                            case race_set({Pid, Action}, Trace) of
                                [] ->
                                    Acc;
                                RaceSet ->
                                    Acc#{Uid => RaceSet}
                            end;
                        _ ->
                            Acc
                    end
                end,
                maps:new(),
                Actions
            )
        end,
        Trace
    ),
    maps:filter(fun(_, Map) -> maps:size(Map) =/= 0 end, RaceSets).

%%------------------------------------------------------------------------------
%% @doc Returns a set with a list of message tags for each process with at least
%% one racing message, ordered by the corresponding sending actions.

-spec race_set({Pid, ReceiveEvent}, Trace) -> RaceSet when
    Pid :: cauder_types:proc_id(),
    ReceiveEvent :: cauder_types:trace_action_receive(),
    Trace :: cauder_types:trace(),
    RaceSet :: cauder_types:race_set().

race_set({P, {'receive', L}} = Er, Trace) ->
    Ed = {P, {deliver, L}},

    true = belongs_to(Ed, Trace),
    true = belongs_to(Er, Trace),

    % Type: #{P1 => [{'send', L1, P}]}
    SendEvents = maps:map(
        fun(_, Actions) ->
            lists:filter(
                fun(Action) ->
                    case Action of
                        {send, L1, P} -> L1 =/= L;
                        _ -> false
                    end
                end,
                Actions
            )
        end,
        Trace
    ),

    Graph = trace_graph(Trace),
    RaceSet = maps:fold(
        fun(P1, Actions, Set) ->
            List = lists:filtermap(
                fun({'send', L1, _} = As1) ->
                    Es1 = {P1, As1},
                    Ed1 = {P, {deliver, L1}},

                    true = belongs_to(Es1, Trace),
                    true = belongs_to(Ed1, Trace),

                    case not happened_before(Ed, Es1, Graph) andalso not precedes(Ed1, Ed, Trace) of
                        true -> {true, L1};
                        false -> false
                    end
                end,
                Actions
            ),
            case List of
                [] -> Set;
                List -> ordsets:add_element(List, Set)
            end
        end,
        ordsets:new(),
        SendEvents
    ),
    digraph:delete(Graph),
    RaceSet.

%%------------------------------------------------------------------------------
%% @doc Checks whether the condition `Event ∈ Trace' holds or not.

-spec belongs_to(Event, Trace) -> boolean() when
    Event :: {cauder_types:proc_id(), cauder_types:trace_action()},
    Trace :: cauder_types:trace().

belongs_to({P, Action}, Trace) -> lists:member(Action, maps:get(P, Trace)).

-spec happened_before(Event1, Event2, Graph) -> boolean() when
    Event1 :: {cauder_types:proc_id(), cauder_types:trace_action()},
    Event2 :: {cauder_types:proc_id(), cauder_types:trace_action()},
    Graph :: digraph:graph().

happened_before({_P1, A1}, {_P2, A2}, G) ->
    case digraph:get_path(G, A1, A2) of
        false -> false;
        _Path -> true
    end.

-spec precedes(Event1, Event2, Trace) -> boolean() when
    Event1 :: {cauder_types:proc_id(), cauder_types:trace_action()},
    Event2 :: {cauder_types:proc_id(), cauder_types:trace_action()},
    Trace :: cauder_types:trace().

precedes({P, A1}, {P, A2}, Trace) ->
    Actions = maps:get(P, Trace),
    case lists:dropwhile(fun(A) -> A =/= A1 end, Actions) of
        [A1 | RestActions] -> lists:member(A2, RestActions);
        _ -> false
    end.

-spec trace_graph(Trace) -> Graph when
    Trace :: cauder_types:trace(),
    Graph :: digraph:graph().

trace_graph(Trace) ->
    G = digraph:new([acyclic]),

    #{
        spawn := SpawnActions,
        send := SendMap,
        deliver := DeliverUids,
        'receive' := ReceiveUids
    } = lists:foldl(
        fun(As, Map0) ->
            lists:foldl(
                fun(A, Map1) ->
                    digraph:add_vertex(G, A),
                    case A of
                        {spawn, _, success} ->
                            maps:update_with(spawn, fun(Spawns) -> [A | Spawns] end, Map1);
                        {send, Uid, Pid} ->
                            maps:update_with(send, fun(Pairs) -> maps:put(Uid, Pid, Pairs) end, Map1);
                        {deliver, Uid} ->
                            maps:update_with(deliver, fun(Uids) -> sets:add_element(Uid, Uids) end, Map1);
                        {'receive', Uid} ->
                            maps:update_with('receive', fun(Uids) -> sets:add_element(Uid, Uids) end, Map1);
                        _ ->
                            Map1
                    end
                end,
                Map0,
                As
            )
        end,
        #{
            spawn => [],
            send => maps:new(),
            deliver => sets:new(),
            'receive' => sets:new()
        },
        maps:values(Trace)
    ),

    % Condition 1: p1 = p2, a1 != deliver(_), a2 != deliver(_), and a1 < a2
    % Condition 2: p1 = p2, a1 = deliver(l), a2 = deliver(l'), l != l', and a1 < a2
    lists:foreach(
        fun(As) ->
            lists:foreach(
                fun(A1) ->
                    lists:foreach(
                        fun(A2) ->
                            case {A1, A2} of
                                {{deliver, L}, {deliver, L1}} when L =/= L1 -> digraph:add_edge(G, A1, A2);
                                {{deliver, _}, _} -> skip;
                                {_, {deliver, _}} -> skip;
                                {_, _} -> digraph:add_edge(G, A1, A2)
                            end
                        end,
                        % This guarantees that and a1 < a2
                        tl(lists:dropwhile(fun(A) -> A =/= A1 end, As))
                    )
                end,
                As
            )
        end,
        maps:values(Trace)
    ),

    % Condition 3: a1 = spawn(p2)
    lists:foreach(
        fun({spawn, {_, P2}, success} = A1) ->
            lists:foreach(
                fun(A2) ->
                    digraph:add_edge(G, A1, A2)
                end,
                maps:get(P2, Trace)
            )
        end,
        SpawnActions
    ),

    % Condition 4: a1 = send(l), and a2 = deliver(l)
    lists:foreach(
        fun({Uid, Pid}) ->
            digraph:add_edge(G, {send, Uid, Pid}, {deliver, Uid})
        end,
        maps:to_list(maps:with(sets:to_list(DeliverUids), SendMap))
    ),

    % Condition 5: a1 = deliver(l), and a2 = receive(l)
    lists:foreach(
        fun(Uid) ->
            digraph:add_edge(G, {deliver, Uid}, {'receive', Uid})
        end,
        sets:to_list(sets:intersection(DeliverUids, ReceiveUids))
    ),

    % Condition 6: p1 = p2, and a2 = exit
    lists:foreach(
        fun(As) ->
            case lists:reverse(As) of
                [{exit} = A2 | As1] ->
                    lists:foreach(fun(A1) -> digraph:add_edge(G, A1, A2) end, As1);
                _ ->
                    continue
            end
        end,
        maps:values(Trace)
    ),

    G.
