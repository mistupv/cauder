-module(cauder_map).

%% API
-export([new_key/0]).

-export([
    get_map/2,
    is_in_map/2,
    in_map/2,
    not_in_map/2,
    find_pid/2,
    find_atom/2,
    find_el/2,
    get_atoms_list/2,
    get_ghost_list/3
]).

-include("cauder.hrl").
-include("cauder_process.hrl").

-export_type([key/0, map_node/0]).

-opaque key() :: pos_integer().
-type alive() :: top | bot.
-type map_element() :: {atom(), cauder_process:id(), cauder_map:key(), cauder_map:alive()}.
-type map_node() :: {[map_element()], node()}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new UID.

-spec new_key() -> Key when
    Key :: cauder_map:key().

new_key() ->
    ets:update_counter(?APP_DB, last_key, 1, {last_key, -1}).

%%------------------------------------------------------------------------------
%% @doc Return the Map connected with the node Nid

-spec get_map(Maps, Node) -> Map when
    Maps :: [{cauder_map:map_node()}],
    Node :: node(),
    Map :: [cauder_map:map_element()].

get_map([], _) -> [];
get_map([{Map, Nid} | _], Nid) -> Map;
get_map([_ | Maps], Nid) -> get_map(Maps, Nid).

-spec is_in_map(Map, El) -> boolean() when
    Map :: [cauder_map:map_element()],
    El :: cauder_map:map_element().

is_in_map([], _) -> false;
is_in_map([El | _], El) -> true;
is_in_map([_ | M], El) -> is_in_map(M, El).

-spec in_map(Map, El) -> boolean() when
    Map :: [cauder_map:map_element()],
    El :: [cauder_map:map_element()].

in_map(_, []) ->
    true;
in_map(M, [El]) ->
    is_in_map(M, El);
in_map(M, [El | T]) ->
    case is_in_map(M, El) of
        true -> in_map(M, T);
        false -> false
    end.

-spec not_in_map(Map, El) -> true | false when
    Map :: [cauder_map:map_element()],
    El :: atom() | cauder_process:id().

not_in_map([], _) -> true;
not_in_map([{Atom, _, _, top} | _], Atom) -> false;
not_in_map([{_, Pid, _, top} | _], Pid) -> false;
not_in_map([{_, _, _, _} | M], El) -> not_in_map(M, El).

-spec find_pid(Atom, Map) -> {Pid, Key} | undefined when
    Atom :: atom(),
    Map :: [cauder_map:map_element()],
    Pid :: cauder_process:id(),
    Key :: cauder_map:key().

find_pid(_, []) -> undefined;
find_pid(Atom, [{Atom, Pid, K, top} | _]) -> {Pid, K};
find_pid(Atom, [{_, _, _, _} | Map]) -> find_pid(Atom, Map).

-spec find_atom(Pid, Map) -> {Atom, Key} | undefined when
    Pid :: cauder_process:id(),
    Map :: [cauder_map:map_element()],
    Atom :: atom(),
    Key :: cauder_map:key().

find_atom(_, []) -> undefined;
find_atom(Pid, [{Atom, Pid, K, top} | _]) -> {Atom, K};
find_atom(Pid, [{_, _, _, _} | Map]) -> find_atom(Pid, Map).

-spec get_atoms_list(Map, ListAtoms) -> Atoms when
    Map :: [cauder_map:map_element()],
    ListAtoms :: [atom()],
    Atoms :: [atom()].

get_atoms_list([], R) -> R;
get_atoms_list([{Atom, _, _, top} | T], R) -> get_atoms_list(T, [Atom | R]);
get_atoms_list([{_, _, _, _} | T], R) -> get_atoms_list(T, R).

-spec get_ghost_list(El, Map, ListGhost) -> Ghosts when
    El :: atom() | cauder_process:id(),
    Map :: [cauder_map:map_element()],
    ListGhost :: [cauder_map:map_element()],
    Ghosts :: [cauder_map:map_element()].

get_ghost_list(_, [], LG) -> LG;
get_ghost_list(Atom, [{Atom, Pid, K, bot} | Map], LG) -> get_ghost_list(Atom, Map, [{Atom, Pid, K, bot} | LG]);
get_ghost_list(Pid, [{Atom, Pid, K, bot} | Map], LG) -> get_ghost_list(Pid, Map, [{Atom, Pid, K, bot} | LG]);
get_ghost_list(El, [{_, _, _, _} | Map], LG) -> get_ghost_list(El, Map, LG).

-spec find_el(El, Map) -> Tuple | undefined when
    El :: atom() | cauder_process:id(),
    Map :: [cauder_map:map_element()],
    Tuple :: cauder_map:map_element().

find_el(_, []) -> undefined;
find_el(El, [{El, Pid, K, top} | _]) -> {El, Pid, K, top};
find_el(El, [{Atom, El, K, top} | _]) -> {Atom, El, K, top};
find_el(El, [{_, _, _, _} | Map]) -> find_atom(El, Map).
