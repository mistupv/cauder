-module(cauder_mailbox).

%% API
-export([uid/0]).
-export([new/0, add/2, insert/3, delete/2, pid_get/2, uid_member/2, uid_take/2, to_list/1]).

-ignore_xref([insert/3]).

-export_type([mailbox/0, uid/0, message/0]).

-include("cauder.hrl").

-record(mailbox, {
    index = maps:new() :: #{Uid :: uid() => {Src :: cauder_types:proc_id(), Dest :: cauder_types:proc_id()}},
    map = maps:new() :: #{
        Dest ::
            cauder_types:proc_id() => orddict:orddict(Src :: cauder_types:proc_id(), MsgQueue :: queue:queue(message()))
    }
}).

-opaque mailbox() :: #mailbox{}.
-opaque uid() :: pos_integer().
-type message() :: #message{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new and unique message identifier.

-spec uid() -> uid().

uid() ->
    ets:update_counter(?APP_DB, last_uid, 1, {last_uid, -1}).

%%------------------------------------------------------------------------------
%% @doc Returns a new empty mailbox.

-spec new() -> mailbox().

new() -> #mailbox{}.

%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' appended to
%% the rear.

-spec add(Message, Mailbox1) -> Mailbox2 when
    Message :: message(),
    Mailbox1 :: mailbox(),
    Mailbox2 :: mailbox().

add(#message{uid = Uid} = Message, #mailbox{index = Index0} = Mailbox) when is_map_key(Uid, Index0) ->
    error({existing_uid, Uid}, [Message, Mailbox]);
add(#message{uid = Uid, src = Src, dest = Dest} = Message, #mailbox{index = Index0, map = DestMap0}) ->
    Index = maps:put(Uid, {Src, Dest}, Index0),

    SrcMap0 = maps:get(Dest, DestMap0, orddict:new()),
    Queue0 =
        case orddict:find(Src, SrcMap0) of
            {ok, Value} -> Value;
            error -> queue:new()
        end,

    Queue = queue:in(Message, Queue0),
    SrcMap = orddict:store(Src, Queue, SrcMap0),
    DestMap = maps:put(Dest, SrcMap, DestMap0),

    #mailbox{index = Index, map = DestMap}.

%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' inserted at
%% `QueuePos'.

-spec insert(Message, QueuePos, Mailbox1) -> Mailbox2 when
    Message :: message(),
    QueuePos :: pos_integer(),
    Mailbox1 :: mailbox(),
    Mailbox2 :: mailbox().

insert(#message{uid = Uid} = Message, QueuePos, #mailbox{index = Index0} = Mailbox) when is_map_key(Uid, Index0) ->
    error({existing_uid, Uid}, [Message, QueuePos, Mailbox]);
insert(#message{uid = Uid, src = Src, dest = Dest} = Message, QueuePos, #mailbox{index = Index0, map = DestMap0}) ->
    Index = maps:put(Uid, {Src, Dest}, Index0),

    SrcMap0 = maps:get(Dest, DestMap0, orddict:new()),
    Queue0 =
        case orddict:find(Src, SrcMap0) of
            {ok, Value} -> Value;
            error -> queue:new()
        end,

    Queue = queue_insert(QueuePos, Message, Queue0),
    SrcMap = orddict:store(Src, Queue, SrcMap0),
    DestMap = maps:put(Dest, SrcMap, DestMap0),

    #mailbox{index = Index, map = DestMap}.

%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec delete(Message, Mailbox1) -> {QueuePosition, Mailbox2} when
    Message :: message(),
    Mailbox1 :: mailbox(),
    QueuePosition :: pos_integer(),
    Mailbox2 :: mailbox().

delete(#message{uid = Uid, src = Src, dest = Dest} = Message, #mailbox{index = Index0, map = DestMap0}) ->
    Index = maps:remove(Uid, Index0),

    SrcMap0 = maps:get(Dest, DestMap0),
    Queue0 = orddict:fetch(Src, SrcMap0),

    Queue = queue_delete(Message, Queue0),
    SrcMap = orddict:store(Src, Queue, SrcMap0),
    DestMap = maps:put(Dest, SrcMap, DestMap0),

    QueuePos = queue_index_of(Message, Queue0),
    Mailbox = #mailbox{index = Index, map = DestMap},

    {QueuePos, Mailbox}.

%%------------------------------------------------------------------------------
%% @doc Returns the a list of messages queues from `Mailbox' whose destination
%% is the given `Destination'.

-spec pid_get(Destination, Mailbox) -> MessageQueues when
    Destination :: cauder_types:proc_id(),
    Mailbox :: mailbox(),
    MessageQueues :: [queue:queue(message())].

pid_get(Dest, #mailbox{map = DestMap}) when is_map_key(Dest, DestMap) ->
    lists:filtermap(
        fun({_, Queue}) ->
            case queue:is_empty(Queue) of
                true -> false;
                false -> {true, Queue}
            end
        end,
        orddict:to_list(maps:get(Dest, DestMap))
    );
pid_get(_, _) ->
    [].

%%------------------------------------------------------------------------------
%% @doc Returns `true' if there is a message in `Mailbox' whose uid compares
%% equal to `Uid', otherwise `false'.

-spec uid_member(Uid, Mailbox) -> boolean() when
    Uid :: uid(),
    Mailbox :: mailbox().

uid_member(Uid, #mailbox{index = Index}) -> maps:is_key(Uid, Index).

%%------------------------------------------------------------------------------
%% @doc Searches the mailbox `Mailbox1' for a message whose uid compares equal
%% to `Uid'. Returns `{value, Message, Mailbox2}' if such a message is found,
%% otherwise `false'. `Mailbox2' is a copy of `Mailbox1' where `Message' has
%% been removed.

-spec uid_take(Uid, Mailbox1) -> {{Message, QueuePosition}, Mailbox2} | false when
    Uid :: uid(),
    Mailbox1 :: mailbox(),
    Message :: message(),
    QueuePosition :: pos_integer(),
    Mailbox2 :: mailbox().

uid_take(Uid, #mailbox{index = Index, map = DestMap} = Mailbox0) ->
    case maps:find(Uid, Index) of
        error ->
            false;
        {ok, {Src, Dest}} ->
            SrcMap = maps:get(Dest, DestMap),
            Queue = orddict:fetch(Src, SrcMap),
            {value, Message} = lists:search(fun(M) -> M#message.uid =:= Uid end, queue:to_list(Queue)),
            QueuePos = queue_index_of(Message, Queue),
            {_, Mailbox} = delete(Message, Mailbox0),
            {{Message, QueuePos}, Mailbox}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a complete list of messages, in arbitrary order, contained in
%% `Mailbox'.

-spec to_list(Mailbox) -> [Message] when
    Mailbox :: mailbox(),
    Message :: message().

to_list(#mailbox{map = DestMap}) ->
    QueueToList = fun({_, Queue}) -> queue:to_list(Queue) end,
    MapToList = fun(SrcMap) -> lists:flatmap(QueueToList, orddict:to_list(SrcMap)) end,
    lists:flatmap(MapToList, maps:values(DestMap)).

%%%=============================================================================
%%% Utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a copy of `Queue1' where the first element matching `Item' is
%% deleted, if there is such an element.

-spec queue_delete(Item, Queue1) -> Queue2 when
    Item :: T,
    Queue1 :: queue:queue(T),
    Queue2 :: queue:queue(T),
    T :: term().

queue_delete(Item, Queue) -> queue:from_list(lists:delete(Item, queue:to_list(Queue))).

%%------------------------------------------------------------------------------
%% @doc Returns a copy of `Queue1' with `Item' inserted at `Index'.

-spec queue_insert(Index, Item, Queue1) -> Queue2 when
    Index :: pos_integer(),
    Item :: T,
    Queue1 :: queue:queue(T),
    Queue2 :: queue:queue(T),
    T :: term().

queue_insert(Index, Item, Queue) -> queue:from_list(list_insert(Index, Item, queue:to_list(Queue))).

%%------------------------------------------------------------------------------
%% @doc Returns a copy of `List1' with `Item' inserted at `Index'.

-spec list_insert(Index, Item, List1) -> List2 when
    Index :: pos_integer(),
    Item :: T,
    List1 :: [T],
    List2 :: [T],
    T :: term().

list_insert(Index, Item, List) -> list_insert(Index, 1, Item, List, []).

list_insert(Index, Index, Item, List, Acc) -> lists:reverse([Item | Acc], List);
list_insert(Index, CurrIdx, Item, [H | T], Acc) -> list_insert(Index, CurrIdx + 1, Item, T, [H | Acc]).

%%------------------------------------------------------------------------------
%% @doc Returns the index of `Item' in `Queue' or `false' if there is no such
%% item.

-spec queue_index_of(Item, Queue) -> Index | false when
    Item :: T,
    Queue :: queue:queue(T),
    Index :: pos_integer(),
    T :: term().

queue_index_of(Item, Queue) -> index_of(Item, queue:to_list(Queue)).

%%------------------------------------------------------------------------------
%% @doc Returns the index of `Item' in `List' or `false' if there is no such
%% item.

-spec index_of(Item, List) -> Index | false when
    Item :: T,
    List :: [T],
    Index :: pos_integer(),
    T :: term().

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _) -> false;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tail], Index) -> index_of(Item, Tail, Index + 1).
