-module(cauder_mailbox).

%% API
-export([
    new/0,
    add/2,
    insert/3,
    remove/2,
    is_element/2,
    take/2,
    to_list/1
]).
-export([
    find_destination/2
]).

-include("cauder_message.hrl").

-export_type([mailbox/0]).

-record(mailbox, {
    index = maps:new() :: #{Uid :: cauder_message:uid() => {Src :: cauder_process:id(), Dest :: cauder_process:id()}},
    map = maps:new() :: #{
        Dest ::
            cauder_process:id() => orddict:orddict(
                Src :: cauder_process:id(),
                MsgQueue :: queue:queue(cauder_message:message())
            )
    }
}).

-opaque mailbox() :: #mailbox{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new empty mailbox.

-spec new() -> cauder_mailbox:mailbox().

new() -> #mailbox{}.

%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' appended to
%% the rear.

-spec add(Message, Mailbox1) -> Mailbox2 when
    Message :: cauder_message:message(),
    Mailbox1 :: cauder_mailbox:mailbox(),
    Mailbox2 :: cauder_mailbox:mailbox().

add(#message{uid = Uid} = Message, #mailbox{index = Index0} = Mailbox) when is_map_key(Uid, Index0) ->
    error({existing_uid, Uid}, [Message, Mailbox]);
add(#message{uid = Uid, src = Src, dst = Dest} = Message, #mailbox{index = Index0, map = DestMap0}) ->
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
    Message :: cauder_message:message(),
    QueuePos :: pos_integer(),
    Mailbox1 :: cauder_mailbox:mailbox(),
    Mailbox2 :: cauder_mailbox:mailbox().

insert(#message{uid = Uid} = Message, QueuePos, #mailbox{index = Index0} = Mailbox) when is_map_key(Uid, Index0) ->
    error({existing_uid, Uid}, [Message, QueuePos, Mailbox]);
insert(#message{uid = Uid, src = Src, dst = Dest} = Message, QueuePos, #mailbox{index = Index0, map = DestMap0}) ->
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

-spec remove(Message, Mailbox1) -> {QueuePosition, Mailbox2} when
    Message :: cauder_message:message(),
    Mailbox1 :: cauder_mailbox:mailbox(),
    QueuePosition :: pos_integer(),
    Mailbox2 :: cauder_mailbox:mailbox().

remove(#message{uid = Uid, src = Src, dst = Dst} = Message, #mailbox{index = Index0, map = DestMap0}) ->
    Index = maps:remove(Uid, Index0),

    SrcMap0 = maps:get(Dst, DestMap0),
    Queue0 = orddict:fetch(Src, SrcMap0),

    Queue = queue_delete(Message, Queue0),
    SrcMap = orddict:store(Src, Queue, SrcMap0),
    DestMap = maps:put(Dst, SrcMap, DestMap0),

    QueuePos = queue_index_of(Message, Queue0),
    Mailbox = #mailbox{index = Index, map = DestMap},

    {QueuePos, Mailbox}.

%%------------------------------------------------------------------------------
%% @doc Returns `true' if there is a message in `Mailbox' whose uid compares
%% equal to `Uid', otherwise `false'.

-spec is_element(Uid, Mailbox) -> boolean() when
    Uid :: cauder_message:uid(),
    Mailbox :: cauder_mailbox:mailbox().

is_element(Uid, #mailbox{index = Index}) ->
    maps:is_key(Uid, Index).

%%------------------------------------------------------------------------------
%% @doc Searches the mailbox `Mailbox1' for a message whose uid compares equal
%% to `Uid'. Returns `{value, Message, Mailbox2}' if such a message is found,
%% otherwise `false'. `Mailbox2' is a copy of `Mailbox1' where `Message' has
%% been removed.

-spec take(Uid, Mailbox1) -> {{Message, QueuePosition}, Mailbox2} | error when
    Uid :: cauder_message:uid(),
    Mailbox1 :: cauder_mailbox:mailbox(),
    Message :: cauder_message:message(),
    QueuePosition :: pos_integer(),
    Mailbox2 :: cauder_mailbox:mailbox().

take(Uid, #mailbox{index = Index, map = DestMap} = Mailbox0) ->
    case maps:find(Uid, Index) of
        error ->
            error;
        {ok, {Src, Dest}} ->
            SrcMap = maps:get(Dest, DestMap),
            Queue = orddict:fetch(Src, SrcMap),
            {value, Message} = lists:search(fun(M) -> M#message.uid =:= Uid end, queue:to_list(Queue)),
            QueuePos = queue_index_of(Message, Queue),
            {_, Mailbox} = remove(Message, Mailbox0),
            {{Message, QueuePos}, Mailbox}
    end.

%%------------------------------------------------------------------------------
%% @doc Returns a complete list of messages, in arbitrary order, contained in
%% `Mailbox'.

-spec to_list(Mailbox) -> [Message] when
    Mailbox :: cauder_mailbox:mailbox(),
    Message :: cauder_message:message().

to_list(#mailbox{map = DestMap}) ->
    QueueToList = fun({_, Queue}) -> queue:to_list(Queue) end,
    MapToList = fun(SrcMap) -> lists:flatmap(QueueToList, orddict:to_list(SrcMap)) end,
    lists:flatmap(MapToList, maps:values(DestMap)).

%%%=============================================================================
%%% Utils
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns the a list of messages queues from `Mailbox' with the given
%% `Destination'.

-spec find_destination(Destination, Mailbox) -> MessageQueues when
    Destination :: cauder_process:id(),
    Mailbox :: cauder_mailbox:mailbox(),
    MessageQueues :: [queue:queue(cauder_message:message())].

find_destination(Dest, #mailbox{map = DestMap}) when is_map_key(Dest, DestMap) ->
    lists:filtermap(
        fun({_, Queue}) ->
            case queue:is_empty(Queue) of
                true -> false;
                false -> {true, Queue}
            end
        end,
        orddict:to_list(maps:get(Dest, DestMap))
    );
find_destination(_, _) ->
    [].

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
%% @doc Returns the index of `Item' in `Queue'.

-spec queue_index_of(Item, Queue) -> Index when
    Item :: T,
    Queue :: queue:queue(T),
    Index :: pos_integer(),
    T :: term().

queue_index_of(Item, Queue) -> index_of(Item, queue:to_list(Queue)).

%%------------------------------------------------------------------------------
%% @doc Returns the index of `Item' in `List'.

-spec index_of(Item, List) -> Index when
    Item :: T,
    List :: [T],
    Index :: pos_integer(),
    T :: term().

index_of(Item, List) -> index_of(Item, List, 1).

index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tail], Index) -> index_of(Item, Tail, Index + 1).
