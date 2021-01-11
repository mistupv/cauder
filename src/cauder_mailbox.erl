-module(cauder_mailbox).

%% API
-export([uid/0]).
-export([new/0, add/3, add_r/4, delete/2, pid_get/2, uid_member/2, uid_take/2, to_list/1]).

-export_type([mailbox/0, uid/0, message/0]).

-include("cauder.hrl").

-opaque mailbox() :: {
  #{uid() => {cauder_types:proc_id(), cauder_types:proc_id()}}, % #{Uid => {Source, Target}}
  #{cauder_types:proc_id() => #{cauder_types:proc_id() => queue:queue(message())}} % #{Target => #{Source => queue(Msg)}
}.
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

new() ->
  {maps:new(), maps:new()}.


%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' appended to
%% the rear.

-spec add(Message, Source, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Source :: cauder_types:proc_id(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

add(#message{uid = Uid} = Message, Source, {Index0, _} = Mailbox) when is_map_key(Uid, Index0) ->
  error({existing_uid, Uid}, [Message, Source, Mailbox]);
add(#message{uid = Uid, dest = Target} = Message, Source, {Index0, TargetMap0}) ->
  Index = maps:put(Uid, {Source, Target}, Index0),

  SourceMap0 = maps:get(Target, TargetMap0, maps:new()),
  Queue0 = maps:get(Source, SourceMap0, queue:new()),

  Queue = queue:in(Message, Queue0),
  SourceMap = maps:put(Source, Queue, SourceMap0),
  TargetMap = maps:put(Target, SourceMap, TargetMap0),

  {Index, TargetMap}.

%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' appended to
%% the front.

-spec add_r(Message, Source, QueuePos, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Source :: cauder_types:proc_id(),
  QueuePos :: pos_integer(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

add_r(#message{uid = Uid} = Message, Source, QueuePos, {Index0, _} = Mailbox) when is_map_key(Uid, Index0) ->
  error({existing_uid, Uid}, [Message, Source, QueuePos, Mailbox]);
add_r(#message{uid = Uid, dest = Target} = Message, Source, QueuePos, {Index0, TargetMap0}) ->
  Index = maps:put(Uid, {Source, Target}, Index0),

  SourceMap0 = maps:get(Target, TargetMap0, maps:new()),
  Queue0 = maps:get(Source, SourceMap0, queue:new()),

  Queue = queue_insert(QueuePos, Message, Queue0),
  SourceMap = maps:put(Source, Queue, SourceMap0),
  TargetMap = maps:put(Target, SourceMap, TargetMap0),

  {Index, TargetMap}.


%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec delete(Message, Mailbox1) -> {{Source, QueuePosition}, Mailbox2} when
  Message :: message(),
  Mailbox1 :: mailbox(),
  Source :: cauder_types:proc_id(),
  QueuePosition :: pos_integer(),
  Mailbox2 :: mailbox().

delete(#message{uid = Uid, dest = Target} = Message, {Index0, Targets0}) when is_map_key(Uid, Index0) ->
  {Source, Target} = maps:get(Uid, Index0),
  Index = maps:remove(Uid, Index0),

  SourceMap0 = maps:get(Target, Targets0),
  Queue0 = maps:get(Source, SourceMap0),

  Queue = queue_delete(Message, Queue0),
  SourceMap = maps:put(Source, Queue, SourceMap0),
  TargetMap = maps:put(Target, SourceMap, Targets0),

  QueuePos = queue_index_of(Message, Queue0),
  Mailbox = {Index, TargetMap},

  {{Source, QueuePos}, Mailbox}.


%%------------------------------------------------------------------------------
%% @doc Returns the messages whose destination is the given `Pid'.

-spec pid_get(Pid, Mailbox) -> Messages when
  Pid :: cauder_types:proc_id(),
  Mailbox :: mailbox(),
  Messages :: [message()].

pid_get(Pid, {_, Map}) when is_map_key(Pid, Map) ->
  lists:flatmap(fun queue:to_list/1, maps:values(maps:get(Pid, Map)));
pid_get(_, _) -> [].


%%------------------------------------------------------------------------------
%% @doc Returns `true' if there is a message in `Mailbox' whose uid compares
%% equal to `Uid', otherwise `false'.

-spec uid_member(Uid, Mailbox) -> boolean() when
  Uid :: uid(),
  Mailbox :: mailbox().

uid_member(Uid, {Index, _}) -> maps:is_key(Uid, Index).


%%------------------------------------------------------------------------------
%% @doc Searches the mailbox `Mailbox1' for a message whose uid compares equal
%% to `Uid'. Returns `{value, Message, Mailbox2}' if such a message is found,
%% otherwise `false'. `Mailbox2' is a copy of `Mailbox1' where the `Message'
%% has been removed.

-spec uid_take(Uid, Mailbox1) -> {value, {Message, Source, QueuePosition}, Mailbox2} | false when
  Uid :: uid(),
  Mailbox1 :: mailbox(),
  Message :: message(),
  Source :: cauder_types:proc_id(),
  QueuePosition :: pos_integer(),
  Mailbox2 :: mailbox().

uid_take(Uid, {Index, TargetMap} = Mailbox0) ->
  case maps:find(Uid, Index) of
    error ->
      false;
    {ok, {Source, Target}} ->
      Queue = maps:get(Source, maps:get(Target, TargetMap)),
      {value, Message} = lists:search(fun(M) -> M#message.uid =:= Uid end, queue:to_list(Queue)),
      QueuePos = queue_index_of(Message, Queue),
      {_, Mailbox} = delete(Message, Mailbox0),
      {value, {Message, Source, QueuePos}, Mailbox}
  end.


%%------------------------------------------------------------------------------
%% @doc Returns a complete list of messages, in arbitrary order, contained in
%% `Mailbox'.

-spec to_list(Mailbox) -> [Message] when
  Mailbox :: mailbox(),
  Message :: message().

to_list({_, TargetMap}) ->
  io:format("TargetMap: ~p\n", [TargetMap]),
  QueueToList = fun queue:to_list/1,
  MapToList = fun(SourceMap) -> lists:flatmap(QueueToList, maps:values(SourceMap)) end,
  List = lists:flatmap(MapToList, maps:values(TargetMap)),
  io:format("List: ~p\n", [List]),
  List.


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
%% @doc Returns the a copy of `Queue1' with `Item' inserted at `Index'.

-spec queue_insert(Index, Item, Queue1) -> Queue2 when
  Index :: pos_integer(),
  Item :: T,
  Queue1 :: queue:queue(T),
  Queue2 :: queue:queue(T),
  T :: term().

queue_insert(Index, Item, Queue) -> queue:from_list(list_insert(Index, Item, queue:to_list(Queue))).


%%------------------------------------------------------------------------------
%% @doc Returns the a copy of `List1' with `Item' inserted at `Index'.

-spec list_insert(Index, Item, List1) -> List2 when
  Index :: pos_integer(),
  Item :: T,
  List1 :: [T],
  List2 :: [T],
  T :: term().

list_insert(Index, Item, List) -> list_insert(Index, 1, Item, List, []).


list_insert(Index, Index, Item, List, Acc)      -> lists:reverse([Item | Acc], List);
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

index_of(_, [], _)                -> false;
index_of(Item, [Item | _], Index) -> Index;
index_of(Item, [_ | Tail], Index) -> index_of(Item, Tail, Index + 1).
