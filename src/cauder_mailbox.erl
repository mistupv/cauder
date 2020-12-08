-module(cauder_mailbox).

%% API
-export([uid/0]).
-export([new/0, add/2, delete/2, get/2, keymember/2, keytake/2, to_list/1]).

-export_type([mailbox/0, uid/0, message/0]).

-include("cauder.hrl").

-opaque mailbox() :: #{cauder_types:proc_id() => queue:queue(message())}.
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
  maps:new().


%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' inserted.

-spec add(Message, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

add(#message{dest = Dest} = Message, Mailbox) ->
  Append = fun(Queue) -> queue:in(Message, Queue) end,
  maps:update_with(Dest, Append, queue:from_list([Message]), Mailbox).


%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec delete(Message, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

delete(#message{dest = Dest} = Message, Mailbox) ->
  NotEquals = fun(Msg) -> Msg =/= Message end,
  QueueFilter = fun(Queue) -> queue:filter(NotEquals, Queue) end,
  Mailbox2 = maps:update_with(Dest, QueueFilter, Mailbox),
  case queue:is_empty(maps:get(Dest, Mailbox2)) of
    true -> maps:remove(Dest, Mailbox2);
    false -> Mailbox2
  end.


%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec get(Pid, Mailbox) -> Queue | false when
  Pid :: cauder_types:proc_id(),
  Mailbox :: mailbox(),
  Queue :: queue:queue(message()).

get(Destination, Mailbox) ->
  maps:get(Destination, Mailbox, queue:new()).


%%------------------------------------------------------------------------------
%% @doc Returns `true' if there is a message in `Mailbox' whose uid compares
%% equal to `Uid', otherwise `false'.

-spec keymember(Uid, Mailbox) -> boolean() when
  Uid :: uid(),
  Mailbox :: mailbox().

keymember(Uid, Mailbox) ->
  MatchUid = fun(Msg) -> Msg#message.uid =:= Uid end,
  QueueAny = fun({R, F}) -> lists:any(MatchUid, R) orelse lists:any(MatchUid, F) end,
  MapIterate =
    fun F(I) ->
      case maps:next(I) of
        {_, V, I2} -> QueueAny(V) orelse F(I2);
        none -> false
      end
    end,
  MapIterate(maps:iterator(Mailbox)).


%%------------------------------------------------------------------------------
%% @doc Searches the mailbox `Mailbox1' for a message whose uid compares equal
%% to `Uid'. Returns {value, Message, Mailbox2} if such a message is found,
%% otherwise `false'. `Mailbox2' is a copy of `Mailbox1' where the `Message'
%% has been removed.

-spec keytake(Uid, Mailbox1) -> {value, Message, Mailbox2} | false when
  Uid :: uid(),
  Mailbox1 :: mailbox(),
  Message :: message(),
  Mailbox2 :: mailbox().

keytake(Uid, Mailbox) ->
  MatchUid = fun(Msg) -> Msg#message.uid =:= Uid end,
  MapIterate =
    fun Fun(I0) ->
      case maps:next(I0) of
        {K, {R0, F0}, I1} ->
          case take_first(MatchUid, R0) of
            {value, Msg, R1} ->
              throw({K, Msg, {R1, F0}});
            false ->
              case take_first(MatchUid, F0) of
                {value, Msg, F1} ->
                  throw({K, Msg, {R0, F1}});
                false ->
                  Fun(I1)
              end
          end;
        none -> false
      end
    end,
  try
    MapIterate(maps:iterator(Mailbox))
  catch
    throw:{Key, Message, Queue} ->
      {value, Message, maps:put(Key, Queue, Mailbox)}
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the messages of `Mailbox' as a list. The messages returned are
%% sorted according to their uid.

-spec to_list(Mailbox) -> [Message] when
  Mailbox :: mailbox(),
  Message :: message().

to_list(Mailbox) ->
  CompareUid = fun(#message{uid = Uid1}, #message{uid = Uid2}) -> Uid1 =< Uid2 end,
  Messages = lists:flatmap(fun queue:to_list/1, maps:values(Mailbox)),
  lists:sort(CompareUid, Messages).


%%%=============================================================================
%%% Utils
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc If there is a `Value' in `List1' such that `Pred(Value)' returns `true',
%% returns `{value, Value, List2}' for the first such `Value' where `List2' is a
%% copy of `List1' where `Value' is deleted, otherwise returns `false'.

-spec take_first(Pred, List1) -> {value, Value, List2} | false when
  Pred :: fun((Value) -> boolean()),
  List1 :: [T],
  Value :: T,
  List2 :: [T].

take_first(Pred, L) ->
  take_first(Pred, L, []).

take_first(_, [], _) -> false;
take_first(Pred, [H | T], Es) ->
  case Pred(H) of
    true -> {value, H, lists:reverse(Es, T)};
    false -> take_first(Pred, T, [H | Es])
  end.
