-module(cauder_mailbox).

%% API
-export([uid/0]).
-export([new/0, add/2, delete/2, pid_get/2, uid_member/2, uid_take/2, to_list/1]).

-export_type([mailbox/0, uid/0, message/0]).

-include("cauder.hrl").

-opaque mailbox() :: {[uid()], #{cauder_types:proc_id() => queue:queue(message())}}.
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
  {[], maps:new()}.


%%------------------------------------------------------------------------------
%% @doc Returns a new mailbox formed from `Mailbox1' with `Message' inserted.

-spec add(Message, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

add(#message{uid = Uid, dest = Dest} = Message, {Uids, Map0} = Mailbox) ->
  case lists:member(Uid, Uids) of
    true ->
      error({existing_uid, Uid}, [Message, Mailbox]);
    false ->
      QueueIn = fun(Queue) -> queue:in(Message, Queue) end,
      Map1 = maps:update_with(Dest, QueueIn, queue:from_list([Message]), Map0),
      {[Uid | Uids], Map1}
  end.


%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec delete(Message, Mailbox1) -> Mailbox2 when
  Message :: message(),
  Mailbox1 :: mailbox(),
  Mailbox2 :: mailbox().

delete(#message{uid = Uid, dest = Dest} = Message, {Uids, Map0} = Mailbox) ->
  case lists:member(Uid, Uids) of
    false -> Mailbox;
    true ->
      Queue = maps:get(Dest, Map0),
      NewQueue = queue:filter(fun(Msg) -> Msg =/= Message end, Queue),
      Map1 =
        case queue:is_empty(NewQueue) of
          true ->
            maps:remove(Dest, Map0);
          false ->
            Map0#{Dest := NewQueue}
        end,
      {lists:delete(Uid, Uids), Map1}
  end.


%%------------------------------------------------------------------------------
%% @doc Returns `Mailbox1', but with `Message' removed.

-spec pid_get(Pid, Mailbox) -> Queue | false when
  Pid :: cauder_types:proc_id(),
  Mailbox :: mailbox(),
  Queue :: queue:queue(message()).

pid_get(Pid, {_, Map}) ->
  maps:get(Pid, Map, queue:new()).


%%------------------------------------------------------------------------------
%% @doc Returns `true' if there is a message in `Mailbox' whose uid compares
%% equal to `Uid', otherwise `false'.

-spec uid_member(Uid, Mailbox) -> boolean() when
  Uid :: uid(),
  Mailbox :: mailbox().

uid_member(Uid, {Uids, _}) ->
  lists:member(Uid, Uids).


%%------------------------------------------------------------------------------
%% @doc Searches the mailbox `Mailbox1' for a message whose uid compares equal
%% to `Uid'. Returns {value, Message, Mailbox2} if such a message is found,
%% otherwise `false'. `Mailbox2' is a copy of `Mailbox1' where the `Message'
%% has been removed.

-spec uid_take(Uid, Mailbox1) -> {value, Message, Mailbox2} | false when
  Uid :: uid(),
  Mailbox1 :: mailbox(),
  Message :: message(),
  Mailbox2 :: mailbox().

uid_take(Uid, {Uids, Map}) ->
  case lists:delete(Uid, Uids) of
    Uids -> false;
    NewUids ->
      MatchUid =
        fun
          (Msg) -> Msg#message.uid =:= Uid
        end,
      MapIterate =
        fun
          Fun(I0) ->
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
        MapIterate(maps:iterator(Map))
      catch
        throw:{Key, Message, Queue} ->
          {value, Message, {NewUids, maps:put(Key, Queue, Map)}}
      end
  end.


%%------------------------------------------------------------------------------
%% @doc Returns the messages of `Mailbox' as a list. The messages returned are
%% sorted according to their uid.

-spec to_list(Mailbox) -> [Message] when
  Mailbox :: mailbox(),
  Message :: message().

to_list({Uids, Map0}) ->
  Map =
    lists:foldl(
      fun(Queue, Map1) ->
        lists:foldl(
          fun(Msg, Map2) ->
            maps:put(Msg#message.uid, Msg, Map2)
          end,
          Map1, queue:to_list(Queue))
      end,
      #{}, maps:values(Map0)),
  lists:map(fun(Uid) -> maps:get(Uid, Map) end, lists:reverse(Uids)).


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
