-module(cauder_message).

%% API
-export([new_uid/0]).

-include("cauder.hrl").
-include("cauder_message.hrl").

-export_type([uid/0, message/0]).

-opaque uid() :: pos_integer().
-type message() :: #message{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Returns a new UID.

-spec new_uid() -> Uid when
    Uid :: cauder_message:uid().

new_uid() ->
    ets:update_counter(?APP_DB, last_uid, 1, {last_uid, -1}).
