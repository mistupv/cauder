-module(cauder_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link(Args) -> Return when
    Args :: term(),
    Return :: supervisor:startlink_ret().

start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

-spec init(Args) -> {'ok', {SupFlags, [ChildSpec]}} when
    Args :: term(),
    SupFlags :: supervisor:sup_flags(),
    ChildSpec :: supervisor:child_spec().

init(Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 0, period => 5},
    Debugger = #{id => cauder, start => {cauder, start_link, []}},
    GUI = #{id => cauder_wx, start => {cauder_wx, start_link, []}},

    ChildSpecs =
        case Args of
            [nogui] -> [Debugger];
            [wx] -> [Debugger, GUI]
        end,

    {ok, {SupFlags, ChildSpecs}}.
