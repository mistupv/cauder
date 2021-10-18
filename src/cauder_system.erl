-module(cauder_system).

%% API
-export([]).
-export_type([system/0]).

-include("cauder.hrl").

-type system() :: #sys{}.
