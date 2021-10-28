-module(cauder_process).

%% API
-export([]).
-export_type([
    process_map/0,
    proc_id/0,
    process/0
]).

-include("cauder.hrl").

-type process_map() :: #{proc_id() := process()}.
-type proc_id() :: pos_integer().
-type process() :: #proc{}.
