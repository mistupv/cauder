-module(cauder_trace).

%% API
-export([]).
-export_type([
    trace/0,
    trace_action/0,
    trace_entry_search/0
]).

-type trace() :: #{cauder_process:id() => [trace_action()]}.
-type trace_action() ::
    {send, cauder_message:uid()}
    %| {deliver, cauder_message:uid()}
    | {'receive', cauder_message:uid()}
    | {nodes, [node()]}
    | {start, node(), success}
    | {start, node(), failure}
    | {spawn, {node(), cauder_process:id()}, success}
    | {spawn, {node(), cauder_process:id()}, failure}.

-type trace_entry_search() ::
    {send, cauder_message:uid()}
    | {'receive', cauder_message:uid()}
    | {start, node(), success}
    | {spawn, {'_', cauder_process:id()}, '_'}
    | {spawn, {node(), '_'}, failure}.
