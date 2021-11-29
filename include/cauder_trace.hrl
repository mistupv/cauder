-record(trace_spawn, {
    node :: node(),
    pid :: cauder_process:id(),
    success :: boolean()
}).

-record(trace_send, {
    uid :: cauder_message:uid()
}).

%%-record(trace_deliver, {
%%    uid :: cauder_message:uid()
%%}).

-record(trace_receive, {
    uid :: cauder_message:uid()
}).

-record(trace_nodes, {
    nodes :: [node()]
}).

-record(trace_start, {
    node :: node(),
    success :: boolean()
}).
