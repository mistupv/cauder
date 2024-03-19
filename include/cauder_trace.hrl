-record(trace_nodes, {
    nodes :: [node()]
}).

-record(trace_start, {
    node :: node(),
    success :: boolean()
}).

-record(trace_spawn, {
    node :: node(),
    pid :: cauder_process:id(),
    success :: boolean()
}).

-record(trace_send, {
    uid :: cauder_message:uid()
    %dst :: cauder_process:id()
}).

%%-record(trace_deliver, {
%%    uid :: cauder_message:uid()
%%}).

-record(trace_receive, {
    uid :: cauder_message:uid()
}).

-record(trace_del, {
    key :: cauder_map:key(),
    atom :: atom(),
    pid :: cauder_process:id(),
    map :: [cauder_map:map_element()]
}).

-record(trace_reg, {
    key :: cauder_map:key(),
    atom :: atom(),
    pid :: cauder_process:id(),
    map :: [cauder_map:map_element()]
}).

-record(trace_sendA, {
    uid :: cauder_message:uid(),
    el :: cauder_map:map_element()
}).

-record(trace_read, {
    atom :: atom(),
    pid :: cauder_process:id() | undefined,
    map :: [cauder_map:map_element()]
}).
