-record(log_spawn, {
    node :: node(),
    pid :: cauder_process:id(),
    success :: boolean()
}).

-record(log_send, {
    uid :: cauder_message:uid()
}).

-record(log_receive, {
    uid :: cauder_message:uid()
}).

-record(log_nodes, {
    nodes :: [node()]
}).

-record(log_start, {
    node :: node(),
    success :: boolean()
}).
