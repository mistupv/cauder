-record(log_nodes, {
    nodes :: [node()]
}).

-record(log_start, {
    node :: node(),
    success :: boolean()
}).

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

-record(log_del, {
    key :: cauder_map:key(),
    atom :: atom(),
    pid :: cauder_process:id(),
    map :: [cauder_map:map_element()]
}).

-record(log_reg, {
    key :: cauder_map:key(),
    atom :: atom(),
    pid :: cauder_process:id(),
    map :: [cauder_map:map_element()]
}).

-record(log_sendA, {
    uid :: cauder_message:uid(),
    el :: cauder_map:map_element()
}).

-record(log_read, {
    atom :: atom(),
    pid :: cauder_process:id() | undefined,
    map :: [cauder_map:map_element()]
}).
