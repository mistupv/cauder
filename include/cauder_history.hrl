-record(hist_tau, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(hist_self, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(hist_node, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(hist_nodes, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    nodes :: [node()]
}).

-record(hist_start, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    node :: node(),
    success :: boolean()
}).

-record(hist_spawn, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    node :: node(),
    pid :: cauder_process:id(),
    success :: boolean()
}).

-record(hist_send, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    msg :: cauder_message:message()
}).

-record(hist_receive, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    msg :: cauder_message:message(),
    q_pos :: pos_integer()
}).

-record(hist_regS, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    mapEl :: cauder_map:map_element(),
    node :: node()
}).

-record(hist_del, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    mapEl :: cauder_map:map_element(),
    map :: [cauder_map:map_element()],
    node :: node()
}).

-record(hist_readS, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    atom :: atom(),
    pid :: cauder_process:id(),
    mapEl :: [cauder_map:map_element()],
    node :: node()
}).

-record(hist_readF, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    atom :: atom() | cauder_process:id(),
    mapGhost :: [cauder_map:map_element()],
    node :: node()
}).

-record(hist_sendA, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    msg :: cauder_message:message(),
    mapEl :: cauder_map:map_element(),
    node :: node()
}).

-record(hist_registered, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    map :: [cauder_map:map_element()],
    node :: node()
}).
