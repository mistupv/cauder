-record(h_tau, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(h_self, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(h_node, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack()
}).

-record(h_nodes, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    nodes :: [node()]
}).

-record(h_spawn, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    node :: node(),
    pid :: cauder_process:id()
}).

-record(h_start, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    node :: node(),
    success :: boolean()
}).

-record(h_send, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    msg :: cauder_mailbox:message()
}).

-record(h_receive, {
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack :: cauder_stack:stack(),
    msg :: cauder_mailbox:message(),
    q_pos :: pos_integer()
}).
