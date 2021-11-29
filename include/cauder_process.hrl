-record(process, {
    node :: node(),
    pid :: cauder_process:id(),
    entry_point :: mfa(),
    env = cauder_bindings:new() :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    stack = cauder_stack:new() :: cauder_stack:stack(),
    hist = cauder_history:new() :: cauder_history:history()
}).
