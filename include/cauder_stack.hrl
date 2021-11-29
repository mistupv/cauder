-record(s_function, {
    mfa :: mfa(),
    env :: cauder_bindings:bindings(),
    expr :: [cauder_syntax:abstract_expr()],
    var :: cauder_syntax:af_variable()
}).

-record(s_block, {
    type :: 'if' | 'case' | 'receive',
    expr :: [cauder_syntax:abstract_expr()],
    var :: cauder_syntax:af_variable()
}).
