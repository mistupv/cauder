-record(trace_info, {
    % Initial node
    node :: node(),
    % Initial pid
    pid :: cauder_process:id(),
    % Initial function call
    call :: {module(), atom(), [term()]},
    % Whether the execution completed or the timeout was reached
    tracing :: success | timeout,
    % The value returned by the function application
    return = none :: none | {value, term()},
    % Compile time in microseconds
    comp :: non_neg_integer(),
    % Execution time in microseconds
    exec :: non_neg_integer(),
    trace = #{} :: cauder_trace:trace()
}).
