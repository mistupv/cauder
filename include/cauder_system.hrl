-record(system, {
    mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
    pool :: cauder_pool:pool(),
    nodes = [] :: [node()],
    traces = maps:new() :: cauder_trace:trace(),
    x_trace = [] :: [cauder_types:x_trace()],
    roll = []
}).
