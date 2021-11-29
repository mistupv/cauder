-record(system, {
    mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
    pool :: cauder_pool:pool(),
    nodes = [] :: [node()],
    log = cauder_log:new() :: cauder_log:log(),
    x_trace = [] :: [cauder_types:x_trace()],
    roll = []
}).
