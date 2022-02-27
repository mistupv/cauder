-record(system, {
    mail = cauder_mailbox:new() :: cauder_mailbox:mailbox(),
    pool :: cauder_pool:pool(),
    nodes = [] :: [node()],
    maps = [] :: [{cauder_map:map_node()}],
    log = cauder_log:new() :: cauder_log:log(),
    trace = cauder_trace:new() :: cauder_trace:trace(),
    % TODO Remove?
    roll = []
}).
