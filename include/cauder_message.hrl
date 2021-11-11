-record(message, {
    uid :: cauder_message:uid(),
    src :: cauder_process:id(),
    dst :: cauder_process:id(),
    val :: term()
}).
