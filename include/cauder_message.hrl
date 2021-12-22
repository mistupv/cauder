-record(message, {
    uid :: cauder_message:uid(),
    src :: cauder_process:id(),
    dst :: cauder_process:id(),
    val :: term()
}).

-define(SCHEDULER_Random, random).
-define(SCHEDULER_Manual, manual).
