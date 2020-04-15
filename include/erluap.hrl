
-type value() :: binary() | null.

-record(device, {
    device_type   :: device_type(),
    family :: value(),
    model :: value(),
    brand :: value()
}).

-record(agent, {
    family :: value(),
    version_major :: value(),
    version_minor :: value(),
    version_patch :: value(),
    version_patch_minor :: value()
}).

-type device_type() :: unknown | desktop | mobile | tablet.
-type device() :: #device{}.
-type os() :: #agent{}.
-type browser() :: #agent{}.
