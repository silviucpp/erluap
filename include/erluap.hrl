
-type value() :: binary() | null.

-record(device, {
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

-type device() :: #device{}.
-type os() :: #agent{}.
-type browser() :: #agent{}.