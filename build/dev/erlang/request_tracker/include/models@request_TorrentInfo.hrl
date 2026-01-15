-record(torrent_info, {
    hash :: binary(),
    name :: binary(),
    progress :: float(),
    dlspeed :: integer(),
    eta :: integer(),
    state :: binary()
}).
