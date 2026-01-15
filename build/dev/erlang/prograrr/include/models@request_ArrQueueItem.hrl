-record(arr_queue_item, {
    id :: integer(),
    title :: binary(),
    status :: binary(),
    size :: integer(),
    sizeleft :: integer(),
    tmdb_id :: gleam@option:option(integer()),
    tvdb_id :: gleam@option:option(integer()),
    download_id :: gleam@option:option(binary())
}).
