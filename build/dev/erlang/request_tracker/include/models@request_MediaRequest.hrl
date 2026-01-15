-record(media_request, {
    id :: integer(),
    media_type :: models@request:media_type(),
    title :: binary(),
    poster_url :: gleam@option:option(binary()),
    year :: gleam@option:option(integer()),
    request_status :: models@request:request_status(),
    requested_by :: gleam@option:option(binary()),
    requested_at :: gleam@option:option(binary()),
    download_status :: gleam@option:option(models@request:download_status()),
    download_progress :: gleam@option:option(float()),
    download_speed :: gleam@option:option(integer()),
    eta_seconds :: gleam@option:option(integer()),
    queue_position :: gleam@option:option(integer()),
    queue_status :: gleam@option:option(binary()),
    tmdb_id :: gleam@option:option(integer()),
    tvdb_id :: gleam@option:option(integer())
}).
