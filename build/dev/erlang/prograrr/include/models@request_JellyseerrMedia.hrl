-record(jellyseerr_media, {
    id :: integer(),
    tmdb_id :: gleam@option:option(integer()),
    tvdb_id :: gleam@option:option(integer()),
    title :: gleam@option:option(binary()),
    name :: gleam@option:option(binary()),
    poster_path :: gleam@option:option(binary()),
    release_date :: gleam@option:option(binary()),
    first_air_date :: gleam@option:option(binary())
}).
