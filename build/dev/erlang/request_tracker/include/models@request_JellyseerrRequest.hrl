-record(jellyseerr_request, {
    id :: integer(),
    media_type :: binary(),
    status :: integer(),
    requested_by :: gleam@option:option(binary()),
    created_at :: gleam@option:option(binary()),
    media :: gleam@option:option(models@request:jellyseerr_media())
}).
