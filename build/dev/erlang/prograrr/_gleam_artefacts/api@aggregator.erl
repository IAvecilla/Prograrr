-module(api@aggregator).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api/aggregator.gleam").
-export([get_all_requests/1]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-file("src/api/aggregator.gleam", 108).
-spec find_radarr_queue_item(
    list(models@request:arr_queue_item()),
    gleam@option:option(integer())
) -> gleam@option:option(models@request:arr_queue_item()).
find_radarr_queue_item(Queue, Tmdb_id) ->
    case Tmdb_id of
        {some, Id} ->
            _pipe = Queue,
            _pipe@1 = gleam@list:find(
                _pipe,
                fun(Item) -> erlang:element(7, Item) =:= {some, Id} end
            ),
            gleam@option:from_result(_pipe@1);

        none ->
            none
    end.

-file("src/api/aggregator.gleam", 121).
-spec find_sonarr_queue_item(
    list(models@request:arr_queue_item()),
    gleam@option:option(integer())
) -> gleam@option:option(models@request:arr_queue_item()).
find_sonarr_queue_item(Queue, Tvdb_id) ->
    case Tvdb_id of
        {some, Id} ->
            _pipe = Queue,
            _pipe@1 = gleam@list:find(
                _pipe,
                fun(Item) -> erlang:element(8, Item) =:= {some, Id} end
            ),
            gleam@option:from_result(_pipe@1);

        none ->
            none
    end.

-file("src/api/aggregator.gleam", 134).
-spec find_torrent_by_hash(list(models@request:torrent_info()), binary()) -> gleam@option:option(models@request:torrent_info()).
find_torrent_by_hash(Torrents, Hash) ->
    Lower_hash = string:lowercase(Hash),
    _pipe = Torrents,
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(T) -> string:lowercase(erlang:element(2, T)) =:= Lower_hash end
    ),
    gleam@option:from_result(_pipe@1).

-file("src/api/aggregator.gleam", 164).
?DOC(
    " Extract words from a string for matching:\n"
    " - Convert to lowercase\n"
    " - Replace dots, underscores, hyphens with spaces\n"
    " - Split into words and filter out empty/short ones\n"
).
-spec extract_words(binary()) -> list(binary()).
extract_words(S) ->
    _pipe = S,
    _pipe@1 = string:lowercase(_pipe),
    _pipe@2 = gleam@string:replace(_pipe@1, <<"."/utf8>>, <<" "/utf8>>),
    _pipe@3 = gleam@string:replace(_pipe@2, <<"_"/utf8>>, <<" "/utf8>>),
    _pipe@4 = gleam@string:replace(_pipe@3, <<"-"/utf8>>, <<" "/utf8>>),
    _pipe@5 = gleam@string:replace(_pipe@4, <<"'"/utf8>>, <<""/utf8>>),
    _pipe@6 = gleam@string:replace(_pipe@5, <<":"/utf8>>, <<""/utf8>>),
    _pipe@7 = gleam@string:split(_pipe@6, <<" "/utf8>>),
    gleam@list:filter(_pipe@7, fun(Word) -> string:length(Word) > 1 end).

-file("src/api/aggregator.gleam", 144).
-spec find_torrent_by_name(list(models@request:torrent_info()), binary()) -> gleam@option:option(models@request:torrent_info()).
find_torrent_by_name(Torrents, Title) ->
    Title_words = extract_words(Title),
    _pipe = Torrents,
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(T) ->
            Torrent_words = extract_words(erlang:element(3, T)),
            gleam@list:all(
                Title_words,
                fun(Word) -> gleam@list:contains(Torrent_words, Word) end
            )
        end
    ),
    gleam@option:from_result(_pipe@1).

-file("src/api/aggregator.gleam", 176).
-spec extract_year(gleam@option:option(binary())) -> gleam@option:option(integer()).
extract_year(Date) ->
    case Date of
        {some, D} ->
            case gleam@string:split(D, <<"-"/utf8>>) of
                [Year | _] ->
                    _pipe = gleam_stdlib:parse_int(Year),
                    gleam@option:from_result(_pipe);

                _ ->
                    none
            end;

        none ->
            none
    end.

-file("src/api/aggregator.gleam", 45).
-spec combine_request(
    models@request:jellyseerr_request(),
    list(models@request:arr_queue_item()),
    list(models@request:arr_queue_item()),
    list(models@request:torrent_info())
) -> models@request:media_request().
combine_request(Js_req, Sonarr_queue, Radarr_queue, Torrents) ->
    Media_type = case erlang:element(3, Js_req) of
        <<"movie"/utf8>> ->
            movie;

        _ ->
            tv_show
    end,
    {Title@1, Poster_url, Year@1, Tmdb_id, Tvdb_id} = case erlang:element(
        7,
        Js_req
    ) of
        {some, Media} ->
            Title = begin
                _pipe = gleam@option:'or'(
                    erlang:element(5, Media),
                    erlang:element(6, Media)
                ),
                gleam@option:unwrap(_pipe, <<"Unknown"/utf8>>)
            end,
            Poster = case erlang:element(7, Media) of
                {some, Path} ->
                    {some,
                        <<"https://image.tmdb.org/t/p/w500"/utf8, Path/binary>>};

                none ->
                    none
            end,
            Year = extract_year(
                gleam@option:'or'(
                    erlang:element(8, Media),
                    erlang:element(9, Media)
                )
            ),
            {Title,
                Poster,
                Year,
                erlang:element(3, Media),
                erlang:element(4, Media)};

        none ->
            {<<"Unknown"/utf8>>, none, none, none, none}
    end,
    Queue_item = case Media_type of
        movie ->
            find_radarr_queue_item(Radarr_queue, Tmdb_id);

        tv_show ->
            find_sonarr_queue_item(Sonarr_queue, Tvdb_id)
    end,
    Torrent = case Queue_item of
        {some, Qi} ->
            case erlang:element(9, Qi) of
                {some, Dl_id} ->
                    find_torrent_by_hash(Torrents, Dl_id);

                none ->
                    find_torrent_by_name(Torrents, Title@1)
            end;

        none ->
            find_torrent_by_name(Torrents, Title@1)
    end,
    {media_request,
        erlang:element(2, Js_req),
        Media_type,
        Title@1,
        Poster_url,
        Year@1,
        models@request:jellyseerr_status_to_request_status(
            erlang:element(4, Js_req)
        ),
        erlang:element(5, Js_req),
        erlang:element(6, Js_req),
        begin
            _pipe@1 = Torrent,
            gleam@option:map(
                _pipe@1,
                fun(T) ->
                    models@request:torrent_state_to_download_status(
                        erlang:element(7, T)
                    )
                end
            )
        end,
        begin
            _pipe@2 = Torrent,
            gleam@option:map(
                _pipe@2,
                fun(T@1) -> erlang:element(4, T@1) * 100.0 end
            )
        end,
        begin
            _pipe@3 = Torrent,
            gleam@option:map(_pipe@3, fun(T@2) -> erlang:element(5, T@2) end)
        end,
        begin
            _pipe@4 = Torrent,
            gleam@option:map(_pipe@4, fun(T@3) -> erlang:element(6, T@3) end)
        end,
        begin
            _pipe@5 = Queue_item,
            gleam@option:map(_pipe@5, fun(Qi@1) -> erlang:element(2, Qi@1) end)
        end,
        begin
            _pipe@6 = Queue_item,
            gleam@option:map(_pipe@6, fun(Qi@2) -> erlang:element(4, Qi@2) end)
        end,
        Tmdb_id,
        Tvdb_id}.

-file("src/api/aggregator.gleam", 16).
?DOC(" Fetch and aggregate all data from configured services\n").
-spec get_all_requests(config:config()) -> list(models@request:media_request()).
get_all_requests(Config) ->
    Jellyseerr_requests = begin
        _pipe = api@jellyseerr:get_requests(
            erlang:element(3, Config),
            erlang:element(4, Config)
        ),
        gleam@result:unwrap(_pipe, [])
    end,
    Sonarr_queue = begin
        _pipe@1 = api@arr:get_sonarr_queue(
            erlang:element(5, Config),
            erlang:element(6, Config)
        ),
        gleam@result:unwrap(_pipe@1, [])
    end,
    Radarr_queue = begin
        _pipe@2 = api@arr:get_radarr_queue(
            erlang:element(7, Config),
            erlang:element(8, Config)
        ),
        gleam@result:unwrap(_pipe@2, [])
    end,
    Torrents = begin
        _pipe@3 = api@qbittorrent:get_torrents_with_auth(
            erlang:element(9, Config),
            erlang:element(10, Config),
            erlang:element(11, Config)
        ),
        gleam@result:unwrap(_pipe@3, [])
    end,
    _pipe@4 = Jellyseerr_requests,
    gleam@list:map(
        _pipe@4,
        fun(Req) ->
            combine_request(Req, Sonarr_queue, Radarr_queue, Torrents)
        end
    ).
