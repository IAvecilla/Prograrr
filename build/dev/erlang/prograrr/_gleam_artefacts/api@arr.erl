-module(api@arr).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api/arr.gleam").
-export([get_sonarr_queue/2, get_radarr_queue/2]).
-export_type([arr_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type arr_error() :: {http_error, api@http_client:http_error()} |
    {parse_error, binary()}.

-file("src/api/arr.gleam", 89).
-spec movie_decoder() -> gleam@dynamic@decode:decoder(gleam@option:option(integer())).
movie_decoder() ->
    gleam@dynamic@decode:optional_field(
        <<"tmdbId"/utf8>>,
        none,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_int/1}
        ),
        fun(Tmdb_id) -> gleam@dynamic@decode:success(Tmdb_id) end
    ).

-file("src/api/arr.gleam", 94).
-spec series_decoder() -> gleam@dynamic@decode:decoder(gleam@option:option(integer())).
series_decoder() ->
    gleam@dynamic@decode:optional_field(
        <<"tvdbId"/utf8>>,
        none,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_int/1}
        ),
        fun(Tvdb_id) -> gleam@dynamic@decode:success(Tvdb_id) end
    ).

-file("src/api/arr.gleam", 61).
-spec queue_item_decoder() -> gleam@dynamic@decode:decoder(models@request:arr_queue_item()).
queue_item_decoder() ->
    gleam@dynamic@decode:field(
        <<"id"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(Id) ->
            gleam@dynamic@decode:field(
                <<"title"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Title) ->
                    gleam@dynamic@decode:field(
                        <<"status"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_string/1},
                        fun(Status) ->
                            gleam@dynamic@decode:field(
                                <<"size"/utf8>>,
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(Size) ->
                                    gleam@dynamic@decode:field(
                                        <<"sizeleft"/utf8>>,
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_int/1},
                                        fun(Sizeleft) ->
                                            gleam@dynamic@decode:optional_field(
                                                <<"downloadId"/utf8>>,
                                                none,
                                                gleam@dynamic@decode:optional(
                                                    {decoder,
                                                        fun gleam@dynamic@decode:decode_string/1}
                                                ),
                                                fun(Download_id) ->
                                                    gleam@dynamic@decode:optional_field(
                                                        <<"movie"/utf8>>,
                                                        none,
                                                        gleam@dynamic@decode:optional(
                                                            movie_decoder()
                                                        ),
                                                        fun(Movie) ->
                                                            Tmdb_id = gleam@option:flatten(
                                                                Movie
                                                            ),
                                                            gleam@dynamic@decode:optional_field(
                                                                <<"series"/utf8>>,
                                                                none,
                                                                gleam@dynamic@decode:optional(
                                                                    series_decoder(
                                                                        
                                                                    )
                                                                ),
                                                                fun(Series) ->
                                                                    Tvdb_id = gleam@option:flatten(
                                                                        Series
                                                                    ),
                                                                    gleam@dynamic@decode:success(
                                                                        {arr_queue_item,
                                                                            Id,
                                                                            Title,
                                                                            Status,
                                                                            Size,
                                                                            Sizeleft,
                                                                            Tmdb_id,
                                                                            Tvdb_id,
                                                                            Download_id}
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            )
                                        end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/api/arr.gleam", 46).
-spec parse_queue_response(binary(), boolean()) -> {ok,
        list(models@request:arr_queue_item())} |
    {error, arr_error()}.
parse_queue_response(Body, _) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"records"/utf8>>,
            gleam@dynamic@decode:list(queue_item_decoder()),
            fun(Records) -> gleam@dynamic@decode:success(Records) end
        )
    end,
    case gleam@json:parse(Body, Decoder) of
        {ok, Items} ->
            {ok, Items};

        {error, _} ->
            {error,
                {parse_error, <<"Failed to parse *arr queue response"/utf8>>}}
    end.

-file("src/api/arr.gleam", 28).
-spec get_queue(binary(), binary(), boolean()) -> {ok,
        list(models@request:arr_queue_item())} |
    {error, arr_error()}.
get_queue(Base_url, Api_key, Is_sonarr) ->
    Url = case Is_sonarr of
        true ->
            <<Base_url/binary,
                "/api/v3/queue?pageSize=100&includeUnknownSeriesItems=true&includeSeries=true"/utf8>>;

        false ->
            <<Base_url/binary,
                "/api/v3/queue?pageSize=100&includeMovie=true"/utf8>>
    end,
    Headers = [{<<"X-Api-Key"/utf8>>, Api_key},
        {<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
    case api@http_client:get(Url, Headers) of
        {ok, Body} ->
            parse_queue_response(Body, Is_sonarr);

        {error, Err} ->
            {error, {http_error, Err}}
    end.

-file("src/api/arr.gleam", 13).
?DOC(" Fetch queue from Sonarr\n").
-spec get_sonarr_queue(binary(), binary()) -> {ok,
        list(models@request:arr_queue_item())} |
    {error, arr_error()}.
get_sonarr_queue(Base_url, Api_key) ->
    get_queue(Base_url, Api_key, true).

-file("src/api/arr.gleam", 21).
?DOC(" Fetch queue from Radarr\n").
-spec get_radarr_queue(binary(), binary()) -> {ok,
        list(models@request:arr_queue_item())} |
    {error, arr_error()}.
get_radarr_queue(Base_url, Api_key) ->
    get_queue(Base_url, Api_key, false).
