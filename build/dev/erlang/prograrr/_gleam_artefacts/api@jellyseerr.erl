-module(api@jellyseerr).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api/jellyseerr.gleam").
-export([get_requests/2]).
-export_type([jellyseerr_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type jellyseerr_error() :: {http_error, api@http_client:http_error()} |
    {parse_error, binary()}.

-file("src/api/jellyseerr.gleam", 92).
-spec media_details_decoder(integer()) -> gleam@dynamic@decode:decoder(models@request:jellyseerr_media()).
media_details_decoder(Tmdb_id) ->
    gleam@dynamic@decode:optional_field(
        <<"title"/utf8>>,
        none,
        gleam@dynamic@decode:optional(
            {decoder, fun gleam@dynamic@decode:decode_string/1}
        ),
        fun(Title) ->
            gleam@dynamic@decode:optional_field(
                <<"name"/utf8>>,
                none,
                gleam@dynamic@decode:optional(
                    {decoder, fun gleam@dynamic@decode:decode_string/1}
                ),
                fun(Name) ->
                    gleam@dynamic@decode:optional_field(
                        <<"posterPath"/utf8>>,
                        none,
                        gleam@dynamic@decode:optional(
                            {decoder, fun gleam@dynamic@decode:decode_string/1}
                        ),
                        fun(Poster_path) ->
                            gleam@dynamic@decode:optional_field(
                                <<"releaseDate"/utf8>>,
                                none,
                                gleam@dynamic@decode:optional(
                                    {decoder,
                                        fun gleam@dynamic@decode:decode_string/1}
                                ),
                                fun(Release_date) ->
                                    gleam@dynamic@decode:optional_field(
                                        <<"firstAirDate"/utf8>>,
                                        none,
                                        gleam@dynamic@decode:optional(
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1}
                                        ),
                                        fun(First_air_date) ->
                                            gleam@dynamic@decode:success(
                                                {jellyseerr_media,
                                                    Tmdb_id,
                                                    {some, Tmdb_id},
                                                    none,
                                                    Title,
                                                    Name,
                                                    Poster_path,
                                                    Release_date,
                                                    First_air_date}
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

-file("src/api/jellyseerr.gleam", 84).
-spec parse_media_details(binary(), integer()) -> {ok,
        models@request:jellyseerr_media()} |
    {error, jellyseerr_error()}.
parse_media_details(Body, Tmdb_id) ->
    Decoder = media_details_decoder(Tmdb_id),
    case gleam@json:parse(Body, Decoder) of
        {ok, Media} ->
            {ok, Media};

        {error, _} ->
            {error, {parse_error, <<"Failed to parse media details"/utf8>>}}
    end.

-file("src/api/jellyseerr.gleam", 69).
-spec get_media_details(binary(), binary(), binary(), integer()) -> {ok,
        models@request:jellyseerr_media()} |
    {error, jellyseerr_error()}.
get_media_details(Base_url, Api_key, Media_type, Tmdb_id) ->
    Url = <<<<<<<<Base_url/binary, "/api/v1/"/utf8>>/binary, Media_type/binary>>/binary,
            "/"/utf8>>/binary,
        (erlang:integer_to_binary(Tmdb_id))/binary>>,
    Headers = [{<<"X-Api-Key"/utf8>>, Api_key},
        {<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
    case api@http_client:get(Url, Headers) of
        {ok, Body} ->
            parse_media_details(Body, Tmdb_id);

        {error, Err} ->
            {error, {http_error, Err}}
    end.

-file("src/api/jellyseerr.gleam", 48).
-spec enrich_request(models@request:jellyseerr_request(), binary(), binary()) -> models@request:jellyseerr_request().
enrich_request(Req, Base_url, Api_key) ->
    case erlang:element(7, Req) of
        {some, Media} ->
            case erlang:element(3, Media) of
                {some, Tmdb_id} ->
                    Media_type = case erlang:element(3, Req) of
                        <<"tv"/utf8>> ->
                            <<"tv"/utf8>>;

                        _ ->
                            <<"movie"/utf8>>
                    end,
                    case get_media_details(
                        Base_url,
                        Api_key,
                        Media_type,
                        Tmdb_id
                    ) of
                        {ok, Details} ->
                            {jellyseerr_request,
                                erlang:element(2, Req),
                                erlang:element(3, Req),
                                erlang:element(4, Req),
                                erlang:element(5, Req),
                                erlang:element(6, Req),
                                {some, Details}};

                        {error, _} ->
                            Req
                    end;

                none ->
                    Req
            end;

        none ->
            Req
    end.

-file("src/api/jellyseerr.gleam", 151).
-spec nested_display_name_decoder() -> gleam@dynamic@decode:decoder(binary()).
nested_display_name_decoder() ->
    gleam@dynamic@decode:field(
        <<"displayName"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Display_name) -> gleam@dynamic@decode:success(Display_name) end
    ).

-file("src/api/jellyseerr.gleam", 156).
-spec basic_media_decoder() -> gleam@dynamic@decode:decoder(models@request:jellyseerr_media()).
basic_media_decoder() ->
    gleam@dynamic@decode:field(
        <<"id"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(Id) ->
            gleam@dynamic@decode:optional_field(
                <<"tmdbId"/utf8>>,
                none,
                gleam@dynamic@decode:optional(
                    {decoder, fun gleam@dynamic@decode:decode_int/1}
                ),
                fun(Tmdb_id) ->
                    gleam@dynamic@decode:optional_field(
                        <<"tvdbId"/utf8>>,
                        none,
                        gleam@dynamic@decode:optional(
                            {decoder, fun gleam@dynamic@decode:decode_int/1}
                        ),
                        fun(Tvdb_id) ->
                            gleam@dynamic@decode:success(
                                {jellyseerr_media,
                                    Id,
                                    Tmdb_id,
                                    Tvdb_id,
                                    none,
                                    none,
                                    none,
                                    none,
                                    none}
                            )
                        end
                    )
                end
            )
        end
    ).

-file("src/api/jellyseerr.gleam", 125).
-spec request_decoder() -> gleam@dynamic@decode:decoder(models@request:jellyseerr_request()).
request_decoder() ->
    gleam@dynamic@decode:field(
        <<"id"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_int/1},
        fun(Id) ->
            gleam@dynamic@decode:field(
                <<"type"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Media_type) ->
                    gleam@dynamic@decode:field(
                        <<"status"/utf8>>,
                        {decoder, fun gleam@dynamic@decode:decode_int/1},
                        fun(Status) ->
                            gleam@dynamic@decode:optional_field(
                                <<"requestedBy"/utf8>>,
                                none,
                                gleam@dynamic@decode:optional(
                                    nested_display_name_decoder()
                                ),
                                fun(Requested_by) ->
                                    gleam@dynamic@decode:optional_field(
                                        <<"createdAt"/utf8>>,
                                        none,
                                        gleam@dynamic@decode:optional(
                                            {decoder,
                                                fun gleam@dynamic@decode:decode_string/1}
                                        ),
                                        fun(Created_at) ->
                                            gleam@dynamic@decode:optional_field(
                                                <<"media"/utf8>>,
                                                none,
                                                gleam@dynamic@decode:optional(
                                                    basic_media_decoder()
                                                ),
                                                fun(Media) ->
                                                    gleam@dynamic@decode:success(
                                                        {jellyseerr_request,
                                                            Id,
                                                            Media_type,
                                                            Status,
                                                            Requested_by,
                                                            Created_at,
                                                            Media}
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

-file("src/api/jellyseerr.gleam", 111).
-spec parse_requests_response(binary()) -> {ok,
        list(models@request:jellyseerr_request())} |
    {error, jellyseerr_error()}.
parse_requests_response(Body) ->
    Decoder = begin
        gleam@dynamic@decode:field(
            <<"results"/utf8>>,
            gleam@dynamic@decode:list(request_decoder()),
            fun(Results) -> gleam@dynamic@decode:success(Results) end
        )
    end,
    case gleam@json:parse(Body, Decoder) of
        {ok, Requests} ->
            {ok, Requests};

        {error, _} ->
            {error,
                {parse_error, <<"Failed to parse Jellyseerr response"/utf8>>}}
    end.

-file("src/api/jellyseerr.gleam", 19).
?DOC(
    " Fetch all requests from Jellyseerr with full media details\n"
    " Only enriches non-available requests to reduce API calls\n"
).
-spec get_requests(binary(), binary()) -> {ok,
        list(models@request:jellyseerr_request())} |
    {error, jellyseerr_error()}.
get_requests(Base_url, Api_key) ->
    Url = <<Base_url/binary, "/api/v1/request?take=20&skip=0&sort=added"/utf8>>,
    Headers = [{<<"X-Api-Key"/utf8>>, Api_key},
        {<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
    case api@http_client:get(Url, Headers) of
        {ok, Body} ->
            case parse_requests_response(Body) of
                {ok, Requests} ->
                    Enriched = gleam@list:map(
                        Requests,
                        fun(Req) -> case erlang:element(4, Req) of
                                3 ->
                                    Req;

                                _ ->
                                    enrich_request(Req, Base_url, Api_key)
                            end end
                    ),
                    {ok, Enriched};

                {error, Err} ->
                    {error, Err}
            end;

        {error, Err@1} ->
            {error, {http_error, Err@1}}
    end.
