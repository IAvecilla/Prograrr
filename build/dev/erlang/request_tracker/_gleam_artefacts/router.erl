-module(router).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/router.gleam").
-export([handle_request/2]).
-export_type([context/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type context() :: {context, config:config(), binary()}.

-file("src/router.gleam", 45).
-spec handle_requests(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_requests(Req, Ctx) ->
    case erlang:element(2, Req) of
        get ->
            Requests = api@aggregator:get_all_requests(erlang:element(2, Ctx)),
            Body = begin
                _pipe = models@request:media_requests_to_json(Requests),
                _pipe@1 = gleam_json_ffi:json_to_iodata(_pipe),
                unicode:characters_to_binary(_pipe@1)
            end,
            wisp:json_response(Body, 200);

        options ->
            wisp:ok();

        _ ->
            wisp:method_not_allowed([get])
    end.

-file("src/router.gleam", 61).
-spec handle_health(gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()).
handle_health(_) ->
    Body = begin
        _pipe = gleam@json:object(
            [{<<"status"/utf8>>, gleam@json:string(<<"ok"/utf8>>)}]
        ),
        _pipe@1 = gleam_json_ffi:json_to_iodata(_pipe),
        unicode:characters_to_binary(_pipe@1)
    end,
    wisp:json_response(Body, 200).

-file("src/router.gleam", 140).
-spec handle_radarr_raw(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_radarr_raw(_, Ctx) ->
    Url = <<(erlang:element(7, erlang:element(2, Ctx)))/binary,
        "/api/v3/queue?pageSize=10&includeMovie=true"/utf8>>,
    Headers = [{<<"X-Api-Key"/utf8>>, erlang:element(8, erlang:element(2, Ctx))},
        {<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
    Body = case api@http_client:get(Url, Headers) of
        {ok, Raw} ->
            Raw;

        {error, E} ->
            <<"Error: "/utf8, (api@http_client:error_message(E))/binary>>
    end,
    _pipe = wisp:response(200),
    _pipe@1 = fun gleam@http@response:set_header/3(
        _pipe,
        <<"Content-Type"/utf8>>,
        <<"application/json"/utf8>>
    ),
    wisp:set_body(_pipe@1, {text, Body}).

-file("src/router.gleam", 154).
-spec handle_qbit_raw(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_qbit_raw(_, Ctx) ->
    Auth_url = <<(erlang:element(9, erlang:element(2, Ctx)))/binary,
        "/api/v2/auth/login"/utf8>>,
    Auth_body = <<<<<<"username="/utf8,
                (erlang:element(10, erlang:element(2, Ctx)))/binary>>/binary,
            "&password="/utf8>>/binary,
        (erlang:element(11, erlang:element(2, Ctx)))/binary>>,
    Auth_headers = [{<<"Content-Type"/utf8>>,
            <<"application/x-www-form-urlencoded"/utf8>>}],
    Body = case api@http_client:post_with_cookie(
        Auth_url,
        Auth_body,
        Auth_headers
    ) of
        {ok, Auth_resp} ->
            case erlang:element(3, Auth_resp) of
                {some, Cookie} ->
                    Url = <<(erlang:element(9, erlang:element(2, Ctx)))/binary,
                        "/api/v2/torrents/info"/utf8>>,
                    case api@http_client:get_with_cookie(Url, [], Cookie) of
                        {ok, Raw} ->
                            Raw;

                        {error, E} ->
                            <<"Error fetching torrents: "/utf8,
                                (api@http_client:error_message(E))/binary>>
                    end;

                none ->
                    <<"Error: No session cookie returned"/utf8>>
            end;

        {error, E@1} ->
            <<"Error authenticating: "/utf8,
                (api@http_client:error_message(E@1))/binary>>
    end,
    _pipe = wisp:response(200),
    _pipe@1 = fun gleam@http@response:set_header/3(
        _pipe,
        <<"Content-Type"/utf8>>,
        <<"application/json"/utf8>>
    ),
    wisp:set_body(_pipe@1, {text, Body}).

-file("src/router.gleam", 181).
-spec debug_jellyseerr_error(api@jellyseerr:jellyseerr_error()) -> binary().
debug_jellyseerr_error(E) ->
    case E of
        {http_error, _} ->
            <<"HTTP error"/utf8>>;

        {parse_error, Msg} ->
            <<"Parse error: "/utf8, Msg/binary>>
    end.

-file("src/router.gleam", 188).
-spec debug_arr_error(api@arr:arr_error()) -> binary().
debug_arr_error(E) ->
    case E of
        {http_error, Http_err} ->
            <<"HTTP error: "/utf8,
                (api@http_client:error_message(Http_err))/binary>>;

        {parse_error, Msg} ->
            <<"Parse error: "/utf8, Msg/binary>>
    end.

-file("src/router.gleam", 197).
-spec debug_qbit_error(api@qbittorrent:q_bit_error()) -> binary().
debug_qbit_error(E) ->
    case E of
        {http_error, Http_err} ->
            <<"HTTP error: "/utf8,
                (api@http_client:error_message(Http_err))/binary>>;

        {parse_error, Msg} ->
            <<"Parse error: "/utf8, Msg/binary>>;

        {auth_error, Msg@1} ->
            <<"Auth error: "/utf8, Msg@1/binary>>
    end.

-file("src/router.gleam", 216).
-spec debug_torrent(models@request:torrent_info()) -> gleam@json:json().
debug_torrent(T) ->
    gleam@json:object(
        [{<<"hash"/utf8>>, gleam@json:string(erlang:element(2, T))},
            {<<"name"/utf8>>, gleam@json:string(erlang:element(3, T))},
            {<<"progress"/utf8>>, gleam@json:float(erlang:element(4, T))},
            {<<"dlspeed"/utf8>>, gleam@json:int(erlang:element(5, T))},
            {<<"eta"/utf8>>, gleam@json:int(erlang:element(6, T))},
            {<<"state"/utf8>>, gleam@json:string(erlang:element(7, T))}]
    ).

-file("src/router.gleam", 229).
-spec option_json(gleam@option:option(BKR), fun((BKR) -> gleam@json:json())) -> gleam@json:json().
option_json(Opt, Encoder) ->
    case Opt of
        {some, V} ->
            Encoder(V);

        none ->
            gleam@json:null()
    end.

-file("src/router.gleam", 205).
-spec debug_arr_item(models@request:arr_queue_item()) -> gleam@json:json().
debug_arr_item(Item) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:int(erlang:element(2, Item))},
            {<<"title"/utf8>>, gleam@json:string(erlang:element(3, Item))},
            {<<"status"/utf8>>, gleam@json:string(erlang:element(4, Item))},
            {<<"tmdb_id"/utf8>>,
                option_json(erlang:element(7, Item), fun gleam@json:int/1)},
            {<<"tvdb_id"/utf8>>,
                option_json(erlang:element(8, Item), fun gleam@json:int/1)},
            {<<"download_id"/utf8>>,
                option_json(erlang:element(9, Item), fun gleam@json:string/1)}]
    ).

-file("src/router.gleam", 69).
-spec handle_debug(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_debug(_, Ctx) ->
    Jellyseerr_result = api@jellyseerr:get_requests(
        erlang:element(3, erlang:element(2, Ctx)),
        erlang:element(4, erlang:element(2, Ctx))
    ),
    Sonarr_result = api@arr:get_sonarr_queue(
        erlang:element(5, erlang:element(2, Ctx)),
        erlang:element(6, erlang:element(2, Ctx))
    ),
    Radarr_result = api@arr:get_radarr_queue(
        erlang:element(7, erlang:element(2, Ctx)),
        erlang:element(8, erlang:element(2, Ctx))
    ),
    Qbit_result = api@qbittorrent:get_torrents_with_auth(
        erlang:element(9, erlang:element(2, Ctx)),
        erlang:element(10, erlang:element(2, Ctx)),
        erlang:element(11, erlang:element(2, Ctx))
    ),
    Jellyseerr_status = case Jellyseerr_result of
        {ok, Reqs} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"ok"/utf8>>)},
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Reqs))}]
            );

        {error, E} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
                    {<<"error"/utf8>>,
                        gleam@json:string(debug_jellyseerr_error(E))}]
            )
    end,
    Sonarr_status = case Sonarr_result of
        {ok, Items} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"ok"/utf8>>)},
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Items))},
                    {<<"items"/utf8>>,
                        gleam@json:array(Items, fun debug_arr_item/1)}]
            );

        {error, E@1} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
                    {<<"error"/utf8>>, gleam@json:string(debug_arr_error(E@1))}]
            )
    end,
    Radarr_status = case Radarr_result of
        {ok, Items@1} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"ok"/utf8>>)},
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Items@1))},
                    {<<"items"/utf8>>,
                        gleam@json:array(Items@1, fun debug_arr_item/1)}]
            );

        {error, E@2} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
                    {<<"error"/utf8>>, gleam@json:string(debug_arr_error(E@2))}]
            )
    end,
    Qbit_status = case Qbit_result of
        {ok, Torrents} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"ok"/utf8>>)},
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Torrents))},
                    {<<"torrents"/utf8>>,
                        gleam@json:array(Torrents, fun debug_torrent/1)}]
            );

        {error, E@3} ->
            gleam@json:object(
                [{<<"status"/utf8>>, gleam@json:string(<<"error"/utf8>>)},
                    {<<"error"/utf8>>, gleam@json:string(debug_qbit_error(E@3))}]
            )
    end,
    Body = begin
        _pipe = gleam@json:object(
            [{<<"jellyseerr"/utf8>>, Jellyseerr_status},
                {<<"sonarr"/utf8>>, Sonarr_status},
                {<<"radarr"/utf8>>, Radarr_status},
                {<<"qbittorrent"/utf8>>, Qbit_status}]
        ),
        _pipe@1 = gleam_json_ffi:json_to_iodata(_pipe),
        unicode:characters_to_binary(_pipe@1)
    end,
    wisp:json_response(Body, 200).

-file("src/router.gleam", 250).
-spec serve_index(binary()) -> gleam@http@response:response(wisp:body()).
serve_index(Static_dir) ->
    Path = <<Static_dir/binary, "/index.html"/utf8>>,
    case simplifile:read(Path) of
        {ok, Content} ->
            _pipe = wisp:response(200),
            _pipe@1 = fun gleam@http@response:set_header/3(
                _pipe,
                <<"Content-Type"/utf8>>,
                <<"text/html"/utf8>>
            ),
            wisp:set_body(_pipe@1, {text, Content});

        {error, _} ->
            wisp:not_found()
    end.

-file("src/router.gleam", 281).
-spec string_join(list(binary()), binary()) -> binary().
string_join(Parts, Sep) ->
    gleam@string:join(Parts, Sep).

-file("src/router.gleam", 285).
-spec get_extension(binary()) -> binary().
get_extension(Path) ->
    case begin
        _pipe = gleam@string:split(Path, <<"."/utf8>>),
        gleam@list:last(_pipe)
    end of
        {ok, Ext} ->
            string:lowercase(Ext);

        {error, _} ->
            <<""/utf8>>
    end.

-file("src/router.gleam", 263).
-spec guess_content_type(binary()) -> binary().
guess_content_type(Path) ->
    case get_extension(Path) of
        <<"html"/utf8>> ->
            <<"text/html"/utf8>>;

        <<"css"/utf8>> ->
            <<"text/css"/utf8>>;

        <<"js"/utf8>> ->
            <<"application/javascript"/utf8>>;

        <<"mjs"/utf8>> ->
            <<"application/javascript"/utf8>>;

        <<"json"/utf8>> ->
            <<"application/json"/utf8>>;

        <<"png"/utf8>> ->
            <<"image/png"/utf8>>;

        <<"jpg"/utf8>> ->
            <<"image/jpeg"/utf8>>;

        <<"jpeg"/utf8>> ->
            <<"image/jpeg"/utf8>>;

        <<"svg"/utf8>> ->
            <<"image/svg+xml"/utf8>>;

        <<"ico"/utf8>> ->
            <<"image/x-icon"/utf8>>;

        _ ->
            <<"application/octet-stream"/utf8>>
    end.

-file("src/router.gleam", 236).
-spec serve_static(list(binary()), binary()) -> gleam@http@response:response(wisp:body()).
serve_static(Path_segments, Static_dir) ->
    Path = <<<<Static_dir/binary, "/"/utf8>>/binary,
        (string_join(Path_segments, <<"/"/utf8>>))/binary>>,
    case simplifile:read(Path) of
        {ok, Content} ->
            Content_type = guess_content_type(Path),
            _pipe = wisp:response(200),
            _pipe@1 = fun gleam@http@response:set_header/3(
                _pipe,
                <<"Content-Type"/utf8>>,
                Content_type
            ),
            wisp:set_body(_pipe@1, {text, Content});

        {error, _} ->
            wisp:not_found()
    end.

-file("src/router.gleam", 19).
?DOC(" Main router handler\n").
-spec handle_request(
    gleam@http@request:request(wisp@internal:connection()),
    context()
) -> gleam@http@response:response(wisp:body()).
handle_request(Req, Ctx) ->
    cors_builder:wisp_middleware(
        Req,
        begin
            _pipe = cors_builder:new(),
            _pipe@1 = cors_builder:allow_origin(_pipe, <<"*"/utf8>>),
            _pipe@2 = cors_builder:allow_method(_pipe@1, get),
            cors_builder:allow_header(_pipe@2, <<"Content-Type"/utf8>>)
        end,
        fun(Req@1) -> case fun gleam@http@request:path_segments/1(Req@1) of
                [<<"api"/utf8>>, <<"requests"/utf8>>] ->
                    handle_requests(Req@1, Ctx);

                [<<"api"/utf8>>, <<"health"/utf8>>] ->
                    handle_health(Req@1);

                [<<"api"/utf8>>, <<"debug"/utf8>>] ->
                    handle_debug(Req@1, Ctx);

                [<<"api"/utf8>>, <<"debug"/utf8>>, <<"radarr-raw"/utf8>>] ->
                    handle_radarr_raw(Req@1, Ctx);

                [<<"api"/utf8>>, <<"debug"/utf8>>, <<"qbit-raw"/utf8>>] ->
                    handle_qbit_raw(Req@1, Ctx);

                [<<"static"/utf8>> | Rest] ->
                    serve_static(Rest, erlang:element(3, Ctx));

                [] ->
                    serve_index(erlang:element(3, Ctx));

                _ ->
                    serve_index(erlang:element(3, Ctx))
            end end
    ).
