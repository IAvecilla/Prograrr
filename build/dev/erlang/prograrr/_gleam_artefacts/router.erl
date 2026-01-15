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

-file("src/router.gleam", 151).
-spec debug_jellyseerr_error(api@jellyseerr:jellyseerr_error()) -> binary().
debug_jellyseerr_error(E) ->
    case E of
        {http_error, _} ->
            <<"HTTP error"/utf8>>;

        {parse_error, Msg} ->
            <<"Parse error: "/utf8, Msg/binary>>
    end.

-file("src/router.gleam", 158).
-spec debug_arr_error(api@arr:arr_error()) -> binary().
debug_arr_error(E) ->
    case E of
        {http_error, Http_err} ->
            <<"HTTP error: "/utf8,
                (api@http_client:error_message(Http_err))/binary>>;

        {parse_error, Msg} ->
            <<"Parse error: "/utf8, Msg/binary>>
    end.

-file("src/router.gleam", 165).
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
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Reqs))},
                    {<<"requests"/utf8>>,
                        gleam@json:array(
                            Reqs,
                            fun(R) ->
                                gleam@json:object(
                                    [{<<"id"/utf8>>,
                                            gleam@json:int(erlang:element(2, R))},
                                        {<<"status"/utf8>>,
                                            gleam@json:int(erlang:element(4, R))},
                                        {<<"type"/utf8>>,
                                            gleam@json:string(
                                                erlang:element(3, R)
                                            )}]
                                )
                            end
                        )}]
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
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Items))}]
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
                    {<<"count"/utf8>>, gleam@json:int(erlang:length(Items@1))}]
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
                        gleam@json:array(
                            Torrents,
                            fun(T) ->
                                gleam@json:object(
                                    [{<<"name"/utf8>>,
                                            gleam@json:string(
                                                erlang:element(3, T)
                                            )},
                                        {<<"hash"/utf8>>,
                                            gleam@json:string(
                                                erlang:element(2, T)
                                            )},
                                        {<<"progress"/utf8>>,
                                            gleam@json:float(
                                                erlang:element(4, T)
                                            )},
                                        {<<"state"/utf8>>,
                                            gleam@json:string(
                                                erlang:element(7, T)
                                            )}]
                                )
                            end
                        )}]
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

-file("src/router.gleam", 187).
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

-file("src/router.gleam", 215).
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

-file("src/router.gleam", 200).
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

-file("src/router.gleam", 173).
-spec serve_static(list(binary()), binary()) -> gleam@http@response:response(wisp:body()).
serve_static(Path_segments, Static_dir) ->
    Path = <<<<Static_dir/binary, "/"/utf8>>/binary,
        (gleam@string:join(Path_segments, <<"/"/utf8>>))/binary>>,
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

-file("src/router.gleam", 22).
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

                [<<"static"/utf8>> | Rest] ->
                    serve_static(Rest, erlang:element(3, Ctx));

                _ ->
                    serve_index(erlang:element(3, Ctx))
            end end
    ).
