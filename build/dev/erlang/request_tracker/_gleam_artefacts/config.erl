-module(config).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/config.gleam").
-export([load/0]).
-export_type([config/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type config() :: {config,
        integer(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary(),
        binary()}.

-file("src/config.gleam", 48).
-spec get_env(binary(), binary()) -> {ok, binary()} | {error, binary()}.
get_env(Key, Default) ->
    {ok, gleam@result:unwrap(envoy_ffi:get(Key), Default)}.

-file("src/config.gleam", 52).
-spec get_env_required(binary()) -> {ok, binary()} | {error, binary()}.
get_env_required(Key) ->
    case envoy_ffi:get(Key) of
        {ok, Value} ->
            {ok, Value};

        {error, _} ->
            {error,
                <<"Missing required environment variable: "/utf8, Key/binary>>}
    end.

-file("src/config.gleam", 59).
-spec get_env_int(binary(), integer()) -> {ok, integer()} | {error, binary()}.
get_env_int(Key, Default) ->
    case envoy_ffi:get(Key) of
        {ok, Value} ->
            case gleam_stdlib:parse_int(Value) of
                {ok, N} ->
                    {ok, N};

                {error, _} ->
                    {error,
                        <<<<<<"Invalid integer for "/utf8, Key/binary>>/binary,
                                ": "/utf8>>/binary,
                            Value/binary>>}
            end;

        {error, _} ->
            {ok, Default}
    end.

-file("src/config.gleam", 22).
?DOC(" Load configuration from environment variables\n").
-spec load() -> {ok, config()} | {error, binary()}.
load() ->
    gleam@result:'try'(
        get_env_int(<<"PORT"/utf8>>, 3000),
        fun(Port) ->
            gleam@result:'try'(
                get_env(
                    <<"JELLYSEERR_URL"/utf8>>,
                    <<"http://localhost:5055"/utf8>>
                ),
                fun(Jellyseerr_url) ->
                    gleam@result:'try'(
                        get_env_required(<<"JELLYSEERR_API_KEY"/utf8>>),
                        fun(Jellyseerr_api_key) ->
                            gleam@result:'try'(
                                get_env(
                                    <<"SONARR_URL"/utf8>>,
                                    <<"http://localhost:8989"/utf8>>
                                ),
                                fun(Sonarr_url) ->
                                    gleam@result:'try'(
                                        get_env_required(
                                            <<"SONARR_API_KEY"/utf8>>
                                        ),
                                        fun(Sonarr_api_key) ->
                                            gleam@result:'try'(
                                                get_env(
                                                    <<"RADARR_URL"/utf8>>,
                                                    <<"http://localhost:7878"/utf8>>
                                                ),
                                                fun(Radarr_url) ->
                                                    gleam@result:'try'(
                                                        get_env_required(
                                                            <<"RADARR_API_KEY"/utf8>>
                                                        ),
                                                        fun(Radarr_api_key) ->
                                                            gleam@result:'try'(
                                                                get_env(
                                                                    <<"QBITTORRENT_URL"/utf8>>,
                                                                    <<"http://localhost:8080"/utf8>>
                                                                ),
                                                                fun(
                                                                    Qbittorrent_url
                                                                ) ->
                                                                    gleam@result:'try'(
                                                                        get_env(
                                                                            <<"QBITTORRENT_USERNAME"/utf8>>,
                                                                            <<"admin"/utf8>>
                                                                        ),
                                                                        fun(
                                                                            Qbittorrent_username
                                                                        ) ->
                                                                            gleam@result:'try'(
                                                                                get_env(
                                                                                    <<"QBITTORRENT_PASSWORD"/utf8>>,
                                                                                    <<"adminadmin"/utf8>>
                                                                                ),
                                                                                fun(
                                                                                    Qbittorrent_password
                                                                                ) ->
                                                                                    {ok,
                                                                                        {config,
                                                                                            Port,
                                                                                            Jellyseerr_url,
                                                                                            Jellyseerr_api_key,
                                                                                            Sonarr_url,
                                                                                            Sonarr_api_key,
                                                                                            Radarr_url,
                                                                                            Radarr_api_key,
                                                                                            Qbittorrent_url,
                                                                                            Qbittorrent_username,
                                                                                            Qbittorrent_password}}
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
                    )
                end
            )
        end
    ).
