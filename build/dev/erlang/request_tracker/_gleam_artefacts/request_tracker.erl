-module(request_tracker).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/request_tracker.gleam").
-export([main/0]).

-file("src/request_tracker.gleam", 10).
-spec main() -> nil.
main() ->
    wisp:configure_logger(),
    case config:load() of
        {ok, Cfg} ->
            gleam_stdlib:println(
                <<"Starting Media Request Tracker on port "/utf8,
                    (erlang:integer_to_binary(erlang:element(2, Cfg)))/binary>>
            ),
            Ctx = {context, Cfg, <<"./static"/utf8>>},
            Handler = fun(_capture) -> router:handle_request(_capture, Ctx) end,
            case begin
                _pipe = wisp@wisp_mist:handler(
                    Handler,
                    <<"secret_key_change_me"/utf8>>
                ),
                _pipe@1 = mist:new(_pipe),
                _pipe@2 = mist:port(_pipe@1, erlang:element(2, Cfg)),
                _pipe@3 = mist:bind(_pipe@2, <<"0.0.0.0"/utf8>>),
                mist:start(_pipe@3)
            end of
                {ok, _} -> nil;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Pattern match failed, no pattern matched the value."/utf8>>,
                                file => <<?FILEPATH/utf8>>,
                                module => <<"request_tracker"/utf8>>,
                                function => <<"main"/utf8>>,
                                line => 23,
                                value => _assert_fail,
                                start => 487,
                                'end' => 669,
                                pattern_start => 498,
                                pattern_end => 503})
            end,
            gleam_stdlib:println(<<"Server started successfully!"/utf8>>),
            gleam_erlang_ffi:sleep_forever();

        {error, Err} ->
            gleam_stdlib:println(
                <<"Failed to load configuration: "/utf8, Err/binary>>
            ),
            gleam_stdlib:println(<<""/utf8>>),
            gleam_stdlib:println(<<"Required environment variables:"/utf8>>),
            gleam_stdlib:println(
                <<"  JELLYSEERR_API_KEY - Jellyseerr API key"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  SONARR_API_KEY     - Sonarr API key"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  RADARR_API_KEY     - Radarr API key"/utf8>>
            ),
            gleam_stdlib:println(<<""/utf8>>),
            gleam_stdlib:println(<<"Optional environment variables:"/utf8>>),
            gleam_stdlib:println(
                <<"  PORT               - Server port (default: 3000)"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  JELLYSEERR_URL     - Jellyseerr URL (default: http://localhost:5055)"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  SONARR_URL         - Sonarr URL (default: http://localhost:8989)"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  RADARR_URL         - Radarr URL (default: http://localhost:7878)"/utf8>>
            ),
            gleam_stdlib:println(
                <<"  QBITTORRENT_URL    - qBittorrent URL (default: http://localhost:8080)"/utf8>>
            )
    end.
