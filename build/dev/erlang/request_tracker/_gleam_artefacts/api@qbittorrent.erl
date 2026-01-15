-module(api@qbittorrent).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api/qbittorrent.gleam").
-export([get_torrents/1, get_torrents_with_auth/3]).
-export_type([q_bit_error/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type q_bit_error() :: {http_error, api@http_client:http_error()} |
    {parse_error, binary()} |
    {auth_error, binary()}.

-file("src/api/qbittorrent.gleam", 92).
?DOC(" Decode a number (int or float) as a float\n").
-spec number_as_float() -> gleam@dynamic@decode:decoder(float()).
number_as_float() ->
    gleam@dynamic@decode:one_of(
        {decoder, fun gleam@dynamic@decode:decode_float/1},
        [begin
                _pipe = {decoder, fun gleam@dynamic@decode:decode_int/1},
                gleam@dynamic@decode:map(_pipe, fun erlang:float/1)
            end]
    ).

-file("src/api/qbittorrent.gleam", 71).
-spec torrent_decoder() -> gleam@dynamic@decode:decoder(models@request:torrent_info()).
torrent_decoder() ->
    gleam@dynamic@decode:field(
        <<"hash"/utf8>>,
        {decoder, fun gleam@dynamic@decode:decode_string/1},
        fun(Hash) ->
            gleam@dynamic@decode:field(
                <<"name"/utf8>>,
                {decoder, fun gleam@dynamic@decode:decode_string/1},
                fun(Name) ->
                    gleam@dynamic@decode:field(
                        <<"progress"/utf8>>,
                        number_as_float(),
                        fun(Progress) ->
                            gleam@dynamic@decode:field(
                                <<"dlspeed"/utf8>>,
                                {decoder, fun gleam@dynamic@decode:decode_int/1},
                                fun(Dlspeed) ->
                                    gleam@dynamic@decode:field(
                                        <<"eta"/utf8>>,
                                        {decoder,
                                            fun gleam@dynamic@decode:decode_int/1},
                                        fun(Eta) ->
                                            gleam@dynamic@decode:field(
                                                <<"state"/utf8>>,
                                                {decoder,
                                                    fun gleam@dynamic@decode:decode_string/1},
                                                fun(State) ->
                                                    gleam@dynamic@decode:success(
                                                        {torrent_info,
                                                            Hash,
                                                            Name,
                                                            Progress,
                                                            Dlspeed,
                                                            Eta,
                                                            State}
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

-file("src/api/qbittorrent.gleam", 60).
-spec parse_torrents_response(binary()) -> {ok,
        list(models@request:torrent_info())} |
    {error, q_bit_error()}.
parse_torrents_response(Body) ->
    Decoder = gleam@dynamic@decode:list(torrent_decoder()),
    case gleam@json:parse(Body, Decoder) of
        {ok, Torrents} ->
            {ok, Torrents};

        {error, _} ->
            {error,
                {parse_error, <<"Failed to parse qBittorrent response"/utf8>>}}
    end.

-file("src/api/qbittorrent.gleam", 14).
?DOC(" Fetch all torrents from qBittorrent (no auth)\n").
-spec get_torrents(binary()) -> {ok, list(models@request:torrent_info())} |
    {error, q_bit_error()}.
get_torrents(Base_url) ->
    Url = <<Base_url/binary, "/api/v2/torrents/info"/utf8>>,
    Headers = [{<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
    case api@http_client:get(Url, Headers) of
        {ok, Body} ->
            parse_torrents_response(Body);

        {error, Err} ->
            {error, {http_error, Err}}
    end.

-file("src/api/qbittorrent.gleam", 25).
?DOC(" Get torrents with authentication (handles session cookie)\n").
-spec get_torrents_with_auth(binary(), binary(), binary()) -> {ok,
        list(models@request:torrent_info())} |
    {error, q_bit_error()}.
get_torrents_with_auth(Base_url, Username, Password) ->
    Auth_url = <<Base_url/binary, "/api/v2/auth/login"/utf8>>,
    Auth_body = <<<<<<"username="/utf8, Username/binary>>/binary,
            "&password="/utf8>>/binary,
        Password/binary>>,
    Auth_headers = [{<<"Content-Type"/utf8>>,
            <<"application/x-www-form-urlencoded"/utf8>>}],
    case api@http_client:post_with_cookie(Auth_url, Auth_body, Auth_headers) of
        {ok, Auth_resp} ->
            case erlang:element(3, Auth_resp) of
                {some, Cookie} ->
                    Url = <<Base_url/binary, "/api/v2/torrents/info"/utf8>>,
                    Headers = [{<<"Accept"/utf8>>, <<"application/json"/utf8>>}],
                    case api@http_client:get_with_cookie(Url, Headers, Cookie) of
                        {ok, Body} ->
                            parse_torrents_response(Body);

                        {error, Err} ->
                            {error, {http_error, Err}}
                    end;

                none ->
                    {error,
                        {auth_error,
                            <<"No session cookie returned from qBittorrent"/utf8>>}}
            end;

        {error, Err@1} ->
            {error, {http_error, Err@1}}
    end.
