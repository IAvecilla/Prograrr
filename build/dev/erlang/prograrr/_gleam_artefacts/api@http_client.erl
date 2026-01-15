-module(api@http_client).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/api/http_client.gleam").
-export([error_message/1, get/2, get_with_cookie/3, post_with_cookie/3]).
-export_type([http_error/0, http_response/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

-type http_error() :: {request_failed, binary()} | {invalid_response, binary()}.

-type http_response() :: {http_response,
        binary(),
        gleam@option:option(binary())}.

-file("src/api/http_client.gleam", 20).
?DOC(" Get the error message from an HttpError\n").
-spec error_message(http_error()) -> binary().
error_message(Err) ->
    case Err of
        {request_failed, Msg} ->
            Msg;

        {invalid_response, Msg@1} ->
            Msg@1
    end.

-file("src/api/http_client.gleam", 81).
-spec add_headers(
    gleam@http@request:request(binary()),
    list({binary(), binary()})
) -> gleam@http@request:request(binary()).
add_headers(Req, Headers) ->
    gleam@list:fold(
        Headers,
        Req,
        fun(R, Header) ->
            gleam@http@request:set_header(
                R,
                erlang:element(1, Header),
                erlang:element(2, Header)
            )
        end
    ).

-file("src/api/http_client.gleam", 28).
?DOC(" Make an HTTP GET request\n").
-spec get(binary(), list({binary(), binary()})) -> {ok, binary()} |
    {error, http_error()}.
get(Url, Headers) ->
    case gleam@http@request:to(Url) of
        {ok, Req} ->
            Req_with_headers = add_headers(Req, Headers),
            case gleam@httpc:send(Req_with_headers) of
                {ok, Resp} ->
                    case erlang:element(2, Resp) of
                        200 ->
                            {ok, erlang:element(4, Resp)};

                        Status ->
                            {error,
                                {request_failed,
                                    <<<<<<"HTTP "/utf8,
                                                (erlang:integer_to_binary(
                                                    Status
                                                ))/binary>>/binary,
                                            " from "/utf8>>/binary,
                                        Url/binary>>}}
                    end;

                {error, _} ->
                    {error,
                        {request_failed,
                            <<"HTTP request failed to "/utf8, Url/binary>>}}
            end;

        {error, _} ->
            {error, {invalid_response, <<"Invalid URL: "/utf8, Url/binary>>}}
    end.

-file("src/api/http_client.gleam", 48).
?DOC(" Make an HTTP GET request with a cookie\n").
-spec get_with_cookie(binary(), list({binary(), binary()}), binary()) -> {ok,
        binary()} |
    {error, http_error()}.
get_with_cookie(Url, Headers, Cookie) ->
    Headers_with_cookie = [{<<"Cookie"/utf8>>, Cookie} | Headers],
    get(Url, Headers_with_cookie).

-file("src/api/http_client.gleam", 87).
-spec extract_cookie(gleam@http@response:response(binary())) -> gleam@option:option(binary()).
extract_cookie(Resp) ->
    _pipe = erlang:element(3, Resp),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(H) ->
            string:lowercase(erlang:element(1, H)) =:= <<"set-cookie"/utf8>>
        end
    ),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    gleam@option:map(_pipe@2, fun(H@1) -> erlang:element(2, H@1) end).

-file("src/api/http_client.gleam", 54).
?DOC(" Make an HTTP POST request and return body + set-cookie header\n").
-spec post_with_cookie(binary(), binary(), list({binary(), binary()})) -> {ok,
        http_response()} |
    {error, http_error()}.
post_with_cookie(Url, Body, Headers) ->
    case gleam@http@request:to(Url) of
        {ok, Req} ->
            Req_with_body = begin
                _pipe = Req,
                _pipe@1 = gleam@http@request:set_method(_pipe, post),
                gleam@http@request:set_body(_pipe@1, Body)
            end,
            Req_with_headers = add_headers(Req_with_body, Headers),
            case gleam@httpc:send(Req_with_headers) of
                {ok, Resp} ->
                    case (erlang:element(2, Resp) >= 200) andalso (erlang:element(
                        2,
                        Resp
                    )
                    < 300) of
                        true ->
                            Cookie = extract_cookie(Resp),
                            {ok,
                                {http_response, erlang:element(4, Resp), Cookie}};

                        false ->
                            {error,
                                {request_failed,
                                    <<<<<<"HTTP "/utf8,
                                                (erlang:integer_to_binary(
                                                    erlang:element(2, Resp)
                                                ))/binary>>/binary,
                                            " from "/utf8>>/binary,
                                        Url/binary>>}}
                    end;

                {error, _} ->
                    {error,
                        {request_failed,
                            <<"HTTP request failed to "/utf8, Url/binary>>}}
            end;

        {error, _} ->
            {error, {invalid_response, <<"Invalid URL: "/utf8, Url/binary>>}}
    end.
