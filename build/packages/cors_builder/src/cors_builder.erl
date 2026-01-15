-module(cors_builder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/cors_builder.gleam").
-export([new/0, allow_all_origins/1, allow_origin/2, expose_header/2, max_age/2, allow_credentials/1, allow_method/2, allow_header/2, set_cors/2, set_cors_multiple_origin/3, mist_middleware/3, wisp_middleware/3]).
-export_type([origin/0, cors/0]).

-if(?OTP_RELEASE >= 27).
-define(MODULEDOC(Str), -moduledoc(Str)).
-define(DOC(Str), -doc(Str)).
-else.
-define(MODULEDOC(Str), -compile([])).
-define(DOC(Str), -compile([])).
-endif.

?MODULEDOC(
    " `cors_builder` provides an easy way to build and inject your CORS configuration\n"
    " in your server. The package tries to remains as simplest as possible, while\n"
    " guaranteeing type-safety and correctness of the CORS configuration.\n"
    "\n"
    " ## Quickstart\n"
    "\n"
    " Import the `cors_builder` package, and configure your CORS. Finally, use the\n"
    " correct corresponding middleware for your server, and you're done!\n"
    "\n"
    " ```\n"
    " import cors_builder as cors\n"
    " import gleam/http\n"
    " import mist\n"
    " import wisp.{type Request, type Response}\n"
    "\n"
    " // Dummy example.\n"
    " fn cors() {\n"
    "   cors.new()\n"
    "   |> cors.allow_origin(\"http://localhost:3000\")\n"
    "   |> cors.allow_origin(\"http://localhost:4000\")\n"
    "   |> cors.allow_method(http.Get)\n"
    "   |> cors.allow_method(http.Post)\n"
    " }\n"
    "\n"
    " fn handler(req: Request) -> Response {\n"
    "   use req <- cors.wisp_middleware(req, cors())\n"
    "   wisp.ok()\n"
    " }\n"
    "\n"
    " fn main() {\n"
    "   handler\n"
    "   |> wisp.mist_handler(secret_key)\n"
    "   |> mist.new()\n"
    "   |> mist.port(3000)\n"
    "   |> mist.start_http()\n"
    " }\n"
    " ```\n"
    "\n"
    " ## Low-level functions\n"
    "\n"
    " If you're building your framework or you know what you're doing, you should\n"
    " take a look at [`set_cors`](#set_cors) and\n"
    " [`set_cors_multiple_origin`](#set_cors_multiple_origin). They allow to\n"
    " inject the CORS in your response, and it allows you to create your\n"
    " middleware to use with the bare CORS data.\n"
    "\n"
    " If you're not building your framework, you should _probably_ heads to [`wisp`](https://hexdocs.pm/wisp)\n"
    " to get you started. It's better to familiarize with the ecosystem before\n"
    " jumping right in your custom code.\n"
).

-opaque origin() :: wildcard | {origin, gleam@set:set(binary())}.

-opaque cors() :: {cors,
        gleam@option:option(origin()),
        gleam@set:set(binary()),
        gleam@option:option(integer()),
        gleam@option:option(boolean()),
        gleam@set:set(gleam@http:method()),
        gleam@set:set(binary())}.

-file("src/cors_builder.gleam", 93).
?DOC(
    " Creates an empty CORS object. It will not contains anything by default.\n"
    " If you're using it directly, no headers will be added to the response.\n"
).
-spec new() -> cors().
new() ->
    {cors, none, gleam@set:new(), none, none, gleam@set:new(), gleam@set:new()}.

-file("src/cors_builder.gleam", 110).
?DOC(
    " Allow all domains to access your server.\n"
    "\n"
    " Be extremely careful, you should not use this function in production!\n"
    " Allowing all origins can easily be a huge security flaw!\n"
    " Allow only the origins you need, and use this function only locally,\n"
    " in dev mode.\n"
).
-spec allow_all_origins(cors()) -> cors().
allow_all_origins(Cors) ->
    Allow_origin = {some, wildcard},
    {cors,
        Allow_origin,
        erlang:element(3, Cors),
        erlang:element(4, Cors),
        erlang:element(5, Cors),
        erlang:element(6, Cors),
        erlang:element(7, Cors)}.

-file("src/cors_builder.gleam", 115).
-spec invalid_uri(binary()) -> boolean().
invalid_uri(Origin) ->
    _pipe = gleam_stdlib:uri_parse(Origin),
    _pipe@1 = gleam@result:is_error(_pipe),
    cors_builder@internal@function:tap(
        _pipe@1,
        fun(Value) ->
            gleam@bool:guard(
                not Value,
                nil,
                fun() ->
                    gleam_stdlib:println(
                        <<<<"Your provided origin: \""/utf8, Origin/binary>>/binary,
                            "\" is not a valid URI."/utf8>>
                    )
                end
            )
        end
    ).

-file("src/cors_builder.gleam", 135).
?DOC(
    " Allow a specific domain to access your server. The domain must be a valid\n"
    " URI, conformant to RFC 3986. In case it's not conformant, a warning will be\n"
    " emitted, and Cors won't be changed.\n"
    " You can specify multiple domains to access your server. In this case, call\n"
    " the function multiple times on `Cors` data.\n"
    " ```\n"
    " fn cors() {\n"
    "   cors.new()\n"
    "   |> cors.allow_origin(\"domain\")\n"
    "   |> cors.allow_origin(\"domain2\")\n"
    " }\n"
).
-spec allow_origin(cors(), binary()) -> cors().
allow_origin(Cors, Origin) ->
    gleam@bool:guard(
        invalid_uri(Origin),
        Cors,
        fun() ->
            Allow_origin = case erlang:element(2, Cors) of
                {some, wildcard} ->
                    {some, wildcard};

                {some, {origin, Content}} ->
                    {some, {origin, gleam@set:insert(Content, Origin)}};

                none ->
                    {some, {origin, gleam@set:from_list([Origin])}}
            end,
            {cors,
                Allow_origin,
                erlang:element(3, Cors),
                erlang:element(4, Cors),
                erlang:element(5, Cors),
                erlang:element(6, Cors),
                erlang:element(7, Cors)}
        end
    ).

-file("src/cors_builder.gleam", 155).
?DOC(
    " Expose headers in the resulting request.\n"
    " You can specify multiple headers to access your server. In this case, call\n"
    " the function multiple times on `Cors` data.\n"
    " ```\n"
    " fn cors() {\n"
    "   cors.new()\n"
    "   |> cors.expose_header(\"content-type\")\n"
    "   |> cors.expose_header(\"vary\")\n"
    " }\n"
    " ```\n"
).
-spec expose_header(cors(), binary()) -> cors().
expose_header(Cors, Header) ->
    Expose_headers = gleam@set:insert(erlang:element(3, Cors), Header),
    {cors,
        erlang:element(2, Cors),
        Expose_headers,
        erlang:element(4, Cors),
        erlang:element(5, Cors),
        erlang:element(6, Cors),
        erlang:element(7, Cors)}.

-file("src/cors_builder.gleam", 164).
?DOC(
    " Set an amount of seconds during which CORS requests can be cached.\n"
    " When using `max_age`, the browser will issue one request `OPTIONS` at first,\n"
    " and will reuse the result of that request for the specified amount of time.\n"
    " Once the cache expired, a new `OPTIONS` request will be made.\n"
).
-spec max_age(cors(), integer()) -> cors().
max_age(Cors, Age) ->
    Max_age = {some, Age},
    {cors,
        erlang:element(2, Cors),
        erlang:element(3, Cors),
        Max_age,
        erlang:element(5, Cors),
        erlang:element(6, Cors),
        erlang:element(7, Cors)}.

-file("src/cors_builder.gleam", 182).
?DOC(
    " Allow credentials to be sent in the request. Credentials take form of\n"
    " username and password, stored in cookies most of the time.\n"
    "\n"
    " Be extremely careful with this header, and consider it with caution, mainly\n"
    " for legacy systems relying on cookies or for systems aware of the danger of\n"
    " cookies, because of [CSRF attacks](https://developer.mozilla.org/en-US/docs/Glossary/CSRF).\n"
    " You probably don't really need it if you use lustre or any modern framework\n"
    " you'll find in the gleam ecosystem!\n"
    "\n"
    " When you can, prefer using some modern system, like OAuth2 or rely on a\n"
    " framework doing the authentication for you. A simple and secured way to\n"
    " authenticate your users is to use the `authorization` header, with a `Bearer`\n"
    " token.\n"
).
-spec allow_credentials(cors()) -> cors().
allow_credentials(Cors) ->
    Allow_credentials = {some, true},
    {cors,
        erlang:element(2, Cors),
        erlang:element(3, Cors),
        erlang:element(4, Cors),
        Allow_credentials,
        erlang:element(6, Cors),
        erlang:element(7, Cors)}.

-file("src/cors_builder.gleam", 199).
?DOC(
    " Allow methods to be used in subsequent CORS requests.\n"
    " You can specify multiple allowed methods. In this case, call the function\n"
    " multiple times on `Cors` data.\n"
    " ```\n"
    " import gleam/http\n"
    "\n"
    " fn cors() {\n"
    "   cors.new()\n"
    "   |> cors.allow_method(http.Get)\n"
    "   |> cors.allow_method(http.Post)\n"
    " }\n"
    " ```\n"
).
-spec allow_method(cors(), gleam@http:method()) -> cors().
allow_method(Cors, Method) ->
    Allow_methods = gleam@set:insert(erlang:element(6, Cors), Method),
    {cors,
        erlang:element(2, Cors),
        erlang:element(3, Cors),
        erlang:element(4, Cors),
        erlang:element(5, Cors),
        Allow_methods,
        erlang:element(7, Cors)}.

-file("src/cors_builder.gleam", 214).
?DOC(
    " All header to be sent to the server.\n"
    " You can specify multiple headers to send to your server. In this case, call\n"
    " the function multiple times on `Cors` data.\n"
    " ```\n"
    " fn cors() {\n"
    "   cors.new()\n"
    "   |> cors.allow_header(\"content-type\")\n"
    "   |> cors.allow_header(\"origin\")\n"
    " }\n"
    " ```\n"
).
-spec allow_header(cors(), binary()) -> cors().
allow_header(Cors, Header) ->
    Allow_headers = gleam@set:insert(erlang:element(7, Cors), Header),
    {cors,
        erlang:element(2, Cors),
        erlang:element(3, Cors),
        erlang:element(4, Cors),
        erlang:element(5, Cors),
        erlang:element(6, Cors),
        Allow_headers}.

-file("src/cors_builder.gleam", 222).
-spec warn_if_origin_empty(binary()) -> nil.
warn_if_origin_empty(Origin) ->
    case Origin of
        <<""/utf8>> ->
            gleam_stdlib:println(
                <<"origin is empty, but you have multiple allowed domains in your CORS configuration. Are you sure you're calling set_cors_multiple_origin and not set_cors?"/utf8>>
            );

        _ ->
            nil
    end.

-file("src/cors_builder.gleam", 232).
-spec set_allowed_origin(cors(), binary()) -> fun((gleam@http@response:response(RTK)) -> gleam@http@response:response(RTK)).
set_allowed_origin(Cors, Origin) ->
    Hd = <<"access-control-allow-origin"/utf8>>,
    case erlang:element(2, Cors) of
        none ->
            fun cors_builder@internal@function:identity/1;

        {some, wildcard} ->
            fun(_capture) ->
                gleam@http@response:set_header(_capture, Hd, <<"*"/utf8>>)
            end;

        {some, {origin, Origins}} ->
            Origins@1 = gleam@set:to_list(Origins),
            case Origins@1 of
                [O] ->
                    fun(_capture@1) ->
                        gleam@http@response:set_header(_capture@1, Hd, O)
                    end;

                _ ->
                    warn_if_origin_empty(Origin),
                    Not_origin = not gleam@list:contains(Origins@1, Origin),
                    gleam@bool:guard(
                        Not_origin,
                        fun cors_builder@internal@function:identity/1,
                        fun() -> fun(Res) -> _pipe = Res,
                                _pipe@1 = gleam@http@response:set_header(
                                    _pipe,
                                    Hd,
                                    Origin
                                ),
                                gleam@http@response:set_header(
                                    _pipe@1,
                                    <<"vary"/utf8>>,
                                    <<"origin"/utf8>>
                                ) end end
                    )
            end
    end.

-file("src/cors_builder.gleam", 256).
-spec set_expose_headers(gleam@http@response:response(RPW), cors()) -> gleam@http@response:response(RPW).
set_expose_headers(Res, Cors) ->
    Hd = <<"access-control-expose-headers"/utf8>>,
    Ls = gleam@set:to_list(erlang:element(3, Cors)),
    gleam@bool:guard(gleam@list:is_empty(Ls), Res, fun() -> _pipe = Ls,
            _pipe@1 = gleam@string:join(_pipe, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@1) end).

-file("src/cors_builder.gleam", 265).
-spec set_max_age(gleam@http@response:response(RPZ), cors()) -> gleam@http@response:response(RPZ).
set_max_age(Res, Cors) ->
    Hd = <<"access-control-max-age"/utf8>>,
    _pipe = erlang:element(4, Cors),
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(A) ->
            gleam@http@response:set_header(Res, Hd, erlang:integer_to_binary(A))
        end
    ),
    gleam@option:unwrap(_pipe@1, Res).

-file("src/cors_builder.gleam", 272).
-spec set_allow_credentials(gleam@http@response:response(RQC), cors()) -> gleam@http@response:response(RQC).
set_allow_credentials(Res, Cors) ->
    Hd = <<"access-control-allow-credentials"/utf8>>,
    _pipe = erlang:element(5, Cors),
    _pipe@1 = gleam@option:map(
        _pipe,
        fun(_) -> gleam@http@response:set_header(Res, Hd, <<"true"/utf8>>) end
    ),
    gleam@option:unwrap(_pipe@1, Res).

-file("src/cors_builder.gleam", 279).
-spec method_to_string(gleam@http:method()) -> binary().
method_to_string(Method) ->
    case Method of
        get ->
            <<"GET"/utf8>>;

        post ->
            <<"POST"/utf8>>;

        head ->
            <<"HEAD"/utf8>>;

        put ->
            <<"PUT"/utf8>>;

        delete ->
            <<"DELETE"/utf8>>;

        trace ->
            <<"TRACE"/utf8>>;

        connect ->
            <<"CONNECT"/utf8>>;

        options ->
            <<"OPTIONS"/utf8>>;

        patch ->
            <<"PATCH"/utf8>>;

        {other, Content} ->
            Content
    end.

-file("src/cors_builder.gleam", 294).
-spec set_allow_methods(gleam@http@response:response(RQG), cors()) -> gleam@http@response:response(RQG).
set_allow_methods(Res, Cors) ->
    Hd = <<"access-control-allow-methods"/utf8>>,
    Methods = gleam@set:to_list(erlang:element(6, Cors)),
    gleam@bool:guard(
        gleam@list:is_empty(Methods),
        Res,
        fun() -> _pipe = Methods,
            _pipe@1 = gleam@list:map(_pipe, fun method_to_string/1),
            _pipe@2 = gleam@string:join(_pipe@1, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@2) end
    ).

-file("src/cors_builder.gleam", 304).
-spec set_allow_headers(gleam@http@response:response(RQJ), cors()) -> gleam@http@response:response(RQJ).
set_allow_headers(Res, Cors) ->
    Hd = <<"access-control-allow-headers"/utf8>>,
    Headers = gleam@set:to_list(erlang:element(7, Cors)),
    case gleam@list:is_empty(Headers) of
        true ->
            Res;

        false ->
            _pipe = Headers,
            _pipe@1 = gleam@string:join(_pipe, <<","/utf8>>),
            gleam@http@response:set_header(Res, Hd, _pipe@1)
    end.

-file("src/cors_builder.gleam", 316).
-spec set_response(
    gleam@http@response:response(RQM),
    cors(),
    gleam@option:option(binary())
) -> gleam@http@response:response(RQM).
set_response(Res, Cors, Origin) ->
    _pipe = Res,
    _pipe@1 = (set_allowed_origin(
        Cors,
        gleam@option:unwrap(Origin, <<""/utf8>>)
    ))(_pipe),
    _pipe@2 = set_expose_headers(_pipe@1, Cors),
    _pipe@3 = set_max_age(_pipe@2, Cors),
    _pipe@4 = set_allow_credentials(_pipe@3, Cors),
    _pipe@5 = set_allow_methods(_pipe@4, Cors),
    set_allow_headers(_pipe@5, Cors).

-file("src/cors_builder.gleam", 333).
?DOC(
    " Set CORS headers on a response. Should be used in your handler.\n"
    " In case you're using a framework, it probably already implements it.\n"
    " If you're using mist or wisp, use the corresponding provided middlewares,\n"
    " ([mist_middleware](#mist_middleware)) and ([wisp_middleware](#wisp_middleware)) and do not\n"
    " use this \"low-level\" function.\n"
).
-spec set_cors(gleam@http@response:response(RQQ), cors()) -> gleam@http@response:response(RQQ).
set_cors(Res, Cors) ->
    set_response(Res, Cors, none).

-file("src/cors_builder.gleam", 343).
?DOC(
    " Set CORS headers on a response. Should be used when you have multiple\n"
    " allowed domains. Should be used in your handler.\n"
    " In case you're using a framework, it probably already implements it.\n"
    " If you're using mist or wisp, use the corresponding provided middlewares,\n"
    " ([mist_middleware](#mist_middleware)) and ([wisp_middleware](#wisp_middleware)) and do not\n"
    " use this \"low-level\" function.\n"
).
-spec set_cors_multiple_origin(
    gleam@http@response:response(RQT),
    cors(),
    binary()
) -> gleam@http@response:response(RQT).
set_cors_multiple_origin(Res, Cors, Origin) ->
    set_response(Res, Cors, {some, Origin}).

-file("src/cors_builder.gleam", 351).
-spec find_origin(gleam@http@request:request(any())) -> gleam@option:option(binary()).
find_origin(Req) ->
    _pipe = erlang:element(3, Req),
    _pipe@1 = gleam@list:find(
        _pipe,
        fun(H) -> gleam@pair:first(H) =:= <<"origin"/utf8>> end
    ),
    _pipe@2 = gleam@result:map(_pipe@1, fun gleam@pair:second/1),
    gleam@option:from_result(_pipe@2).

-file("src/cors_builder.gleam", 358).
-spec middleware(
    RQZ,
    gleam@http@request:request(RRA),
    cors(),
    fun((gleam@http@request:request(RRA)) -> gleam@http@response:response(RQZ))
) -> gleam@http@response:response(RQZ).
middleware(Empty, Req, Cors, Handler) ->
    Res = case erlang:element(2, Req) of
        options ->
            gleam@http@response:set_body(gleam@http@response:new(204), Empty);

        _ ->
            Handler(Req)
    end,
    _pipe = Req,
    _pipe@1 = find_origin(_pipe),
    _pipe@2 = gleam@option:map(
        _pipe@1,
        fun(_capture) -> set_cors_multiple_origin(Res, Cors, _capture) end
    ),
    gleam@option:unwrap(_pipe@2, Res).

-file("src/cors_builder.gleam", 376).
?DOC(
    " Intercepts the request for mist and handles CORS directly without worrying\n"
    " about it. Provide your CORS configuration, and you're good to go!\n"
).
-spec mist_middleware(
    gleam@http@request:request(mist@internal@http:connection()),
    cors(),
    fun((gleam@http@request:request(mist@internal@http:connection())) -> gleam@http@response:response(mist:response_data()))
) -> gleam@http@response:response(mist:response_data()).
mist_middleware(Req, Cors, Handler) ->
    _pipe = gleam@bytes_tree:new(),
    _pipe@1 = {bytes, _pipe},
    middleware(_pipe@1, Req, Cors, Handler).

-file("src/cors_builder.gleam", 388).
?DOC(
    " Intercepts the request for wisp and handles CORS directly without worrying\n"
    " about it. Provide your CORS configuration and you're good to go!\n"
).
-spec wisp_middleware(
    gleam@http@request:request(wisp@internal:connection()),
    cors(),
    fun((gleam@http@request:request(wisp@internal:connection())) -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
wisp_middleware(Req, Cors, Handler) ->
    middleware({bytes, gleam_stdlib:wrap_list(<<""/utf8>>)}, Req, Cors, Handler).
