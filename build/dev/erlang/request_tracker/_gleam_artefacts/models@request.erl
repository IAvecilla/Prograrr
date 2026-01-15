-module(models@request).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch, inline]).
-define(FILEPATH, "src/models/request.gleam").
-export([media_request_to_json/1, media_requests_to_json/1, jellyseerr_status_to_request_status/1, torrent_state_to_download_status/1]).
-export_type([media_type/0, request_status/0, download_status/0, media_request/0, jellyseerr_request/0, jellyseerr_media/0, arr_queue_item/0, torrent_info/0]).

-type media_type() :: movie | tv_show.

-type request_status() :: pending | approved | available | processing | unknown.

-type download_status() :: downloading |
    seeding |
    paused |
    queued |
    stalled |
    completed |
    not_found.

-type media_request() :: {media_request,
        integer(),
        media_type(),
        binary(),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        request_status(),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(download_status()),
        gleam@option:option(float()),
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(binary()),
        gleam@option:option(integer()),
        gleam@option:option(integer())}.

-type jellyseerr_request() :: {jellyseerr_request,
        integer(),
        binary(),
        integer(),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(jellyseerr_media())}.

-type jellyseerr_media() :: {jellyseerr_media,
        integer(),
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary()),
        gleam@option:option(binary())}.

-type arr_queue_item() :: {arr_queue_item,
        integer(),
        binary(),
        binary(),
        integer(),
        integer(),
        gleam@option:option(integer()),
        gleam@option:option(integer()),
        gleam@option:option(binary())}.

-type torrent_info() :: {torrent_info,
        binary(),
        binary(),
        float(),
        integer(),
        integer(),
        binary()}.

-file("src/models/request.gleam", 133).
-spec media_type_to_json(media_type()) -> gleam@json:json().
media_type_to_json(Mt) ->
    case Mt of
        movie ->
            gleam@json:string(<<"movie"/utf8>>);

        tv_show ->
            gleam@json:string(<<"tv"/utf8>>)
    end.

-file("src/models/request.gleam", 140).
-spec request_status_to_json(request_status()) -> gleam@json:json().
request_status_to_json(Status) ->
    case Status of
        pending ->
            gleam@json:string(<<"pending"/utf8>>);

        approved ->
            gleam@json:string(<<"approved"/utf8>>);

        available ->
            gleam@json:string(<<"available"/utf8>>);

        processing ->
            gleam@json:string(<<"processing"/utf8>>);

        unknown ->
            gleam@json:string(<<"unknown"/utf8>>)
    end.

-file("src/models/request.gleam", 150).
-spec download_status_to_json(download_status()) -> gleam@json:json().
download_status_to_json(Status) ->
    case Status of
        downloading ->
            gleam@json:string(<<"downloading"/utf8>>);

        seeding ->
            gleam@json:string(<<"seeding"/utf8>>);

        paused ->
            gleam@json:string(<<"paused"/utf8>>);

        queued ->
            gleam@json:string(<<"queued"/utf8>>);

        stalled ->
            gleam@json:string(<<"stalled"/utf8>>);

        completed ->
            gleam@json:string(<<"completed"/utf8>>);

        not_found ->
            gleam@json:string(<<"not_found"/utf8>>)
    end.

-file("src/models/request.gleam", 162).
-spec option_to_json(gleam@option:option(BBE), fun((BBE) -> gleam@json:json())) -> gleam@json:json().
option_to_json(Opt, Encoder) ->
    case Opt of
        {some, Value} ->
            Encoder(Value);

        none ->
            gleam@json:null()
    end.

-file("src/models/request.gleam", 169).
-spec option_to_json_with(
    gleam@option:option(BBG),
    fun((BBG) -> gleam@json:json())
) -> gleam@json:json().
option_to_json_with(Opt, Encoder) ->
    case Opt of
        {some, Value} ->
            Encoder(Value);

        none ->
            gleam@json:null()
    end.

-file("src/models/request.gleam", 110).
-spec media_request_to_json(media_request()) -> gleam@json:json().
media_request_to_json(Request) ->
    gleam@json:object(
        [{<<"id"/utf8>>, gleam@json:int(erlang:element(2, Request))},
            {<<"mediaType"/utf8>>,
                media_type_to_json(erlang:element(3, Request))},
            {<<"title"/utf8>>, gleam@json:string(erlang:element(4, Request))},
            {<<"posterUrl"/utf8>>,
                option_to_json(
                    erlang:element(5, Request),
                    fun gleam@json:string/1
                )},
            {<<"year"/utf8>>,
                option_to_json(erlang:element(6, Request), fun gleam@json:int/1)},
            {<<"requestStatus"/utf8>>,
                request_status_to_json(erlang:element(7, Request))},
            {<<"requestedBy"/utf8>>,
                option_to_json(
                    erlang:element(8, Request),
                    fun gleam@json:string/1
                )},
            {<<"requestedAt"/utf8>>,
                option_to_json(
                    erlang:element(9, Request),
                    fun gleam@json:string/1
                )},
            {<<"downloadStatus"/utf8>>,
                option_to_json_with(
                    erlang:element(10, Request),
                    fun download_status_to_json/1
                )},
            {<<"downloadProgress"/utf8>>,
                option_to_json(
                    erlang:element(11, Request),
                    fun gleam@json:float/1
                )},
            {<<"downloadSpeed"/utf8>>,
                option_to_json(
                    erlang:element(12, Request),
                    fun gleam@json:int/1
                )},
            {<<"etaSeconds"/utf8>>,
                option_to_json(
                    erlang:element(13, Request),
                    fun gleam@json:int/1
                )},
            {<<"queuePosition"/utf8>>,
                option_to_json(
                    erlang:element(14, Request),
                    fun gleam@json:int/1
                )},
            {<<"queueStatus"/utf8>>,
                option_to_json(
                    erlang:element(15, Request),
                    fun gleam@json:string/1
                )}]
    ).

-file("src/models/request.gleam", 129).
-spec media_requests_to_json(list(media_request())) -> gleam@json:json().
media_requests_to_json(Requests) ->
    gleam@json:array(Requests, fun media_request_to_json/1).

-file("src/models/request.gleam", 178).
-spec jellyseerr_status_to_request_status(integer()) -> request_status().
jellyseerr_status_to_request_status(Status) ->
    case Status of
        1 ->
            pending;

        2 ->
            approved;

        3 ->
            available;

        4 ->
            processing;

        _ ->
            unknown
    end.

-file("src/models/request.gleam", 188).
-spec torrent_state_to_download_status(binary()) -> download_status().
torrent_state_to_download_status(State) ->
    case State of
        <<"downloading"/utf8>> ->
            downloading;

        <<"stalledDL"/utf8>> ->
            stalled;

        <<"pausedDL"/utf8>> ->
            paused;

        <<"queuedDL"/utf8>> ->
            queued;

        <<"uploading"/utf8>> ->
            seeding;

        <<"stalledUP"/utf8>> ->
            seeding;

        <<"pausedUP"/utf8>> ->
            seeding;

        <<"queuedUP"/utf8>> ->
            seeding;

        <<"checkingDL"/utf8>> ->
            queued;

        <<"checkingUP"/utf8>> ->
            completed;

        <<"forcedDL"/utf8>> ->
            downloading;

        <<"forcedUP"/utf8>> ->
            seeding;

        <<"allocating"/utf8>> ->
            queued;

        <<"metaDL"/utf8>> ->
            queued;

        _ ->
            not_found
    end.
