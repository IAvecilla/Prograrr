import api/aggregator
import api/http_client.{type ServiceError}
import session
import config.{type Config}
import cors_builder as cors
import gleam/dynamic/decode
import gleam/http.{Get, Options, Post}
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import models/overview
import models/request
import simplifile
import wisp.{type Request, type Response}

pub type Context {
  Context(config: Config, static_dir: String)
}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  let cors_config = case ctx.config.cors_origin {
    option.None -> cors.new()
    option.Some(origin) ->
      cors.new()
      |> cors.allow_origin(origin)
  }
  let cors_config =
    cors_config
    |> cors.allow_method(Get)
    |> cors.allow_header("Content-Type")
    |> cors.allow_header("X-Api-Key")

  use req <- cors.wisp_middleware(req, cors_config)
  use <- wisp.serve_static(req, under: "/static", from: ctx.static_dir)

  case wisp.path_segments(req) {
    ["api", "requests"] -> with_auth(req, ctx, handle_requests)
    ["api", "overview"] -> with_auth(req, ctx, handle_overview)
    ["api", "health"] -> handle_health(req)
    ["api", "session"] -> handle_session(req, ctx)
    ["api", "login"] -> handle_login(req, ctx)
    ["api", "logout"] -> handle_logout(req)
    ["api", "debug"] -> with_auth(req, ctx, handle_debug)
    _ -> serve_index(ctx.static_dir)
  }
}

fn with_auth(
  req: Request,
  ctx: Context,
  handler: fn(Request, Context) -> Response,
) -> Response {
  case session.is_authorized(req, ctx.config.api_key) {
    True -> handler(req, ctx)
    False -> session.unauthorized()
  }
}

/// Lets the frontend decide whether to show the login form
fn handle_session(req: Request, ctx: Context) -> Response {
  json.object([
    #("authRequired", json.bool(option.is_some(ctx.config.api_key))),
    #("authenticated", json.bool(session.is_authorized(req, ctx.config.api_key))),
  ])
  |> json.to_string
  |> wisp.json_response(200)
}

fn handle_login(req: Request, ctx: Context) -> Response {
  use <- wisp.require_method(req, Post)
  use body <- wisp.require_json(req)
  let decoder = {
    use key <- decode.field("apiKey", decode.string)
    decode.success(key)
  }
  case decode.run(body, decoder) {
    Ok(key) -> session.login(req, ctx.config.api_key, key)
    Error(_) -> wisp.bad_request("Expected {\"apiKey\": string}")
  }
}

fn handle_logout(req: Request) -> Response {
  use <- wisp.require_method(req, Post)
  session.logout(req)
}

fn handle_requests(req: Request, ctx: Context) -> Response {
  case req.method {
    Get -> {
      let requests = aggregator.get_all_requests(ctx.config)

      let filtered = case wisp.get_query(req) |> list.find(fn(pair) { pair.0 == "tmdbId" }) {
        Ok(#(_, value)) -> {
          case int.parse(value) {
            Ok(tmdb_id) -> list.filter(requests, fn(r) { r.tmdb_id == option.Some(tmdb_id) })
            Error(_) -> requests
          }
        }
        Error(_) -> requests
      }

      let body =
        request.media_requests_to_json(filtered)
        |> json.to_string

      wisp.json_response(body, 200)
    }
    Options -> wisp.ok()
    _ -> wisp.method_not_allowed([Get])
  }
}

fn handle_overview(req: Request, ctx: Context) -> Response {
  case req.method {
    Get -> {
      let resp = aggregator.get_overview(ctx.config)
      let body =
        overview.overview_response_to_json(resp)
        |> json.to_string

      wisp.json_response(body, 200)
    }
    Options -> wisp.ok()
    _ -> wisp.method_not_allowed([Get])
  }
}

fn handle_health(_req: Request) -> Response {
  json.object([#("status", json.string("ok"))])
  |> json.to_string
  |> wisp.json_response(200)
}

/// Connection status for each service: ok + item count, error, or disabled
fn handle_debug(_req: Request, ctx: Context) -> Response {
  let sources = aggregator.fetch_sources(ctx.config)
  json.object([
    #("jellyseerr", service_status(sources.jellyseerr)),
    #("sonarr_queue", service_status(sources.sonarr_queue)),
    #("sonarr_series", service_status(sources.sonarr_series)),
    #("radarr_queue", service_status(sources.radarr_queue)),
    #("radarr_movies", service_status(sources.radarr_movies)),
    #("qbittorrent", optional_service_status(sources.qbittorrent)),
    #("sabnzbd", optional_service_status(sources.sabnzbd)),
  ])
  |> json.to_string
  |> wisp.json_response(200)
}

fn service_status(result: Result(List(a), ServiceError)) -> json.Json {
  case result {
    Ok(items) ->
      json.object([
        #("status", json.string("ok")),
        #("count", json.int(list.length(items))),
      ])
    Error(e) ->
      json.object([
        #("status", json.string("error")),
        #("error", json.string(http_client.error_message(e))),
      ])
  }
}

fn optional_service_status(
  result: option.Option(Result(List(a), ServiceError)),
) -> json.Json {
  case result {
    option.Some(result) -> service_status(result)
    option.None -> json.object([#("status", json.string("disabled"))])
  }
}

fn serve_index(static_dir: String) -> Response {
  case simplifile.read(static_dir <> "/index.html") {
    Ok(content) ->
      wisp.html_response(content, 200)
      |> wisp.set_header("cache-control", "no-store")
    Error(_) -> wisp.not_found()
  }
}
