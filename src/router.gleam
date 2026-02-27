import api/aggregator
import api/arr
import api/http_client
import api/jellyseerr
import api/qbittorrent
import api/sabnzbd
import config.{type Config}
import cors_builder as cors
import gleam/bit_array
import gleam/crypto
import gleam/erlang/process
import gleam/http.{Get, Options}
import gleam/http/request as http_request
import gleam/int
import gleam/json
import gleam/list
import gleam/option
import gleam/string
import gleam/string_tree
import models/overview
import models/request
import simplifile
import wisp.{type Request, type Response}

pub type Context {
  Context(config: Config, static_dir: String)
}

pub fn handle_request(req: Request, ctx: Context) -> Response {
  let cors_config = case ctx.config.cors_origin {
    "" ->
      cors.new()
      |> cors.allow_origin("*")
    origin ->
      cors.new()
      |> cors.allow_origin(origin)
  }
  let cors_config =
    cors_config
    |> cors.allow_method(Get)
    |> cors.allow_header("Content-Type")
    |> cors.allow_header("X-Api-Key")

  use req <- cors.wisp_middleware(req, cors_config)

  case wisp.path_segments(req) {
    ["api", "requests"] -> with_auth(req, ctx, handle_requests)
    ["api", "overview"] -> with_auth(req, ctx, handle_overview)
    ["api", "health"] -> handle_health(req)
    ["api", "debug"] -> with_auth(req, ctx, handle_debug)
    ["static", ..rest] -> serve_static(rest, ctx.static_dir)
    _ -> serve_index(ctx.static_dir)
  }
}

fn with_auth(
  req: Request,
  ctx: Context,
  handler: fn(Request, Context) -> Response,
) -> Response {
  let provided = http_request.get_header(req, "x-api-key")
  case provided {
    Ok(value) if value != "" -> {
      let matches =
        crypto.secure_compare(
          bit_array.from_string(value),
          bit_array.from_string(ctx.config.api_key),
        )
      case matches {
        True -> handler(req, ctx)
        False -> reject_auth()
      }
    }
    _ -> reject_auth()
  }
}

fn reject_auth() -> Response {
  // Delay failed auth responses to slow down brute force attempts
  process.sleep(1000)
  let body =
    json.object([#("error", json.string("Unauthorized"))])
    |> json.to_string_tree
    |> string_tree.to_string
  wisp.json_response(body, 401)
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
        |> json.to_string_tree
        |> string_tree.to_string

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
        |> json.to_string_tree
        |> string_tree.to_string

      wisp.json_response(body, 200)
    }
    Options -> wisp.ok()
    _ -> wisp.method_not_allowed([Get])
  }
}

fn handle_health(_req: Request) -> Response {
  let body =
    json.object([#("status", json.string("ok"))])
    |> json.to_string_tree
    |> string_tree.to_string
  wisp.json_response(body, 200)
}

fn handle_debug(_req: Request, ctx: Context) -> Response {
  let jellyseerr_result = jellyseerr.get_requests(ctx.config.jellyseerr_url, ctx.config.jellyseerr_api_key)
  let sonarr_result = arr.get_sonarr_queue(ctx.config.sonarr_url, ctx.config.sonarr_api_key)
  let radarr_result = arr.get_radarr_queue(ctx.config.radarr_url, ctx.config.radarr_api_key)
  let qbit_result = qbittorrent.get_torrents_with_auth(
    ctx.config.qbittorrent_url,
    ctx.config.qbittorrent_username,
    ctx.config.qbittorrent_password,
  )

  let jellyseerr_status = case jellyseerr_result {
    Ok(reqs) -> json.object([
      #("status", json.string("ok")),
      #("count", json.int(list.length(reqs))),
      #("requests", json.array(reqs, fn(r: request.JellyseerrRequest) {
        json.object([
          #("id", json.int(r.id)),
          #("status", json.int(r.status)),
          #("type", json.string(r.media_type)),
        ])
      })),
    ])
    Error(e) -> json.object([
      #("status", json.string("error")),
      #("error", json.string(debug_jellyseerr_error(e))),
    ])
  }

  let sonarr_status = format_arr_status(sonarr_result)
  let radarr_status = format_arr_status(radarr_result)

  let qbit_status = case qbit_result {
    Ok(torrents) -> json.object([
      #("status", json.string("ok")),
      #("count", json.int(list.length(torrents))),
      #("torrents", json.array(torrents, fn(t: request.TorrentInfo) {
        json.object([
          #("name", json.string(t.name)),
          #("hash", json.string(t.hash)),
          #("progress", json.float(t.progress)),
          #("state", json.string(t.state)),
        ])
      })),
    ])
    Error(e) -> json.object([
      #("status", json.string("error")),
      #("error", json.string(debug_qbit_error(e))),
    ])
  }

  let sabnzbd_status = case ctx.config.sabnzbd_api_key {
    "" -> json.object([
      #("status", json.string("disabled")),
    ])
    api_key -> {
      let sab_result = sabnzbd.get_downloads(ctx.config.sabnzbd_url, api_key)
      case sab_result {
        Ok(downloads) -> json.object([
          #("status", json.string("ok")),
          #("count", json.int(list.length(downloads))),
          #("downloads", json.array(downloads, fn(t: request.TorrentInfo) {
            json.object([
              #("name", json.string(t.name)),
              #("nzo_id", json.string(t.hash)),
              #("progress", json.float(t.progress)),
              #("state", json.string(t.state)),
            ])
          })),
        ])
        Error(e) -> json.object([
          #("status", json.string("error")),
          #("error", json.string(debug_sabnzbd_error(e))),
        ])
      }
    }
  }

  let body =
    json.object([
      #("jellyseerr", jellyseerr_status),
      #("sonarr", sonarr_status),
      #("radarr", radarr_status),
      #("qbittorrent", qbit_status),
      #("sabnzbd", sabnzbd_status),
    ])
    |> json.to_string_tree
    |> string_tree.to_string

  wisp.json_response(body, 200)
}

fn debug_jellyseerr_error(e: jellyseerr.JellyseerrError) -> String {
  case e {
    jellyseerr.HttpError(_) -> "HTTP error"
    jellyseerr.ParseError(msg) -> "Parse error: " <> msg
  }
}

fn debug_arr_error(e: arr.ArrError) -> String {
  case e {
    arr.HttpError(http_err) -> "HTTP error: " <> http_client.error_message(http_err)
    arr.ParseError(msg) -> "Parse error: " <> msg
  }
}

fn format_arr_status(
  result: Result(List(request.ArrQueueItem), arr.ArrError),
) -> json.Json {
  case result {
    Ok(items) ->
      json.object([
        #("status", json.string("ok")),
        #("count", json.int(list.length(items))),
      ])
    Error(e) ->
      json.object([
        #("status", json.string("error")),
        #("error", json.string(debug_arr_error(e))),
      ])
  }
}

fn debug_sabnzbd_error(e: sabnzbd.SabnzbdError) -> String {
  case e {
    sabnzbd.HttpError(http_err) -> "HTTP error: " <> http_client.error_message(http_err)
    sabnzbd.ParseError(msg) -> "Parse error: " <> msg
  }
}

fn debug_qbit_error(e: qbittorrent.QBitError) -> String {
  case e {
    qbittorrent.HttpError(http_err) -> "HTTP error: " <> http_client.error_message(http_err)
    qbittorrent.ParseError(msg) -> "Parse error: " <> msg
    qbittorrent.AuthError(msg) -> "Auth error: " <> msg
  }
}

fn serve_static(path_segments: List(String), static_dir: String) -> Response {
  let clean_segments =
    path_segments
    |> list.map(fn(s) {
      case string.split(s, "?") {
        [name, ..] -> name
        _ -> s
      }
    })
  let path = static_dir <> "/" <> string.join(clean_segments, "/")

  case simplifile.read(path) {
    Ok(content) -> {
      let content_type = guess_content_type(path)
      wisp.response(200)
      |> wisp.set_header("Content-Type", content_type)
      |> wisp.set_header("Cache-Control", "public, max-age=3600")
      |> wisp.set_body(wisp.Text(content))
    }
    Error(_) -> wisp.not_found()
  }
}

fn serve_index(static_dir: String) -> Response {
  let path = static_dir <> "/index.html"

  case simplifile.read(path) {
    Ok(content) -> {
      wisp.response(200)
      |> wisp.set_header("Content-Type", "text/html")
      |> wisp.set_body(wisp.Text(content))
    }
    Error(_) -> wisp.not_found()
  }
}

fn guess_content_type(path: String) -> String {
  case get_extension(path) {
    "html" -> "text/html"
    "css" -> "text/css"
    "js" -> "application/javascript"
    "mjs" -> "application/javascript"
    "json" -> "application/json"
    "png" -> "image/png"
    "jpg" | "jpeg" -> "image/jpeg"
    "svg" -> "image/svg+xml"
    "ico" -> "image/x-icon"
    _ -> "application/octet-stream"
  }
}

fn get_extension(path: String) -> String {
  case string.split(path, ".") |> list.last {
    Ok(ext) -> string.lowercase(ext)
    Error(_) -> ""
  }
}
