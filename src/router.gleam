import api/aggregator
import api/arr
import api/http_client
import api/jellyseerr
import api/qbittorrent
import config.{type Config}
import cors_builder as cors
import gleam/http.{Get, Options}
import gleam/json
import gleam/list
import gleam/string
import gleam/string_tree
import models/request
import simplifile
import wisp.{type Request, type Response}

pub type Context {
  Context(config: Config, static_dir: String)
}

/// Main router handler
pub fn handle_request(req: Request, ctx: Context) -> Response {
  use req <- cors.wisp_middleware(
    req,
    cors.new()
      |> cors.allow_origin("*")
      |> cors.allow_method(Get)
      |> cors.allow_header("Content-Type"),
  )

  case wisp.path_segments(req) {
    // API routes
    ["api", "requests"] -> handle_requests(req, ctx)
    ["api", "health"] -> handle_health(req)
    ["api", "debug"] -> handle_debug(req, ctx)

    // Static files
    ["static", ..rest] -> serve_static(rest, ctx.static_dir)

    // Serve index.html for SPA routes (catch-all for SPA routing)
    _ -> serve_index(ctx.static_dir)
  }
}

fn handle_requests(req: Request, ctx: Context) -> Response {
  case req.method {
    Get -> {
      let requests = aggregator.get_all_requests(ctx.config)
      let body =
        request.media_requests_to_json(requests)
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

  let body =
    json.object([
      #("jellyseerr", jellyseerr_status),
      #("sonarr", sonarr_status),
      #("radarr", radarr_status),
      #("qbittorrent", qbit_status),
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

fn debug_qbit_error(e: qbittorrent.QBitError) -> String {
  case e {
    qbittorrent.HttpError(http_err) -> "HTTP error: " <> http_client.error_message(http_err)
    qbittorrent.ParseError(msg) -> "Parse error: " <> msg
    qbittorrent.AuthError(msg) -> "Auth error: " <> msg
  }
}

fn serve_static(path_segments: List(String), static_dir: String) -> Response {
  let path = static_dir <> "/" <> string.join(path_segments, "/")

  case simplifile.read(path) {
    Ok(content) -> {
      let content_type = guess_content_type(path)
      wisp.response(200)
      |> wisp.set_header("Content-Type", content_type)
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
