import api/aggregator
import api/arr
import api/jellyseerr
import api/qbittorrent
import config.{type Config}
import cors_builder as cors
import gleam/http.{Get, Options}
import gleam/json
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
    ["api", "debug", "radarr-raw"] -> handle_radarr_raw(req, ctx)
    ["api", "debug", "qbit-raw"] -> handle_qbit_raw(req, ctx)

    // Static files
    ["static", ..rest] -> serve_static(rest, ctx.static_dir)

    // Serve index.html for SPA routes
    [] -> serve_index(ctx.static_dir)
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
  // Fetch raw data from each service
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
    ])
    Error(e) -> json.object([
      #("status", json.string("error")),
      #("error", json.string(debug_jellyseerr_error(e))),
    ])
  }

  let sonarr_status = case sonarr_result {
    Ok(items) -> json.object([
      #("status", json.string("ok")),
      #("count", json.int(list.length(items))),
      #("items", json.array(items, debug_arr_item)),
    ])
    Error(e) -> json.object([
      #("status", json.string("error")),
      #("error", json.string(debug_arr_error(e))),
    ])
  }

  let radarr_status = case radarr_result {
    Ok(items) -> json.object([
      #("status", json.string("ok")),
      #("count", json.int(list.length(items))),
      #("items", json.array(items, debug_arr_item)),
    ])
    Error(e) -> json.object([
      #("status", json.string("error")),
      #("error", json.string(debug_arr_error(e))),
    ])
  }

  let qbit_status = case qbit_result {
    Ok(torrents) -> json.object([
      #("status", json.string("ok")),
      #("count", json.int(list.length(torrents))),
      #("torrents", json.array(torrents, debug_torrent)),
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

fn handle_radarr_raw(_req: Request, ctx: Context) -> Response {
  let url = ctx.config.radarr_url <> "/api/v3/queue?pageSize=10&includeMovie=true"
  let headers = [#("X-Api-Key", ctx.config.radarr_api_key), #("Accept", "application/json")]

  let body = case http_client.get(url, headers) {
    Ok(raw) -> raw
    Error(e) -> "Error: " <> http_client.error_message(e)
  }

  wisp.response(200)
  |> wisp.set_header("Content-Type", "application/json")
  |> wisp.set_body(wisp.Text(body))
}

fn handle_qbit_raw(_req: Request, ctx: Context) -> Response {
  // First authenticate
  let auth_url = ctx.config.qbittorrent_url <> "/api/v2/auth/login"
  let auth_body = "username=" <> ctx.config.qbittorrent_username <> "&password=" <> ctx.config.qbittorrent_password
  let auth_headers = [#("Content-Type", "application/x-www-form-urlencoded")]

  let body = case http_client.post_with_cookie(auth_url, auth_body, auth_headers) {
    Ok(auth_resp) -> {
      case auth_resp.cookie {
        option.Some(cookie) -> {
          let url = ctx.config.qbittorrent_url <> "/api/v2/torrents/info"
          case http_client.get_with_cookie(url, [], cookie) {
            Ok(raw) -> raw
            Error(e) -> "Error fetching torrents: " <> http_client.error_message(e)
          }
        }
        option.None -> "Error: No session cookie returned"
      }
    }
    Error(e) -> "Error authenticating: " <> http_client.error_message(e)
  }

  wisp.response(200)
  |> wisp.set_header("Content-Type", "application/json")
  |> wisp.set_body(wisp.Text(body))
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

import api/http_client

fn debug_qbit_error(e: qbittorrent.QBitError) -> String {
  case e {
    qbittorrent.HttpError(http_err) -> "HTTP error: " <> http_client.error_message(http_err)
    qbittorrent.ParseError(msg) -> "Parse error: " <> msg
    qbittorrent.AuthError(msg) -> "Auth error: " <> msg
  }
}

fn debug_arr_item(item: request.ArrQueueItem) -> json.Json {
  json.object([
    #("id", json.int(item.id)),
    #("title", json.string(item.title)),
    #("status", json.string(item.status)),
    #("tmdb_id", option_json(item.tmdb_id, json.int)),
    #("tvdb_id", option_json(item.tvdb_id, json.int)),
    #("download_id", option_json(item.download_id, json.string)),
  ])
}

fn debug_torrent(t: request.TorrentInfo) -> json.Json {
  json.object([
    #("hash", json.string(t.hash)),
    #("name", json.string(t.name)),
    #("progress", json.float(t.progress)),
    #("dlspeed", json.int(t.dlspeed)),
    #("eta", json.int(t.eta)),
    #("state", json.string(t.state)),
  ])
}

import gleam/option

fn option_json(opt: option.Option(a), encoder: fn(a) -> json.Json) -> json.Json {
  case opt {
    option.Some(v) -> encoder(v)
    option.None -> json.null()
  }
}

fn serve_static(path_segments: List(String), static_dir: String) -> Response {
  let path = static_dir <> "/" <> string_join(path_segments, "/")

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

import gleam/string
import gleam/list

fn string_join(parts: List(String), sep: String) -> String {
  string.join(parts, sep)
}

fn get_extension(path: String) -> String {
  case string.split(path, ".") |> list.last {
    Ok(ext) -> string.lowercase(ext)
    Error(_) -> ""
  }
}
