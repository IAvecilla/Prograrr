import api/http_client
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import models/request.{type TorrentInfo, TorrentInfo}

pub type QBitError {
  HttpError(http_client.HttpError)
  ParseError(String)
  AuthError(String)
}

/// Get torrents with authentication (handles session cookie)
pub fn get_torrents_with_auth(
  base_url: String,
  username: String,
  password: String,
) -> Result(List(TorrentInfo), QBitError) {
  // First, authenticate and get session cookie
  let auth_url = base_url <> "/api/v2/auth/login"
  let auth_body = "username=" <> username <> "&password=" <> password
  let auth_headers = [
    #("Content-Type", "application/x-www-form-urlencoded"),
  ]

  case http_client.post_with_cookie(auth_url, auth_body, auth_headers) {
    Ok(auth_resp) -> {
      case auth_resp.cookie {
        Some(cookie) -> {
          // Use the session cookie to fetch torrents
          let url = base_url <> "/api/v2/torrents/info"
          let headers = [#("Accept", "application/json")]

          case http_client.get_with_cookie(url, headers, cookie) {
            Ok(body) -> parse_torrents_response(body)
            Error(err) -> Error(HttpError(err))
          }
        }
        None -> {
          // No cookie returned, auth might have failed
          Error(AuthError("No session cookie returned from qBittorrent"))
        }
      }
    }
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_torrents_response(
  body: String,
) -> Result(List(TorrentInfo), QBitError) {
  let decoder = decode.list(torrent_decoder())

  case json.parse(body, decoder) {
    Ok(torrents) -> Ok(torrents)
    Error(_) -> Error(ParseError("Failed to parse qBittorrent response"))
  }
}

fn torrent_decoder() -> decode.Decoder(TorrentInfo) {
  use hash <- decode.field("hash", decode.string)
  use name <- decode.field("name", decode.string)
  use progress <- decode.field("progress", number_as_float())
  use dlspeed <- decode.field("dlspeed", decode.int)
  use eta <- decode.field("eta", decode.int)
  use state <- decode.field("state", decode.string)

  decode.success(TorrentInfo(
    hash: hash,
    name: name,
    progress: progress,
    dlspeed: dlspeed,
    eta: eta,
    state: state,
  ))
}

/// Decode a number (int or float) as a float
fn number_as_float() -> decode.Decoder(Float) {
  decode.one_of(decode.float, [
    decode.int |> decode.map(int.to_float),
  ])
}
