import api/http_client.{type ServiceError, AuthError}
import config.{type QBittorrent}
import gleam/dynamic/decode
import gleam/int
import gleam/option.{None, Some}
import gleam/result
import gleam/uri
import models/request.{type Download, Download}

/// Log in and fetch all torrents (qBittorrent uses a session cookie)
pub fn get_downloads(qbit: QBittorrent) -> Result(List(Download), ServiceError) {
  let auth_body =
    "username="
    <> uri.percent_encode(qbit.username)
    <> "&password="
    <> uri.percent_encode(qbit.password)
  use auth <- result.try(http_client.post_with_cookie(
    qbit.url <> "/api/v2/auth/login",
    auth_body,
    [#("Content-Type", "application/x-www-form-urlencoded")],
  ))

  case auth.cookie {
    Some(cookie) ->
      http_client.get_json(
        qbit.url <> "/api/v2/torrents/info",
        [#("Cookie", cookie), #("Accept", "application/json")],
        decode.list(torrent_decoder()),
        "qBittorrent response",
      )
    None -> Error(AuthError("No session cookie returned from qBittorrent"))
  }
}

fn torrent_decoder() -> decode.Decoder(Download) {
  use hash <- decode.field("hash", decode.string)
  use name <- decode.field("name", decode.string)
  use progress <- decode.field("progress", number_as_float())
  use dlspeed <- decode.field("dlspeed", decode.int)
  use eta <- decode.field("eta", decode.int)
  use state <- decode.field("state", decode.string)

  decode.success(Download(
    id: hash,
    name: name,
    progress: progress,
    speed: dlspeed,
    eta_seconds: eta,
    status: request.torrent_state_to_download_status(state),
  ))
}

/// Decode a number (int or float) as a float
fn number_as_float() -> decode.Decoder(Float) {
  decode.one_of(decode.float, [
    decode.int |> decode.map(int.to_float),
  ])
}
