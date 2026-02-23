import api/http_client
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/json
import gleam/string
import models/request.{type TorrentInfo, TorrentInfo}

pub type SabnzbdError {
  HttpError(http_client.HttpError)
  ParseError(String)
}

/// Fetch current downloads from SABnzbd queue
pub fn get_downloads(
  base_url: String,
  api_key: String,
) -> Result(List(TorrentInfo), SabnzbdError) {
  let url =
    base_url
    <> "/api?apikey="
    <> api_key
    <> "&mode=queue&output=json"

  let headers = [#("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> parse_queue_response(body)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_queue_response(
  body: String,
) -> Result(List(TorrentInfo), SabnzbdError) {
  let decoder = {
    use slots <- decode.field("queue", {
      use slots <- decode.field("slots", decode.list(slot_decoder()))
      decode.success(slots)
    })
    decode.success(slots)
  }

  case json.parse(body, decoder) {
    Ok(slots) -> Ok(slots)
    Error(_) -> Error(ParseError("Failed to parse SABnzbd response"))
  }
}

fn slot_decoder() -> decode.Decoder(TorrentInfo) {
  use nzo_id <- decode.field("nzo_id", decode.string)
  use filename <- decode.field("filename", decode.string)
  use percentage <- decode.field("percentage", string_as_float())
  use mb <- decode.field("mb", string_as_float())
  use mbleft <- decode.field("mbleft", string_as_float())
  use timeleft <- decode.field("timeleft", decode.string)
  use status <- decode.field("status", decode.string)

  let progress = percentage /. 100.0

  // Estimate speed from remaining MB and time left
  let eta_seconds = parse_timeleft(timeleft)
  let dlspeed = case eta_seconds > 0 {
    True -> {
      let bytes_left = float.round(mbleft *. 1_048_576.0)
      bytes_left / eta_seconds
    }
    False ->
      case mbleft <=. 0.0 && mb >. 0.0 {
        True -> 0
        False -> 0
      }
  }

  // Map SABnzbd status to qBittorrent-equivalent state strings
  // so the shared torrent_state_to_download_status converter works
  let normalized_state = normalize_state(status)

  decode.success(TorrentInfo(
    hash: nzo_id,
    name: filename,
    progress: progress,
    dlspeed: dlspeed,
    eta: eta_seconds,
    state: normalized_state,
  ))
}

/// Decode a string that represents a number as a Float
fn string_as_float() -> decode.Decoder(Float) {
  decode.string
  |> decode.then(fn(s) {
    case float.parse(s) {
      Ok(f) -> decode.success(f)
      Error(_) ->
        case int.parse(s) {
          Ok(i) -> decode.success(int.to_float(i))
          Error(_) -> decode.success(0.0)
        }
    }
  })
}

/// Parse SABnzbd timeleft format "HH:MM:SS" into seconds
fn parse_timeleft(timeleft: String) -> Int {
  case string.split(timeleft, ":") {
    [h, m, s] -> {
      let hours = int.parse(h) |> result_unwrap(0)
      let minutes = int.parse(m) |> result_unwrap(0)
      let seconds = int.parse(s) |> result_unwrap(0)
      hours * 3600 + minutes * 60 + seconds
    }
    _ -> 0
  }
}

/// Map SABnzbd status strings to qBittorrent-equivalent state strings
/// so they work with the shared torrent_state_to_download_status converter
fn normalize_state(status: String) -> String {
  case status {
    "Downloading" -> "downloading"
    "Queued" -> "queuedDL"
    "Paused" -> "pausedDL"
    "Verifying" | "Extracting" | "Repairing" -> "downloading"
    "Completed" -> "checkingUP"
    _ -> status
  }
}

fn result_unwrap(result: Result(a, b), default: a) -> a {
  case result {
    Ok(v) -> v
    Error(_) -> default
  }
}
