import api/http_client.{type ServiceError}
import config.{type Service}
import gleam/dynamic/decode
import gleam/float
import gleam/int
import gleam/result
import gleam/string
import models/request.{
  type Download, type DownloadStatus, Completed, Download, Downloading, NotFound,
  Paused, Queued, Stalled,
}

/// Fetch current downloads from SABnzbd queue
pub fn get_downloads(sabnzbd: Service) -> Result(List(Download), ServiceError) {
  let decoder = {
    use slots <- decode.subfield(["queue", "slots"], decode.list(slot_decoder()))
    decode.success(slots)
  }
  http_client.get_json(
    sabnzbd.url <> "/api?apikey=" <> sabnzbd.api_key <> "&mode=queue&output=json",
    [#("Accept", "application/json")],
    decoder,
    "SABnzbd response",
  )
}

fn slot_decoder() -> decode.Decoder(Download) {
  use nzo_id <- decode.field("nzo_id", decode.string)
  use filename <- decode.field("filename", decode.string)
  use percentage <- decode.field("percentage", string_as_float())
  use _mb <- decode.field("mb", string_as_float())
  use mbleft <- decode.field("mbleft", string_as_float())
  use timeleft <- decode.field("timeleft", decode.string)
  use status <- decode.field("status", decode.string)

  let progress = percentage /. 100.0

  let eta_seconds = parse_timeleft(timeleft)
  let dlspeed = case eta_seconds > 0 {
    True -> {
      let bytes_left = float.round(mbleft *. 1_048_576.0)
      bytes_left / eta_seconds
    }
    False -> 0
  }

  decode.success(Download(
    id: nzo_id,
    name: filename,
    progress: progress,
    speed: dlspeed,
    eta_seconds: eta_seconds,
    status: to_download_status(status),
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
      let hours = int.parse(h) |> result.unwrap(0)
      let minutes = int.parse(m) |> result.unwrap(0)
      let seconds = int.parse(s) |> result.unwrap(0)
      hours * 3600 + minutes * 60 + seconds
    }
    _ -> 0
  }
}

fn to_download_status(status: String) -> DownloadStatus {
  case status {
    "Downloading"
    | "Fetching"
    | "Verifying"
    | "Extracting"
    | "Repairing"
    | "QuickCheck"
    | "Moving" -> Downloading
    "Queued" | "Propagating" -> Queued
    "Paused" -> Paused
    "Completed" -> Completed
    "Failed" -> Stalled
    _ -> NotFound
  }
}
