import api/arr
import api/jellyseerr
import api/qbittorrent
import config.{type Config}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import models/request.{
  type ArrQueueItem, type JellyseerrRequest, type MediaRequest, type TorrentInfo,
  MediaRequest, Movie, TvShow,
}

pub type AggregatorError {
  JellyseerrError(jellyseerr.JellyseerrError)
  SonarrError(arr.ArrError)
  RadarrError(arr.ArrError)
  QBitError(qbittorrent.QBitError)
}

/// Fetch and aggregate all data from configured services
pub fn get_all_requests(config: Config) -> List(MediaRequest) {
  // Fetch from all sources, using empty lists on error
  let jellyseerr_requests =
    jellyseerr.get_requests(config.jellyseerr_url, config.jellyseerr_api_key)
    |> result.unwrap([])

  let sonarr_queue =
    arr.get_sonarr_queue(config.sonarr_url, config.sonarr_api_key)
    |> result.unwrap([])

  let radarr_queue =
    arr.get_radarr_queue(config.radarr_url, config.radarr_api_key)
    |> result.unwrap([])

  let torrents =
    qbittorrent.get_torrents_with_auth(
      config.qbittorrent_url,
      config.qbittorrent_username,
      config.qbittorrent_password,
    )
    |> result.unwrap([])

  // Combine all data
  jellyseerr_requests
  |> list.map(fn(req) {
    combine_request(req, sonarr_queue, radarr_queue, torrents)
  })
}

fn combine_request(
  js_req: JellyseerrRequest,
  sonarr_queue: List(ArrQueueItem),
  radarr_queue: List(ArrQueueItem),
  torrents: List(TorrentInfo),
) -> MediaRequest {
  let media_type = case js_req.media_type {
    "movie" -> Movie
    _ -> TvShow
  }

  // Get media info
  let #(title, poster_url, year, tmdb_id, tvdb_id) = case js_req.media {
    Some(media) -> {
      let title = option.or(media.title, media.name) |> option.unwrap("Unknown")
      let poster = case media.poster_path {
        Some(path) -> Some("https://image.tmdb.org/t/p/w500" <> path)
        None -> None
      }
      let year = extract_year(option.or(media.release_date, media.first_air_date))
      #(title, poster, year, media.tmdb_id, media.tvdb_id)
    }
    None -> #("Unknown", None, None, None, None)
  }

  // Find matching queue item from Sonarr/Radarr
  let queue_item = case media_type {
    Movie -> find_radarr_queue_item(radarr_queue, tmdb_id)
    TvShow -> find_sonarr_queue_item(sonarr_queue, tvdb_id)
  }

  // Find matching torrent from qBittorrent
  let torrent = case queue_item {
    Some(qi) ->
      case qi.download_id {
        Some(dl_id) -> find_torrent_by_hash(torrents, dl_id)
        None -> find_torrent_by_name(torrents, title)
      }
    None -> find_torrent_by_name(torrents, title)
  }

  // Build the combined request
  MediaRequest(
    id: js_req.id,
    media_type: media_type,
    title: title,
    poster_url: poster_url,
    year: year,
    request_status: request.jellyseerr_status_to_request_status(js_req.status),
    requested_by: js_req.requested_by,
    requested_at: js_req.created_at,
    download_status: torrent
      |> option.map(fn(t) { request.torrent_state_to_download_status(t.state) }),
    download_progress: torrent |> option.map(fn(t) { t.progress *. 100.0 }),
    download_speed: torrent |> option.map(fn(t) { t.dlspeed }),
    eta_seconds: torrent |> option.map(fn(t) { t.eta }),
    queue_position: queue_item |> option.map(fn(qi) { qi.id }),
    queue_status: queue_item |> option.map(fn(qi) { qi.status }),
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
  )
}

fn find_radarr_queue_item(
  queue: List(ArrQueueItem),
  tmdb_id: Option(Int),
) -> Option(ArrQueueItem) {
  case tmdb_id {
    Some(id) ->
      queue
      |> list.find(fn(item) { item.tmdb_id == Some(id) })
      |> option.from_result
    None -> None
  }
}

fn find_sonarr_queue_item(
  queue: List(ArrQueueItem),
  tvdb_id: Option(Int),
) -> Option(ArrQueueItem) {
  case tvdb_id {
    Some(id) ->
      queue
      |> list.find(fn(item) { item.tvdb_id == Some(id) })
      |> option.from_result
    None -> None
  }
}

fn find_torrent_by_hash(
  torrents: List(TorrentInfo),
  hash: String,
) -> Option(TorrentInfo) {
  let lower_hash = string.lowercase(hash)
  torrents
  |> list.find(fn(t) { string.lowercase(t.hash) == lower_hash })
  |> option.from_result
}

fn find_torrent_by_name(
  torrents: List(TorrentInfo),
  title: String,
) -> Option(TorrentInfo) {
  let lower_title = string.lowercase(title)
  torrents
  |> list.find(fn(t) {
    string.lowercase(t.name) |> string.contains(lower_title)
  })
  |> option.from_result
}

fn extract_year(date: Option(String)) -> Option(Int) {
  case date {
    Some(d) -> {
      case string.split(d, "-") {
        [year, ..] ->
          int.parse(year)
          |> option.from_result
        _ -> None
      }
    }
    None -> None
  }
}
