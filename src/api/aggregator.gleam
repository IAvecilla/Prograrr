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
  type ArrQueueItem, type JellyseerrRequest, type MediaRequest, type RadarrMovie,
  type SonarrSeries, type TorrentInfo, MediaRequest, Movie, TvShow,
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

  // Fetch movie/series lists to check missing status
  let radarr_movies =
    arr.get_radarr_movies(config.radarr_url, config.radarr_api_key)
    |> result.unwrap([])

  let sonarr_series =
    arr.get_sonarr_series(config.sonarr_url, config.sonarr_api_key)
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
    combine_request(
      req,
      sonarr_queue,
      radarr_queue,
      radarr_movies,
      sonarr_series,
      torrents,
    )
  })
}

fn combine_request(
  js_req: JellyseerrRequest,
  sonarr_queue: List(ArrQueueItem),
  radarr_queue: List(ArrQueueItem),
  radarr_movies: List(RadarrMovie),
  sonarr_series: List(SonarrSeries),
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

  // Find matching queue item from Sonarr/Radarr by external ID
  let queue_item = case media_type {
    Movie -> find_queue_item(radarr_queue, tmdb_id, fn(i) { i.tmdb_id })
    TvShow -> find_queue_item(sonarr_queue, tvdb_id, fn(i) { i.tvdb_id })
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

  let request_status = request.jellyseerr_status_to_request_status(js_req.status)

  // Determine if request is missing based on Radarr/Sonarr status
  // Missing: monitored, no file, IS available (released) - should be downloadable
  // Not available: monitored, no file, NOT available (not released yet)
  let #(is_missing, is_not_available) = case media_type {
    Movie -> get_movie_status(radarr_movies, tmdb_id)
    TvShow -> #(is_series_missing(sonarr_series, tvdb_id), False)
  }

  // Build the combined request
  MediaRequest(
    id: js_req.id,
    media_type: media_type,
    title: title,
    poster_url: poster_url,
    year: year,
    request_status: request_status,
    requested_by: js_req.requested_by,
    requested_at: js_req.created_at,
    download_status: torrent
      |> option.map(fn(t) { request.torrent_state_to_download_status(t.state) }),
    download_progress: torrent |> option.map(fn(t) { t.progress *. 100.0 }),
    download_speed: torrent |> option.map(fn(t) { t.dlspeed }),
    eta_seconds: torrent |> option.map(fn(t) { t.eta }),
    queue_position: queue_item |> option.map(fn(qi) { qi.id }),
    queue_status: queue_item |> option.map(fn(qi) { qi.status }),
    quality: queue_item |> option.then(fn(qi) { qi.quality }),
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    is_missing: is_missing,
    is_not_available: is_not_available,
  )
}

/// Get movie status: (is_missing, is_not_available)
/// Missing: monitored, no file, IS available (released)
/// Not available: monitored, no file, NOT available (not released yet)
fn get_movie_status(
  movies: List(RadarrMovie),
  tmdb_id: Option(Int),
) -> #(Bool, Bool) {
  case tmdb_id {
    Some(id) -> {
      movies
      |> list.find(fn(m) { m.tmdb_id == Some(id) })
      |> result.map(fn(m) {
        let monitored_and_no_file = m.monitored && !m.has_file
        case monitored_and_no_file {
          True -> #(m.is_available, !m.is_available)
          False -> #(False, False)
        }
      })
      |> result.unwrap(#(False, False))
    }
    None -> #(False, False)
  }
}

/// Check if a series is missing episodes in Sonarr (monitored but missing episodes)
fn is_series_missing(series: List(SonarrSeries), tvdb_id: Option(Int)) -> Bool {
  case tvdb_id {
    Some(id) -> {
      series
      |> list.find(fn(s) { s.tvdb_id == Some(id) })
      |> result.map(fn(s) {
        s.monitored && s.episode_count > s.episode_file_count
      })
      |> result.unwrap(False)
    }
    None -> False
  }
}

/// Find a queue item by matching an external ID using the provided getter
fn find_queue_item(
  queue: List(ArrQueueItem),
  target_id: Option(Int),
  get_id: fn(ArrQueueItem) -> Option(Int),
) -> Option(ArrQueueItem) {
  case target_id {
    Some(id) ->
      queue
      |> list.find(fn(item) { get_id(item) == Some(id) })
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
  let title_words = extract_words(title)
  torrents
  |> list.find(fn(t) {
    let torrent_words = extract_words(t.name)
    // Check if all title words are present in torrent name
    list.all(title_words, fn(word) {
      list.contains(torrent_words, word)
    })
  })
  |> option.from_result
}

/// Extract words from a string for matching:
/// - Convert to lowercase
/// - Replace dots, underscores, hyphens with spaces
/// - Split into words and filter out empty/short ones
fn extract_words(s: String) -> List(String) {
  s
  |> string.lowercase
  |> string.replace(".", " ")
  |> string.replace("_", " ")
  |> string.replace("-", " ")
  |> string.replace("'", "")
  |> string.replace(":", "")
  |> string.split(" ")
  |> list.filter(fn(word) { string.length(word) > 1 })
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
