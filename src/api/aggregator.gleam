import api/arr
import api/jellyseerr
import api/qbittorrent
import config.{type Config}
import gleam/dict
import gleam/float
import gleam/int
import gleam/list
import gleam/order
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import models/overview.{
  type OverviewResponse, type ProcessedRequest, type SeasonProgress,
  EpisodeProgress, OverviewResponse, ProcessedRequest, SeasonProgress,
}
import models/request.{
  type ArrQueueItem, type DownloadStatus, type EpisodeDownload,
  type JellyseerrRequest, type MediaRequest, type RadarrMovie,
  type SonarrSeries, type TorrentInfo, Completed, Downloading, EpisodeDownload,
  MediaRequest, Movie, Paused, Queued, Seeding, Stalled, TvShow,
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

  // Find matching queue item(s) from Sonarr/Radarr by external ID
  let queue_item = case media_type {
    Movie -> find_queue_item(radarr_queue, tmdb_id, fn(i) { i.tmdb_id })
    TvShow -> find_queue_item(sonarr_queue, tvdb_id, fn(i) { i.tvdb_id })
  }

  // For TV shows, find ALL matching queue items for per-episode tracking
  let requested_seasons = js_req.requested_seasons
  let episode_downloads = case media_type {
    TvShow -> {
      let all_items =
        find_all_queue_items(sonarr_queue, tvdb_id, fn(i) { i.tvdb_id })
      all_items
      // Filter to only episodes from requested seasons (if seasons specified)
      |> list.filter(fn(qi) {
        case requested_seasons {
          [] -> True
          seasons ->
            case qi.season_number {
              Some(sn) -> list.contains(seasons, sn)
              None -> True
            }
        }
      })
      |> list.map(fn(qi) { build_episode_download(qi, torrents) })
      |> list.sort(fn(a, b) {
        case int.compare(a.season_number, b.season_number) {
          order.Eq -> int.compare(a.episode_number, b.episode_number)
          other -> other
        }
      })
    }
    Movie -> []
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
    episode_downloads: episode_downloads,
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

/// Find ALL queue items matching an external ID
fn find_all_queue_items(
  queue: List(ArrQueueItem),
  target_id: Option(Int),
  get_id: fn(ArrQueueItem) -> Option(Int),
) -> List(ArrQueueItem) {
  case target_id {
    Some(id) ->
      queue
      |> list.filter(fn(item) { get_id(item) == Some(id) })
    None -> []
  }
}

/// Build an EpisodeDownload from a queue item by finding its torrent
fn build_episode_download(
  qi: ArrQueueItem,
  torrents: List(TorrentInfo),
) -> EpisodeDownload {
  let torrent = case qi.download_id {
    Some(dl_id) -> find_torrent_by_hash(torrents, dl_id)
    None -> None
  }

  EpisodeDownload(
    season_number: option.unwrap(qi.season_number, 0),
    episode_number: option.unwrap(qi.episode_number, 0),
    episode_title: qi.episode_title,
    download_status: torrent
      |> option.map(fn(t) { request.torrent_state_to_download_status(t.state) }),
    download_progress: torrent |> option.map(fn(t) { t.progress *. 100.0 }),
    download_speed: torrent |> option.map(fn(t) { t.dlspeed }),
    eta_seconds: torrent |> option.map(fn(t) { t.eta }),
    quality: qi.quality,
  )
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

// ---------------------------------------------------------------------------
// Overview endpoint processing
// ---------------------------------------------------------------------------

/// Build the processed overview response with downloading and missing categories.
/// TV shows with multiple Jellyseerr requests are merged into single entries.
pub fn get_overview(config: Config) -> OverviewResponse {
  let requests = get_all_requests(config)

  // Separate into downloading and missing
  let downloading = list.filter(requests, is_actively_downloading)
  let missing =
    list.filter(requests, fn(r) {
      { r.is_missing || r.is_not_available } && !is_actively_downloading(r)
    })

  // Merge same-series TV shows and convert to ProcessedRequest
  let processed_downloading = merge_and_process(downloading, True)
  let processed_missing = merge_and_process(missing, False)

  OverviewResponse(
    downloading: processed_downloading,
    missing: processed_missing,
  )
}

/// Check if a request has any active download status
fn is_actively_downloading(req: MediaRequest) -> Bool {
  let main_active = case req.download_status {
    Some(status) -> is_active_status(status)
    None -> False
  }
  case main_active {
    True -> True
    False ->
      list.any(req.episode_downloads, fn(ep) {
        case ep.download_status {
          Some(status) -> is_active_status(status)
          None -> False
        }
      })
  }
}

fn is_active_status(status: DownloadStatus) -> Bool {
  case status {
    Downloading | Queued | Paused | Stalled -> True
    _ -> False
  }
}

/// Merge same-series TV shows and convert all requests to ProcessedRequest
fn merge_and_process(
  requests: List(MediaRequest),
  is_downloading: Bool,
) -> List(ProcessedRequest) {
  // Separate movies and TV shows
  let movies =
    list.filter(requests, fn(r) {
      case r.media_type {
        Movie -> True
        _ -> False
      }
    })
  let tv_shows =
    list.filter(requests, fn(r) {
      case r.media_type {
        TvShow -> True
        _ -> False
      }
    })

  // Convert movies directly
  let processed_movies = list.map(movies, fn(r) { movie_to_processed(r) })

  // Group TV shows by series identity and merge
  let tv_groups = group_tv_by_series(tv_shows)
  let processed_tv =
    dict.values(tv_groups)
    |> list.map(fn(group) { merge_tv_group(group, is_downloading) })

  list.append(processed_movies, processed_tv)
}

/// Convert a movie MediaRequest to a ProcessedRequest
fn movie_to_processed(r: MediaRequest) -> ProcessedRequest {
  let effective_status = case r.download_status {
    Some(status) ->
      case is_active_status(status) {
        True -> Some(download_status_to_string(status))
        False -> None
      }
    None -> None
  }

  ProcessedRequest(
    title: r.title,
    media_type: "movie",
    poster_url: r.poster_url,
    year: r.year,
    requested_by: r.requested_by,
    effective_status: effective_status,
    progress: r.download_progress,
    speed: r.download_speed,
    eta: r.eta_seconds,
    quality: r.quality,
    seasons: [],
  )
}

/// Group TV shows by series identity (tvdb_id or title+year fallback)
fn group_tv_by_series(
  shows: List(MediaRequest),
) -> dict.Dict(String, List(MediaRequest)) {
  list.fold(shows, dict.new(), fn(acc, show) {
    let key = case show.tvdb_id {
      Some(id) -> "tvdb:" <> int.to_string(id)
      None ->
        "title:"
        <> show.title
        <> "|"
        <> option.map(show.year, int.to_string)
        |> option.unwrap("")
    }
    let existing = dict.get(acc, key) |> result.unwrap([])
    dict.insert(acc, key, list.append(existing, [show]))
  })
}

/// Merge a group of TV show requests into a single ProcessedRequest
fn merge_tv_group(
  group: List(MediaRequest),
  is_downloading: Bool,
) -> ProcessedRequest {
  // Use first request as base for metadata
  let assert [base, ..] = group

  // Collect all episode downloads, de-duplicate by season+episode
  let all_episodes =
    list.flat_map(group, fn(r) { r.episode_downloads })
    |> dedup_episodes

  // Filter out completed/seeding if this is a downloading group
  let active_episodes = case is_downloading {
    True -> list.filter(all_episodes, is_episode_active)
    False -> all_episodes
  }

  // Group by season and build SeasonProgress entries
  let seasons = build_season_groups(active_episodes)

  // Compute effective status across all active episodes
  let effective_status = case is_downloading {
    True -> best_episode_status(active_episodes)
    False -> None
  }

  ProcessedRequest(
    title: base.title,
    media_type: "tv",
    poster_url: base.poster_url,
    year: base.year,
    requested_by: base.requested_by,
    effective_status: effective_status,
    progress: None,
    speed: None,
    eta: None,
    quality: None,
    seasons: seasons,
  )
}

/// De-duplicate episodes by season_number + episode_number, keeping the first occurrence
fn dedup_episodes(episodes: List(EpisodeDownload)) -> List(EpisodeDownload) {
  let #(_, result) =
    list.fold(episodes, #(dict.new(), []), fn(acc, ep) {
      let #(seen, kept) = acc
      let key =
        int.to_string(ep.season_number) <> "-" <> int.to_string(ep.episode_number)
      case dict.get(seen, key) {
        Ok(_) -> #(seen, kept)
        Error(_) -> #(dict.insert(seen, key, True), list.append(kept, [ep]))
      }
    })
  result
}

/// Check if an episode download is still active (not completed/seeding)
fn is_episode_active(ep: EpisodeDownload) -> Bool {
  case ep.download_status {
    Some(Completed) | Some(Seeding) -> False
    _ -> {
      case ep.download_progress {
        Some(p) if p >=. 100.0 -> False
        _ -> True
      }
    }
  }
}

/// Group episodes by season number and compute aggregates
fn build_season_groups(episodes: List(EpisodeDownload)) -> List(SeasonProgress) {
  // Group by season number
  let grouped =
    list.fold(episodes, dict.new(), fn(acc, ep) {
      let existing = dict.get(acc, ep.season_number) |> result.unwrap([])
      dict.insert(acc, ep.season_number, list.append(existing, [ep]))
    })

  dict.to_list(grouped)
  |> list.map(fn(pair) {
    let #(season_num, eps) = pair
    let ep_count = list.length(eps)

    // Compute aggregates
    let total_progress =
      list.fold(eps, 0.0, fn(sum, ep) {
        sum +. option.unwrap(ep.download_progress, 0.0)
      })
    let avg_progress = case ep_count {
      0 -> 0.0
      n -> total_progress /. int.to_float(n)
    }
    let total_speed =
      list.fold(eps, 0, fn(sum, ep) { sum + option.unwrap(ep.download_speed, 0) })
    let max_eta =
      list.fold(eps, 0, fn(mx, ep) { int.max(mx, option.unwrap(ep.eta_seconds, 0)) })

    // Convert episodes to EpisodeProgress
    let episode_details =
      eps
      |> list.sort(fn(a, b) { int.compare(a.episode_number, b.episode_number) })
      |> list.map(fn(ep) {
        EpisodeProgress(
          episode_number: ep.episode_number,
          title: ep.episode_title,
          progress: option.unwrap(ep.download_progress, 0.0),
          speed: option.unwrap(ep.download_speed, 0),
          eta: option.unwrap(ep.eta_seconds, 0),
          status: option.map(ep.download_status, download_status_to_string),
          quality: ep.quality,
        )
      })

    SeasonProgress(
      season_number: season_num,
      episode_count: ep_count,
      progress: round_to_1dp(avg_progress),
      speed: total_speed,
      eta: max_eta,
      episodes: episode_details,
    )
  })
  |> list.sort(fn(a, b) { int.compare(a.season_number, b.season_number) })
}

/// Determine the best (highest priority) status across episodes
/// Priority: downloading > paused > queued > stalled
fn best_episode_status(episodes: List(EpisodeDownload)) -> Option(String) {
  let best_priority =
    list.fold(episodes, #(None, -1), fn(acc, ep) {
      let #(best, best_pri) = acc
      let pri = case ep.download_status {
        Some(Downloading) -> 4
        Some(Paused) -> 3
        Some(Queued) -> 2
        Some(Stalled) -> 1
        _ -> 0
      }
      case pri > best_pri {
        True -> #(ep.download_status, pri)
        False -> #(best, best_pri)
      }
    })
  case best_priority {
    #(Some(status), pri) if pri > 0 -> Some(download_status_to_string(status))
    _ -> Some("downloading")
  }
}

fn download_status_to_string(status: DownloadStatus) -> String {
  case status {
    Downloading -> "downloading"
    Seeding -> "seeding"
    Paused -> "paused"
    Queued -> "queued"
    Stalled -> "stalled"
    Completed -> "completed"
    request.NotFound -> "not_found"
  }
}

/// Round a float to 1 decimal place
fn round_to_1dp(value: Float) -> Float {
  let scaled = float.round(value *. 10.0)
  int.to_float(scaled) /. 10.0
}
