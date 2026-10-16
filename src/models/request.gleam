import gleam/json.{type Json}
import gleam/option.{type Option, Some}

/// Media type enumeration
pub type MediaType {
  Movie
  TvShow
}

/// Request status from Jellyseerr
pub type RequestStatus {
  Pending
  Approved
  Available
  Processing
  Unknown
}

/// Download status from qBittorrent or SABnzbd
pub type DownloadStatus {
  Downloading
  Seeding
  Paused
  Queued
  Stalled
  Completed
  NotFound
}

/// Per-episode download info for TV series
pub type EpisodeDownload {
  EpisodeDownload(
    season_number: Int,
    episode_number: Int,
    episode_title: Option(String),
    download_status: Option(DownloadStatus),
    download_progress: Option(Float),
    download_speed: Option(Int),
    eta_seconds: Option(Int),
    quality: Option(String),
  )
}

/// Combined media request with all status information
pub type MediaRequest {
  MediaRequest(
    id: Int,
    media_type: MediaType,
    title: String,
    poster_url: Option(String),
    year: Option(Int),
    // Jellyseerr status
    request_status: RequestStatus,
    requested_by: Option(String),
    requested_at: Option(String),
    // Download progress from qBittorrent
    download_status: Option(DownloadStatus),
    download_progress: Option(Float),
    download_speed: Option(Int),
    eta_seconds: Option(Int),
    // Queue info from Sonarr/Radarr
    queue_position: Option(Int),
    queue_status: Option(String),
    quality: Option(String),
    // External IDs for matching
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
    // Missing: monitored, no file, but is available (released) - should be downloadable
    is_missing: Bool,
    // Not available: monitored, no file, not available yet (not released)
    is_not_available: Bool,
    // Per-episode download tracking for TV series
    episode_downloads: List(EpisodeDownload),
  )
}

/// Jellyseerr request from API
pub type JellyseerrRequest {
  JellyseerrRequest(
    id: Int,
    media_type: String,
    status: Int,
    requested_by: Option(String),
    created_at: Option(String),
    media: Option(JellyseerrMedia),
    requested_seasons: List(Int),
  )
}

/// Jellyseerr media info
pub type JellyseerrMedia {
  JellyseerrMedia(
    id: Int,
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
    /// Media availability status from Jellyseerr (3=Processing, 4=Partially Available, 5=Available)
    media_status: Option(Int),
    title: Option(String),
    name: Option(String),
    poster_path: Option(String),
    release_date: Option(String),
    first_air_date: Option(String),
  )
}

/// Queue item from Sonarr/Radarr
pub type ArrQueueItem {
  ArrQueueItem(
    id: Int,
    title: String,
    status: String,
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
    download_id: Option(String),
    quality: Option(String),
    season_number: Option(Int),
    episode_number: Option(Int),
    episode_title: Option(String),
  )
}

/// Movie from Radarr with monitored/missing status
pub type RadarrMovie {
  RadarrMovie(
    tmdb_id: Option(Int),
    monitored: Bool,
    has_file: Bool,
    is_available: Bool,
  )
}

/// Series from Sonarr with monitored/missing status
pub type SonarrSeries {
  SonarrSeries(
    tvdb_id: Option(Int),
    monitored: Bool,
    episode_count: Int,
    episode_file_count: Int,
  )
}

/// An active download from qBittorrent or SABnzbd.
/// `id` is the torrent hash or SABnzbd nzo_id, matched against *arr's downloadId.
pub type Download {
  Download(
    id: String,
    name: String,
    /// 0.0 to 1.0
    progress: Float,
    /// Bytes per second
    speed: Int,
    eta_seconds: Int,
    status: DownloadStatus,
  )
}

// JSON Encoders

pub fn media_request_to_json(request: MediaRequest) -> Json {
  json.object([
    #("id", json.int(request.id)),
    #("mediaType", media_type_to_json(request.media_type)),
    #("title", json.string(request.title)),
    #("posterUrl", json.nullable(request.poster_url, json.string)),
    #("year", json.nullable(request.year, json.int)),
    #("requestStatus", request_status_to_json(request.request_status)),
    #("requestedBy", json.nullable(request.requested_by, json.string)),
    #("requestedAt", json.nullable(request.requested_at, json.string)),
    #("downloadStatus", json.nullable(request.download_status, download_status_to_json)),
    #("downloadProgress", json.nullable(request.download_progress, json.float)),
    #("downloadSpeed", json.nullable(request.download_speed, json.int)),
    #("etaSeconds", json.nullable(request.eta_seconds, json.int)),
    #("queuePosition", json.nullable(request.queue_position, json.int)),
    #("queueStatus", json.nullable(request.queue_status, json.string)),
    #("quality", json.nullable(request.quality, json.string)),
    #("isMissing", json.bool(request.is_missing)),
    #("isNotAvailable", json.bool(request.is_not_available)),
    #("episodeDownloads", json.array(request.episode_downloads, episode_download_to_json)),
  ])
}

pub fn media_requests_to_json(requests: List(MediaRequest)) -> Json {
  json.array(requests, media_request_to_json)
}

fn episode_download_to_json(ep: EpisodeDownload) -> Json {
  json.object([
    #("seasonNumber", json.int(ep.season_number)),
    #("episodeNumber", json.int(ep.episode_number)),
    #("episodeTitle", json.nullable(ep.episode_title, json.string)),
    #("downloadStatus", json.nullable(ep.download_status, download_status_to_json)),
    #("downloadProgress", json.nullable(ep.download_progress, json.float)),
    #("downloadSpeed", json.nullable(ep.download_speed, json.int)),
    #("etaSeconds", json.nullable(ep.eta_seconds, json.int)),
    #("quality", json.nullable(ep.quality, json.string)),
  ])
}

fn media_type_to_json(mt: MediaType) -> Json {
  case mt {
    Movie -> json.string("movie")
    TvShow -> json.string("tv")
  }
}

fn request_status_to_json(status: RequestStatus) -> Json {
  case status {
    Pending -> json.string("pending")
    Approved -> json.string("approved")
    Available -> json.string("available")
    Processing -> json.string("processing")
    Unknown -> json.string("unknown")
  }
}

fn download_status_to_json(status: DownloadStatus) -> Json {
  json.string(download_status_to_string(status))
}

pub fn download_status_to_string(status: DownloadStatus) -> String {
  case status {
    Downloading -> "downloading"
    Seeding -> "seeding"
    Paused -> "paused"
    Queued -> "queued"
    Stalled -> "stalled"
    Completed -> "completed"
    NotFound -> "not_found"
  }
}

// Conversion helpers

/// Convert Jellyseerr request + media status to RequestStatus.
/// Request status: 1=Pending, 2=Approved, 3=Declined
/// Media status: 3=Processing, 4=Partially Available, 5=Available
/// Media status takes priority when it indicates availability.
pub fn jellyseerr_status_to_request_status(
  request_status: Int,
  media_status: Option(Int),
) -> RequestStatus {
  case media_status {
    Some(4) | Some(5) -> Available
    Some(3) -> Processing
    _ ->
      case request_status {
        1 -> Pending
        2 -> Approved
        // Seerr sometimes returns media status values (4=Partially Available, 5=Available)
        // in the request status field
        4 | 5 -> Available
        _ -> Unknown
      }
  }
}

/// Convert qBittorrent torrent state to DownloadStatus.
/// See: https://github.com/qbittorrent/qBittorrent/wiki/WebUI-API-(qBittorrent-4.1)#torrent-management
pub fn torrent_state_to_download_status(state: String) -> DownloadStatus {
  case state {
    "downloading" | "forcedDL" -> Downloading
    "metaDL" | "allocating" | "queuedDL" | "checkingDL" -> Queued
    "stalledDL" -> Stalled
    "pausedDL" | "stoppedDL" -> Paused
    "uploading" | "stalledUP" | "pausedUP" | "stoppedUP" | "queuedUP" | "forcedUP" -> Seeding
    "checkingUP" -> Completed
    _ -> NotFound
  }
}
