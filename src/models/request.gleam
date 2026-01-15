import gleam/json.{type Json}
import gleam/option.{type Option}

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

/// Download status from qBittorrent
pub type DownloadStatus {
  Downloading
  Seeding
  Paused
  Queued
  Stalled
  Completed
  NotFound
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
    // External IDs for matching
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
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
  )
}

/// Jellyseerr media info
pub type JellyseerrMedia {
  JellyseerrMedia(
    id: Int,
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
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
    size: Int,
    sizeleft: Int,
    tmdb_id: Option(Int),
    tvdb_id: Option(Int),
    download_id: Option(String),
  )
}

/// Torrent info from qBittorrent
pub type TorrentInfo {
  TorrentInfo(
    hash: String,
    name: String,
    progress: Float,
    dlspeed: Int,
    eta: Int,
    state: String,
  )
}

// JSON Encoders

pub fn media_request_to_json(request: MediaRequest) -> Json {
  json.object([
    #("id", json.int(request.id)),
    #("mediaType", media_type_to_json(request.media_type)),
    #("title", json.string(request.title)),
    #("posterUrl", option_to_json(request.poster_url, json.string)),
    #("year", option_to_json(request.year, json.int)),
    #("requestStatus", request_status_to_json(request.request_status)),
    #("requestedBy", option_to_json(request.requested_by, json.string)),
    #("requestedAt", option_to_json(request.requested_at, json.string)),
    #("downloadStatus", option_to_json(request.download_status, download_status_to_json)),
    #("downloadProgress", option_to_json(request.download_progress, json.float)),
    #("downloadSpeed", option_to_json(request.download_speed, json.int)),
    #("etaSeconds", option_to_json(request.eta_seconds, json.int)),
    #("queuePosition", option_to_json(request.queue_position, json.int)),
    #("queueStatus", option_to_json(request.queue_status, json.string)),
  ])
}

pub fn media_requests_to_json(requests: List(MediaRequest)) -> Json {
  json.array(requests, media_request_to_json)
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
  case status {
    Downloading -> json.string("downloading")
    Seeding -> json.string("seeding")
    Paused -> json.string("paused")
    Queued -> json.string("queued")
    Stalled -> json.string("stalled")
    Completed -> json.string("completed")
    NotFound -> json.string("not_found")
  }
}

fn option_to_json(opt: Option(a), encoder: fn(a) -> Json) -> Json {
  case opt {
    option.Some(value) -> encoder(value)
    option.None -> json.null()
  }
}

// Conversion helpers

pub fn jellyseerr_status_to_request_status(status: Int) -> RequestStatus {
  // Jellyseerr MediaRequest status codes:
  // 1 = Pending (awaiting approval)
  // 2 = Approved (approved, being processed)
  // 3 = Declined
  // Media status codes (sometimes returned):
  // 3 = Processing
  // 4 = Partially Available
  // 5 = Available
  case status {
    1 -> Pending
    2 -> Approved
    3 -> Processing
    4 -> Available
    5 -> Available
    _ -> Unknown
  }
}

pub fn torrent_state_to_download_status(state: String) -> DownloadStatus {
  case state {
    "downloading" -> Downloading
    "stalledDL" -> Stalled
    "pausedDL" -> Paused
    "queuedDL" -> Queued
    "uploading" -> Seeding
    "stalledUP" -> Seeding
    "pausedUP" -> Seeding
    "queuedUP" -> Seeding
    "checkingDL" -> Queued
    "checkingUP" -> Completed
    "forcedDL" -> Downloading
    "forcedUP" -> Seeding
    "allocating" -> Queued
    "metaDL" -> Queued
    _ -> NotFound
  }
}
