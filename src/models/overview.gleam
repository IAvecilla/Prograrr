import gleam/json.{type Json}
import gleam/option.{type Option}

/// Top-level response for /api/overview
pub type OverviewResponse {
  OverviewResponse(
    downloading: List(ProcessedRequest),
    missing: List(ProcessedRequest),
  )
}

/// A merged, processed media item ready to render
pub type ProcessedRequest {
  ProcessedRequest(
    title: String,
    media_type: String,
    poster_url: Option(String),
    year: Option(Int),
    requested_by: Option(String),
    effective_status: Option(String),
    // Movie-level download fields (null for TV)
    progress: Option(Float),
    speed: Option(Int),
    eta: Option(Int),
    quality: Option(String),
    // TV show season groupings (empty for movies)
    seasons: List(SeasonProgress),
  )
}

/// Per-season aggregate with episode list
pub type SeasonProgress {
  SeasonProgress(
    season_number: Int,
    episode_count: Int,
    progress: Float,
    speed: Int,
    eta: Int,
    episodes: List(EpisodeProgress),
  )
}

/// Individual episode download info
pub type EpisodeProgress {
  EpisodeProgress(
    episode_number: Int,
    title: Option(String),
    progress: Float,
    speed: Int,
    eta: Int,
    status: Option(String),
    quality: Option(String),
  )
}

// JSON Encoders

pub fn overview_response_to_json(resp: OverviewResponse) -> Json {
  json.object([
    #("downloading", json.array(resp.downloading, processed_request_to_json)),
    #("missing", json.array(resp.missing, processed_request_to_json)),
  ])
}

fn processed_request_to_json(req: ProcessedRequest) -> Json {
  json.object([
    #("title", json.string(req.title)),
    #("mediaType", json.string(req.media_type)),
    #("posterUrl", json.nullable(req.poster_url, json.string)),
    #("year", json.nullable(req.year, json.int)),
    #("requestedBy", json.nullable(req.requested_by, json.string)),
    #("effectiveStatus", json.nullable(req.effective_status, json.string)),
    #("progress", json.nullable(req.progress, json.float)),
    #("speed", json.nullable(req.speed, json.int)),
    #("eta", json.nullable(req.eta, json.int)),
    #("quality", json.nullable(req.quality, json.string)),
    #("seasons", json.array(req.seasons, season_progress_to_json)),
  ])
}

fn season_progress_to_json(season: SeasonProgress) -> Json {
  json.object([
    #("seasonNumber", json.int(season.season_number)),
    #("episodeCount", json.int(season.episode_count)),
    #("progress", json.float(season.progress)),
    #("speed", json.int(season.speed)),
    #("eta", json.int(season.eta)),
    #("episodes", json.array(season.episodes, episode_progress_to_json)),
  ])
}

fn episode_progress_to_json(ep: EpisodeProgress) -> Json {
  json.object([
    #("episodeNumber", json.int(ep.episode_number)),
    #("title", json.nullable(ep.title, json.string)),
    #("progress", json.float(ep.progress)),
    #("speed", json.int(ep.speed)),
    #("eta", json.int(ep.eta)),
    #("status", json.nullable(ep.status, json.string)),
    #("quality", json.nullable(ep.quality, json.string)),
  ])
}
