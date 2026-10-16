import api/http_client.{type ServiceError}
import config.{type Service}
import gleam/dynamic/decode
import gleam/option.{type Option, None}
import models/request.{
  type ArrQueueItem, type RadarrMovie, type SonarrSeries, ArrQueueItem,
  RadarrMovie, SonarrSeries,
}

/// Fetch queue from Sonarr
pub fn get_sonarr_queue(
  sonarr: Service,
) -> Result(List(ArrQueueItem), ServiceError) {
  get_queue(
    sonarr,
    "includeUnknownSeriesItems=true&includeSeries=true&includeEpisode=true",
  )
}

/// Fetch queue from Radarr
pub fn get_radarr_queue(
  radarr: Service,
) -> Result(List(ArrQueueItem), ServiceError) {
  get_queue(radarr, "includeMovie=true")
}

fn get_queue(
  service: Service,
  include: String,
) -> Result(List(ArrQueueItem), ServiceError) {
  let decoder = {
    use records <- decode.field("records", decode.list(queue_item_decoder()))
    decode.success(records)
  }
  http_client.get_json(
    service.url <> "/api/v3/queue?pageSize=100&" <> include,
    headers(service),
    decoder,
    "*arr queue response",
  )
}

fn headers(service: Service) -> List(#(String, String)) {
  [#("X-Api-Key", service.api_key), #("Accept", "application/json")]
}

fn queue_item_decoder() -> decode.Decoder(ArrQueueItem) {
  use id <- decode.field("id", decode.int)
  use title <- decode.field("title", decode.string)
  use status <- decode.field("status", decode.string)
  use download_id <- decode.optional_field("downloadId", None, decode.optional(decode.string))

  use movie <- decode.optional_field("movie", None, decode.optional(movie_decoder()))
  let tmdb_id = option.flatten(movie)

  use series <- decode.optional_field("series", None, decode.optional(series_decoder()))
  let tvdb_id = option.flatten(series)

  use quality <- decode.optional_field("quality", None, decode.optional(quality_decoder()))
  let quality_name = option.flatten(quality)

  use season_number <- decode.optional_field("seasonNumber", None, decode.optional(decode.int))

  use episode <- decode.optional_field("episode", None, decode.optional(episode_info_decoder()))
  let #(episode_number, episode_title) = case episode {
    option.Some(ep) -> ep
    option.None -> #(None, None)
  }

  decode.success(ArrQueueItem(
    id: id,
    title: title,
    status: status,
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    download_id: download_id,
    quality: quality_name,
    season_number: season_number,
    episode_number: episode_number,
    episode_title: episode_title,
  ))
}

fn movie_decoder() -> decode.Decoder(Option(Int)) {
  use tmdb_id <- decode.optional_field("tmdbId", None, decode.optional(decode.int))
  decode.success(tmdb_id)
}

fn series_decoder() -> decode.Decoder(Option(Int)) {
  use tvdb_id <- decode.optional_field("tvdbId", None, decode.optional(decode.int))
  decode.success(tvdb_id)
}

fn quality_decoder() -> decode.Decoder(Option(String)) {
  // Quality is nested: quality.quality.name
  use inner_quality <- decode.optional_field("quality", None, decode.optional(quality_name_decoder()))
  decode.success(option.flatten(inner_quality))
}

fn quality_name_decoder() -> decode.Decoder(Option(String)) {
  use name <- decode.optional_field("name", None, decode.optional(decode.string))
  decode.success(name)
}

fn episode_info_decoder() -> decode.Decoder(#(Option(Int), Option(String))) {
  use episode_number <- decode.optional_field("episodeNumber", None, decode.optional(decode.int))
  use title <- decode.optional_field("title", None, decode.optional(decode.string))
  decode.success(#(episode_number, title))
}

/// Fetch all movies from Radarr with their monitored/hasFile status
pub fn get_radarr_movies(
  radarr: Service,
) -> Result(List(RadarrMovie), ServiceError) {
  http_client.get_json(
    radarr.url <> "/api/v3/movie",
    headers(radarr),
    decode.list(radarr_movie_decoder()),
    "Radarr movies response",
  )
}

fn radarr_movie_decoder() -> decode.Decoder(RadarrMovie) {
  use tmdb_id <- decode.optional_field("tmdbId", None, decode.optional(decode.int))
  use monitored <- decode.field("monitored", decode.bool)
  use has_file <- decode.field("hasFile", decode.bool)
  use is_available <- decode.field("isAvailable", decode.bool)

  decode.success(RadarrMovie(
    tmdb_id: tmdb_id,
    monitored: monitored,
    has_file: has_file,
    is_available: is_available,
  ))
}

/// Fetch all series from Sonarr with their monitored status and episode counts
pub fn get_sonarr_series(
  sonarr: Service,
) -> Result(List(SonarrSeries), ServiceError) {
  http_client.get_json(
    sonarr.url <> "/api/v3/series",
    headers(sonarr),
    decode.list(sonarr_series_decoder()),
    "Sonarr series response",
  )
}

fn sonarr_series_decoder() -> decode.Decoder(SonarrSeries) {
  use tvdb_id <- decode.optional_field("tvdbId", None, decode.optional(decode.int))
  use monitored <- decode.field("monitored", decode.bool)
  use statistics <- decode.optional_field(
    "statistics",
    None,
    decode.optional(statistics_decoder()),
  )
  let #(episode_count, episode_file_count) = case statistics {
    option.Some(stats) -> stats
    option.None -> #(0, 0)
  }

  decode.success(SonarrSeries(
    tvdb_id: tvdb_id,
    monitored: monitored,
    episode_count: episode_count,
    episode_file_count: episode_file_count,
  ))
}

fn statistics_decoder() -> decode.Decoder(#(Int, Int)) {
  use episode_count <- decode.optional_field("episodeCount", 0, decode.int)
  use episode_file_count <- decode.optional_field("episodeFileCount", 0, decode.int)
  decode.success(#(episode_count, episode_file_count))
}
