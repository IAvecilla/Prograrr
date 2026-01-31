import api/http_client
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
import models/request.{
  type ArrQueueItem, type RadarrMovie, type SonarrSeries, ArrQueueItem,
  RadarrMovie, SonarrSeries,
}

pub type ArrError {
  HttpError(http_client.HttpError)
  ParseError(String)
}

/// Fetch queue from Sonarr
pub fn get_sonarr_queue(
  base_url: String,
  api_key: String,
) -> Result(List(ArrQueueItem), ArrError) {
  get_queue(base_url, api_key, True)
}

/// Fetch queue from Radarr
pub fn get_radarr_queue(
  base_url: String,
  api_key: String,
) -> Result(List(ArrQueueItem), ArrError) {
  get_queue(base_url, api_key, False)
}

fn get_queue(
  base_url: String,
  api_key: String,
  is_sonarr: Bool,
) -> Result(List(ArrQueueItem), ArrError) {
  // includeMovie/includeSeries adds the full movie/series object with tmdbId/tvdbId
  let url = case is_sonarr {
    True -> base_url <> "/api/v3/queue?pageSize=100&includeUnknownSeriesItems=true&includeSeries=true"
    False -> base_url <> "/api/v3/queue?pageSize=100&includeMovie=true"
  }
  let headers = [#("X-Api-Key", api_key), #("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> parse_queue_response(body)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_queue_response(body: String) -> Result(List(ArrQueueItem), ArrError) {
  let decoder = {
    use records <- decode.field("records", decode.list(queue_item_decoder()))
    decode.success(records)
  }

  case json.parse(body, decoder) {
    Ok(items) -> Ok(items)
    Error(_) -> Error(ParseError("Failed to parse *arr queue response"))
  }
}

fn queue_item_decoder() -> decode.Decoder(ArrQueueItem) {
  use id <- decode.field("id", decode.int)
  use title <- decode.field("title", decode.string)
  use status <- decode.field("status", decode.string)
  use size <- decode.field("size", decode.int)
  use sizeleft <- decode.field("sizeleft", decode.int)
  use download_id <- decode.optional_field("downloadId", None, decode.optional(decode.string))

  // Try to extract TMDB ID from nested movie object (Radarr)
  use movie <- decode.optional_field("movie", None, decode.optional(movie_decoder()))
  let tmdb_id = option.flatten(movie)

  // Try to extract TVDB ID from nested series object (Sonarr)
  use series <- decode.optional_field("series", None, decode.optional(series_decoder()))
  let tvdb_id = option.flatten(series)

  // Try to extract quality name from nested quality.quality.name
  use quality <- decode.optional_field("quality", None, decode.optional(quality_decoder()))
  let quality_name = option.flatten(quality)

  decode.success(ArrQueueItem(
    id: id,
    title: title,
    status: status,
    size: size,
    sizeleft: sizeleft,
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    download_id: download_id,
    quality: quality_name,
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

/// Fetch all movies from Radarr with their monitored/hasFile status
pub fn get_radarr_movies(
  base_url: String,
  api_key: String,
) -> Result(List(RadarrMovie), ArrError) {
  let url = base_url <> "/api/v3/movie"
  let headers = [#("X-Api-Key", api_key), #("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> parse_radarr_movies(body)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_radarr_movies(body: String) -> Result(List(RadarrMovie), ArrError) {
  let decoder = decode.list(radarr_movie_decoder())

  case json.parse(body, decoder) {
    Ok(movies) -> Ok(movies)
    Error(_) -> Error(ParseError("Failed to parse Radarr movies response"))
  }
}

fn radarr_movie_decoder() -> decode.Decoder(RadarrMovie) {
  use id <- decode.field("id", decode.int)
  use tmdb_id <- decode.optional_field("tmdbId", None, decode.optional(decode.int))
  use monitored <- decode.field("monitored", decode.bool)
  use has_file <- decode.field("hasFile", decode.bool)
  use is_available <- decode.field("isAvailable", decode.bool)

  decode.success(RadarrMovie(
    id: id,
    tmdb_id: tmdb_id,
    monitored: monitored,
    has_file: has_file,
    is_available: is_available,
  ))
}

/// Fetch all series from Sonarr with their monitored status and episode counts
pub fn get_sonarr_series(
  base_url: String,
  api_key: String,
) -> Result(List(SonarrSeries), ArrError) {
  let url = base_url <> "/api/v3/series"
  let headers = [#("X-Api-Key", api_key), #("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> parse_sonarr_series(body)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_sonarr_series(body: String) -> Result(List(SonarrSeries), ArrError) {
  let decoder = decode.list(sonarr_series_decoder())

  case json.parse(body, decoder) {
    Ok(series) -> Ok(series)
    Error(_) -> Error(ParseError("Failed to parse Sonarr series response"))
  }
}

fn sonarr_series_decoder() -> decode.Decoder(SonarrSeries) {
  use id <- decode.field("id", decode.int)
  use tvdb_id <- decode.optional_field("tvdbId", None, decode.optional(decode.int))
  use monitored <- decode.field("monitored", decode.bool)
  // Statistics are nested in a "statistics" object
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
    id: id,
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
