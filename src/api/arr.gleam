import api/http_client
import gleam/dynamic/decode
import gleam/json
import gleam/option.{type Option, None}
import models/request.{type ArrQueueItem, ArrQueueItem}

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
    Ok(body) -> parse_queue_response(body, is_sonarr)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_queue_response(
  body: String,
  is_sonarr: Bool,
) -> Result(List(ArrQueueItem), ArrError) {
  let decoder = {
    use records <- decode.field("records", decode.list(queue_item_decoder(is_sonarr)))
    decode.success(records)
  }

  case json.parse(body, decoder) {
    Ok(items) -> Ok(items)
    Error(_) -> Error(ParseError("Failed to parse *arr queue response"))
  }
}

fn queue_item_decoder(_is_sonarr: Bool) -> decode.Decoder(ArrQueueItem) {
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

  decode.success(ArrQueueItem(
    id: id,
    title: title,
    status: status,
    size: size,
    sizeleft: sizeleft,
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    download_id: download_id,
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
