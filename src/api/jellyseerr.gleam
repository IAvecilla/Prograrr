import api/http_client
import gleam/dynamic/decode
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import models/request.{
  type JellyseerrMedia, type JellyseerrRequest, JellyseerrMedia,
  JellyseerrRequest,
}

pub type JellyseerrError {
  HttpError(http_client.HttpError)
  ParseError(String)
}

/// Fetch all requests from Jellyseerr with full media details
/// Only enriches non-available requests to reduce API calls
pub fn get_requests(
  base_url: String,
  api_key: String,
) -> Result(List(JellyseerrRequest), JellyseerrError) {
  // Only fetch recent requests (limit to 20) to reduce load
  let url = base_url <> "/api/v1/request?take=20&skip=0&sort=added"
  let headers = [#("X-Api-Key", api_key), #("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> {
      case parse_requests_response(body) {
        Ok(requests) -> {
          // Only enrich non-available requests (status != 3) to reduce API calls
          // Status 3 = Available (already downloaded, no need for extra details)
          let enriched = list.map(requests, fn(req) {
            case req.status {
              3 -> req  // Skip enrichment for available items
              _ -> enrich_request(req, base_url, api_key)
            }
          })
          Ok(enriched)
        }
        Error(err) -> Error(err)
      }
    }
    Error(err) -> Error(HttpError(err))
  }
}

fn enrich_request(req: JellyseerrRequest, base_url: String, api_key: String) -> JellyseerrRequest {
  case req.media {
    Some(media) -> {
      case media.tmdb_id {
        Some(tmdb_id) -> {
          let media_type = case req.media_type {
            "tv" -> "tv"
            _ -> "movie"
          }
          case get_media_details(base_url, api_key, media_type, tmdb_id) {
            Ok(details) -> JellyseerrRequest(..req, media: Some(details))
            Error(_) -> req
          }
        }
        None -> req
      }
    }
    None -> req
  }
}

fn get_media_details(
  base_url: String,
  api_key: String,
  media_type: String,
  tmdb_id: Int,
) -> Result(JellyseerrMedia, JellyseerrError) {
  let url = base_url <> "/api/v1/" <> media_type <> "/" <> int_to_string(tmdb_id)
  let headers = [#("X-Api-Key", api_key), #("Accept", "application/json")]

  case http_client.get(url, headers) {
    Ok(body) -> parse_media_details(body, tmdb_id)
    Error(err) -> Error(HttpError(err))
  }
}

fn parse_media_details(body: String, tmdb_id: Int) -> Result(JellyseerrMedia, JellyseerrError) {
  let decoder = media_details_decoder(tmdb_id)
  case json.parse(body, decoder) {
    Ok(media) -> Ok(media)
    Error(_) -> Error(ParseError("Failed to parse media details"))
  }
}

fn media_details_decoder(tmdb_id: Int) -> decode.Decoder(JellyseerrMedia) {
  use title <- decode.optional_field("title", None, decode.optional(decode.string))
  use name <- decode.optional_field("name", None, decode.optional(decode.string))
  use poster_path <- decode.optional_field("posterPath", None, decode.optional(decode.string))
  use release_date <- decode.optional_field("releaseDate", None, decode.optional(decode.string))
  use first_air_date <- decode.optional_field("firstAirDate", None, decode.optional(decode.string))

  decode.success(JellyseerrMedia(
    id: tmdb_id,
    tmdb_id: Some(tmdb_id),
    tvdb_id: None,
    title: title,
    name: name,
    poster_path: poster_path,
    release_date: release_date,
    first_air_date: first_air_date,
  ))
}

fn parse_requests_response(
  body: String,
) -> Result(List(JellyseerrRequest), JellyseerrError) {
  let decoder = {
    use results <- decode.field("results", decode.list(request_decoder()))
    decode.success(results)
  }

  case json.parse(body, decoder) {
    Ok(requests) -> Ok(requests)
    Error(_) -> Error(ParseError("Failed to parse Jellyseerr response"))
  }
}

fn request_decoder() -> decode.Decoder(JellyseerrRequest) {
  use id <- decode.field("id", decode.int)
  use media_type <- decode.field("type", decode.string)
  use status <- decode.field("status", decode.int)
  use requested_by <- decode.optional_field(
    "requestedBy",
    None,
    decode.optional(nested_display_name_decoder()),
  )
  use created_at <- decode.optional_field(
    "createdAt",
    None,
    decode.optional(decode.string),
  )
  use media <- decode.optional_field("media", None, decode.optional(basic_media_decoder()))

  decode.success(JellyseerrRequest(
    id: id,
    media_type: media_type,
    status: status,
    requested_by: requested_by,
    created_at: created_at,
    media: media,
  ))
}

fn nested_display_name_decoder() -> decode.Decoder(String) {
  use display_name <- decode.field("displayName", decode.string)
  decode.success(display_name)
}

fn basic_media_decoder() -> decode.Decoder(JellyseerrMedia) {
  use id <- decode.field("id", decode.int)
  use tmdb_id <- decode.optional_field("tmdbId", None, decode.optional(decode.int))
  use tvdb_id <- decode.optional_field("tvdbId", None, decode.optional(decode.int))

  decode.success(JellyseerrMedia(
    id: id,
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    title: None,
    name: None,
    poster_path: None,
    release_date: None,
    first_air_date: None,
  ))
}

import gleam/int

fn int_to_string(n: Int) -> String {
  int.to_string(n)
}
