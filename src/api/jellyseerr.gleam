import api/http_client.{type ServiceError}
import config.{type Service}
import gleam/dynamic/decode
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import models/request.{
  type JellyseerrMedia, type JellyseerrRequest, JellyseerrMedia,
  JellyseerrRequest,
}

/// Fetch recent requests from Jellyseerr with full media details.
/// Only enriches non-available requests to reduce API calls.
pub fn get_requests(
  jellyseerr: Service,
) -> Result(List(JellyseerrRequest), ServiceError) {
  let decoder = {
    use results <- decode.field("results", decode.list(request_decoder()))
    decode.success(results)
  }
  use requests <- result.map(http_client.get_json(
    jellyseerr.url <> "/api/v1/request?take=20&skip=0&sort=added",
    headers(jellyseerr),
    decoder,
    "Jellyseerr response",
  ))
  list.map(requests, fn(req) {
    case req.status {
      3 -> req
      _ -> enrich_request(req, jellyseerr)
    }
  })
}

fn headers(jellyseerr: Service) -> List(#(String, String)) {
  [#("X-Api-Key", jellyseerr.api_key), #("Accept", "application/json")]
}

fn enrich_request(
  req: JellyseerrRequest,
  jellyseerr: Service,
) -> JellyseerrRequest {
  case req.media {
    Some(JellyseerrMedia(tmdb_id: Some(tmdb_id), ..) as media) -> {
      let media_type = case req.media_type {
        "tv" -> "tv"
        _ -> "movie"
      }
      let details =
        http_client.get_json(
          jellyseerr.url
            <> "/api/v1/"
            <> media_type
            <> "/"
            <> int.to_string(tmdb_id),
          headers(jellyseerr),
          media_details_decoder(tmdb_id),
          "media details",
        )
      case details {
        Ok(details) ->
          JellyseerrRequest(
            ..req,
            media: Some(
              JellyseerrMedia(
                ..details,
                tvdb_id: media.tvdb_id,
                media_status: media.media_status,
              ),
            ),
          )
        Error(_) -> req
      }
    }
    _ -> req
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
    media_status: None,
    title: title,
    name: name,
    poster_path: poster_path,
    release_date: release_date,
    first_air_date: first_air_date,
  ))
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
  use seasons <- decode.optional_field(
    "seasons",
    [],
    decode.list(season_number_decoder()),
  )

  decode.success(JellyseerrRequest(
    id: id,
    media_type: media_type,
    status: status,
    requested_by: requested_by,
    created_at: created_at,
    media: media,
    requested_seasons: seasons,
  ))
}

fn nested_display_name_decoder() -> decode.Decoder(String) {
  use display_name <- decode.field("displayName", decode.string)
  decode.success(display_name)
}

fn season_number_decoder() -> decode.Decoder(Int) {
  use season_number <- decode.field("seasonNumber", decode.int)
  decode.success(season_number)
}

fn basic_media_decoder() -> decode.Decoder(JellyseerrMedia) {
  use id <- decode.field("id", decode.int)
  use tmdb_id <- decode.optional_field("tmdbId", None, decode.optional(decode.int))
  use tvdb_id <- decode.optional_field("tvdbId", None, decode.optional(decode.int))
  use media_status <- decode.optional_field("status", None, decode.optional(decode.int))

  decode.success(JellyseerrMedia(
    id: id,
    tmdb_id: tmdb_id,
    tvdb_id: tvdb_id,
    media_status: media_status,
    title: None,
    name: None,
    poster_path: None,
    release_date: None,
    first_air_date: None,
  ))
}
