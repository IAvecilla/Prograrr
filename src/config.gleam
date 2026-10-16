import envoy
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

/// URL + API key pair used by most services
pub type Service {
  Service(url: String, api_key: String)
}

pub type QBittorrent {
  QBittorrent(url: String, username: String, password: String)
}

/// Application configuration loaded from environment variables.
/// Download clients are optional: each one is enabled when its URL is set.
pub type Config {
  Config(
    port: Int,
    /// `None` when auth is disabled with PROGRARR_AUTH=false
    api_key: Option(String),
    cors_origin: Option(String),
    jellyseerr: Service,
    sonarr: Service,
    radarr: Service,
    qbittorrent: Option(QBittorrent),
    sabnzbd: Option(Service),
  )
}

pub fn load() -> Result(Config, String) {
  use port <- result.try(port())
  use api_key <- result.try(api_key())
  use jellyseerr <- result.try(service("JELLYSEERR"))
  use sonarr <- result.try(service("SONARR"))
  use radarr <- result.try(service("RADARR"))
  use qbittorrent <- result.try(
    when_set("QBITTORRENT_URL", fn(url) {
      use username <- result.try(required("QBITTORRENT_USERNAME"))
      use password <- result.try(required("QBITTORRENT_PASSWORD"))
      Ok(QBittorrent(url:, username:, password:))
    }),
  )
  use sabnzbd <- result.try(
    when_set("SABNZBD_URL", fn(url) {
      use api_key <- result.try(required("SABNZBD_API_KEY"))
      Ok(Service(url:, api_key:))
    }),
  )

  case qbittorrent, sabnzbd {
    None, None ->
      Error("At least one download client must be configured: set QBITTORRENT_URL or SABNZBD_URL")
    _, _ ->
      Ok(Config(
        port:,
        api_key:,
        cors_origin: optional("CORS_ORIGIN"),
        jellyseerr:,
        sonarr:,
        radarr:,
        qbittorrent:,
        sabnzbd:,
      ))
  }
}

fn api_key() -> Result(Option(String), String) {
  case optional("PROGRARR_AUTH") {
    Some("false") -> Ok(None)
    None | Some("true") -> required("PROGRARR_API_KEY") |> result.map(Some)
    Some(value) ->
      Error("Invalid value for PROGRARR_AUTH (expected true or false): " <> value)
  }
}

/// Load `<PREFIX>_URL` and `<PREFIX>_API_KEY`, both required
fn service(prefix: String) -> Result(Service, String) {
  use url <- result.try(required(prefix <> "_URL"))
  use api_key <- result.try(required(prefix <> "_API_KEY"))
  Ok(Service(url:, api_key:))
}

/// Run `load` only when `key` is set, otherwise the service is disabled
fn when_set(
  key: String,
  load: fn(String) -> Result(a, String),
) -> Result(Option(a), String) {
  case optional(key) {
    Some(value) -> load(value) |> result.map(Some)
    None -> Ok(None)
  }
}

fn port() -> Result(Int, String) {
  case optional("PORT") {
    None -> Ok(3000)
    Some(value) ->
      int.parse(value)
      |> result.replace_error("Invalid integer for PORT: " <> value)
  }
}

fn required(key: String) -> Result(String, String) {
  case optional(key) {
    Some(value) -> Ok(value)
    None -> Error("Missing required environment variable: " <> key)
  }
}

/// Unset and empty variables are both treated as missing
fn optional(key: String) -> Option(String) {
  case envoy.get(key) {
    Ok("") | Error(_) -> None
    Ok(value) -> Some(value)
  }
}
