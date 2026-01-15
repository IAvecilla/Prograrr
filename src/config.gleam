import envoy
import gleam/int
import gleam/result

/// Application configuration loaded from environment variables
pub type Config {
  Config(
    port: Int,
    jellyseerr_url: String,
    jellyseerr_api_key: String,
    sonarr_url: String,
    sonarr_api_key: String,
    radarr_url: String,
    radarr_api_key: String,
    qbittorrent_url: String,
    qbittorrent_username: String,
    qbittorrent_password: String,
  )
}

/// Load configuration from environment variables
pub fn load() -> Result(Config, String) {
  use port <- result.try(get_env_int("PORT", 3000))
  use jellyseerr_url <- result.try(get_env("JELLYSEERR_URL", "http://localhost:5055"))
  use jellyseerr_api_key <- result.try(get_env_required("JELLYSEERR_API_KEY"))
  use sonarr_url <- result.try(get_env("SONARR_URL", "http://localhost:8989"))
  use sonarr_api_key <- result.try(get_env_required("SONARR_API_KEY"))
  use radarr_url <- result.try(get_env("RADARR_URL", "http://localhost:7878"))
  use radarr_api_key <- result.try(get_env_required("RADARR_API_KEY"))
  use qbittorrent_url <- result.try(get_env("QBITTORRENT_URL", "http://localhost:8080"))
  use qbittorrent_username <- result.try(get_env("QBITTORRENT_USERNAME", "admin"))
  use qbittorrent_password <- result.try(get_env("QBITTORRENT_PASSWORD", "adminadmin"))

  Ok(Config(
    port: port,
    jellyseerr_url: jellyseerr_url,
    jellyseerr_api_key: jellyseerr_api_key,
    sonarr_url: sonarr_url,
    sonarr_api_key: sonarr_api_key,
    radarr_url: radarr_url,
    radarr_api_key: radarr_api_key,
    qbittorrent_url: qbittorrent_url,
    qbittorrent_username: qbittorrent_username,
    qbittorrent_password: qbittorrent_password,
  ))
}

fn get_env(key: String, default: String) -> Result(String, String) {
  Ok(result.unwrap(envoy.get(key), default))
}

fn get_env_required(key: String) -> Result(String, String) {
  case envoy.get(key) {
    Ok(value) -> Ok(value)
    Error(_) -> Error("Missing required environment variable: " <> key)
  }
}

fn get_env_int(key: String, default: Int) -> Result(Int, String) {
  case envoy.get(key) {
    Ok(value) -> {
      case int.parse(value) {
        Ok(n) -> Ok(n)
        Error(_) -> Error("Invalid integer for " <> key <> ": " <> value)
      }
    }
    Error(_) -> Ok(default)
  }
}
