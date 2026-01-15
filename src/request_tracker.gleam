import config
import gleam/erlang/process
import gleam/io
import gleam/int
import mist
import router.{Context}
import wisp
import wisp/wisp_mist

pub fn main() -> Nil {
  // Set up logging
  wisp.configure_logger()

  // Load configuration
  case config.load() {
    Ok(cfg) -> {
      io.println("Starting Media Request Tracker on port " <> int.to_string(cfg.port))

      let ctx = Context(config: cfg, static_dir: "./static")

      let handler = router.handle_request(_, ctx)

      let assert Ok(_) =
        wisp_mist.handler(handler, "secret_key_change_me")
        |> mist.new
        |> mist.port(cfg.port)
        |> mist.bind("0.0.0.0")
        |> mist.start

      io.println("Server started successfully!")
      process.sleep_forever()
    }
    Error(err) -> {
      io.println("Failed to load configuration: " <> err)
      io.println("")
      io.println("Required environment variables:")
      io.println("  JELLYSEERR_API_KEY - Jellyseerr API key")
      io.println("  SONARR_API_KEY     - Sonarr API key")
      io.println("  RADARR_API_KEY     - Radarr API key")
      io.println("")
      io.println("Optional environment variables:")
      io.println("  PORT               - Server port (default: 3000)")
      io.println("  JELLYSEERR_URL     - Jellyseerr URL (default: http://localhost:5055)")
      io.println("  SONARR_URL         - Sonarr URL (default: http://localhost:8989)")
      io.println("  RADARR_URL         - Radarr URL (default: http://localhost:7878)")
      io.println("  QBITTORRENT_URL    - qBittorrent URL (default: http://localhost:8080)")
    }
  }
}
