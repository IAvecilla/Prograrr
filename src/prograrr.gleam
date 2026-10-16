import session
import config
import gleam/erlang/process
import gleam/io
import gleam/int
import gleam/option
import mist
import router.{Context}
import wisp
import wisp/wisp_mist

pub fn main() -> Nil {
  wisp.configure_logger()

  case config.load() {
    Ok(cfg) -> {
      io.println("Starting Prograrr on port " <> int.to_string(cfg.port))

      case cfg.api_key {
        option.Some(_) -> Nil
        option.None ->
          io.println(
            "WARNING: PROGRARR_AUTH=false, anyone who can reach this server can see your downloads",
          )
      }

      let ctx = Context(config: cfg, static_dir: "./static")

      let handler = router.handle_request(_, ctx)

      let assert Ok(_) =
        wisp_mist.handler(handler, session.secret_key_base(cfg.api_key))
        |> mist.new
        |> mist.port(cfg.port)
        |> mist.bind("0.0.0.0")
        |> mist.start

      io.println("Server started successfully!")
      process.sleep_forever()
    }
    Error(err) -> {
      io.println("Failed to load configuration: " <> err)
      io.println("See .env.example for all supported environment variables.")
    }
  }
}
