import config
import gleam/erlang/process
import gleam/http/request.{type Request}
import gleam/int
import gleam/io
import gleam/option
import login_limiter
import mist
import router.{Context}
import session
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

      let assert Ok(limiter) = login_limiter.start()
      let ctx = Context(config: cfg, static_dir: "./static", limiter:)
      let secret_key_base = session.secret_key_base(cfg)

      let assert Ok(_) =
        fn(req: Request(mist.Connection)) {
          let handler = router.handle_request(_, ctx, client_ip(req))
          wisp_mist.handler(handler, secret_key_base)(req)
        }
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

/// The socket peer address. Behind a reverse proxy this is the proxy's IP.
fn client_ip(req: Request(mist.Connection)) -> String {
  case mist.get_client_info(req.body) {
    Ok(info) -> mist.ip_address_to_string(info.ip_address)
    Error(_) -> "unknown"
  }
}
