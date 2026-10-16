import gleam/bit_array
import gleam/crypto
import gleam/erlang/process
import gleam/http
import gleam/http/cookie
import gleam/http/request
import gleam/http/response
import gleam/int
import gleam/json
import gleam/option.{type Option, None, Some}
import gleam/result
import wisp.{type Request, type Response}

const cookie_name = "prograrr_session"

const session_seconds = 2_592_000

/// Secret used by wisp to sign cookies. Derived from the API key so that
/// rotating PROGRARR_API_KEY invalidates every existing browser session.
pub fn secret_key_base(api_key: Option(String)) -> String {
  case api_key {
    Some(key) ->
      crypto.hash(crypto.Sha512, <<"prograrr-session:", key:utf8>>)
      |> bit_array.base16_encode
    None -> wisp.random_string(64)
  }
}

/// A request is authorized by a valid X-Api-Key header (API clients) or a
/// valid session cookie (browsers). Always true when auth is disabled.
pub fn is_authorized(req: Request, api_key: Option(String)) -> Bool {
  case api_key {
    None -> True
    Some(key) ->
      case request.get_header(req, "x-api-key") {
        Ok(provided) -> key_matches(provided, key)
        Error(_) -> has_valid_session(req)
      }
  }
}

/// Check the submitted key and start a browser session
pub fn login(req: Request, api_key: Option(String), submitted: String) -> Response {
  case api_key {
    None -> wisp.no_content()
    Some(key) ->
      case key_matches(submitted, key) {
        True ->
          wisp.no_content()
          |> set_session_cookie(req, int.to_string(now()), session_seconds)
        False -> unauthorized()
      }
  }
}

pub fn logout(req: Request) -> Response {
  wisp.no_content()
  |> set_session_cookie(req, "", 0)
}

pub fn unauthorized() -> Response {
  // Delay failed auth responses to slow down brute force attempts
  process.sleep(1000)
  json.object([#("error", json.string("Unauthorized"))])
  |> json.to_string
  |> wisp.json_response(401)
}

fn key_matches(provided: String, key: String) -> Bool {
  provided != ""
  && crypto.secure_compare(
    bit_array.from_string(provided),
    bit_array.from_string(key),
  )
}

/// The cookie holds its signed issue time, so it also expires server-side
fn has_valid_session(req: Request) -> Bool {
  let issued_at =
    wisp.get_cookie(req, cookie_name, wisp.Signed)
    |> result.try(int.parse)
  case issued_at {
    Ok(issued_at) -> now() - issued_at < session_seconds
    Error(_) -> False
  }
}

/// Like wisp.set_cookie, but only marks the cookie Secure when the request
/// really came over HTTPS (directly or via a proxy). Browsers drop Secure
/// cookies on plain HTTP, which would break login on http://<lan-ip>:3000.
fn set_session_cookie(
  resp: Response,
  req: Request,
  value: String,
  max_age: Int,
) -> Response {
  let scheme = case req.scheme, request.get_header(req, "x-forwarded-proto") {
    http.Https, _ | _, Ok("https") -> http.Https
    _, _ -> http.Http
  }
  let attributes =
    cookie.Attributes(
      ..cookie.defaults(scheme),
      max_age: Some(max_age),
      same_site: Some(cookie.Strict),
    )
  let signed = wisp.sign_message(req, <<value:utf8>>, crypto.Sha512)
  response.set_cookie(resp, cookie_name, signed, attributes)
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "system_time")
fn system_time(unit: TimeUnit) -> Int

fn now() -> Int {
  system_time(Second)
}
