import config.{type Config, type Credentials, Credentials}
import gleam/bit_array
import gleam/crypto
import gleam/erlang/process
import gleam/http
import gleam/http/cookie
import gleam/http/request
import gleam/http/response
import gleam/int
import gleam/json
import gleam/option.{None, Some}
import gleam/result
import wisp.{type Request, type Response}

const cookie_name = "prograrr_session"

const session_seconds = 2_592_000

/// What the browser submits to log in
pub type LoginForm {
  ApiKeyLogin(api_key: String)
  PasswordLogin(username: String, password: String)
}

/// Secret used by wisp to sign cookies. Derived from the API key and login
/// credentials so that changing any of them invalidates every browser session.
pub fn secret_key_base(cfg: Config) -> String {
  case cfg.api_key {
    Some(key) -> {
      let login = case cfg.login {
        Some(Credentials(username:, password:)) -> [username, password]
        None -> []
      }
      crypto.hash(
        crypto.Sha512,
        bit_array.from_string("prograrr-session")
          |> append_fields([key, ..login]),
      )
      |> bit_array.base16_encode
    }
    None -> wisp.random_string(64)
  }
}

/// Join values with NUL separators so different splits can't hash the same
fn append_fields(acc: BitArray, values: List(String)) -> BitArray {
  case values {
    [] -> acc
    [value, ..rest] -> append_fields(<<acc:bits, 0, value:utf8>>, rest)
  }
}

/// A request is authorized by a valid X-Api-Key header (API clients) or a
/// valid session cookie (browsers). Always true when auth is disabled.
pub fn is_authorized(req: Request, cfg: Config) -> Bool {
  case cfg.api_key {
    None -> True
    Some(key) ->
      case request.get_header(req, "x-api-key") {
        Ok(provided) -> secret_matches(provided, key)
        Error(_) -> has_valid_session(req)
      }
  }
}

/// Check the submitted login against the config. When a username and password
/// are configured the browser must use them, otherwise it uses the API key.
pub fn check_login(cfg: Config, form: LoginForm) -> Bool {
  case cfg.api_key, cfg.login, form {
    Some(_), Some(expected), PasswordLogin(username:, password:) ->
      credentials_match(expected, username, password)
    Some(key), None, ApiKeyLogin(api_key:) -> secret_matches(api_key, key)
    _, _, _ -> False
  }
}

/// Start a browser session
pub fn login(req: Request) -> Response {
  wisp.no_content()
  |> set_session_cookie(req, int.to_string(now()), session_seconds)
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

/// Both fields are always compared, so a wrong username takes as long as a
/// wrong password and doesn't reveal which one was wrong
fn credentials_match(
  expected: Credentials,
  username: String,
  password: String,
) -> Bool {
  let username_ok = secret_matches(username, expected.username)
  let password_ok = secret_matches(password, expected.password)
  username_ok && password_ok
}

/// Constant-time comparison. Both sides are hashed first so the comparison
/// doesn't leak the length of the secret either.
fn secret_matches(provided: String, secret: String) -> Bool {
  provided != ""
  && crypto.secure_compare(
    crypto.hash(crypto.Sha256, <<provided:utf8>>),
    crypto.hash(crypto.Sha256, <<secret:utf8>>),
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
