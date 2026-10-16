import gleam/dynamic/decode
import gleam/http as gleam_http
import gleam/http/request
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option}
import gleam/result
import gleam/string

/// Error returned by every service client
pub type ServiceError {
  RequestFailed(String)
  ParseError(String)
  AuthError(String)
}

pub fn error_message(err: ServiceError) -> String {
  case err {
    RequestFailed(msg) -> "HTTP error: " <> msg
    ParseError(msg) -> "Parse error: " <> msg
    AuthError(msg) -> "Auth error: " <> msg
  }
}

pub type HttpResponse {
  HttpResponse(body: String, cookie: Option(String))
}

/// GET a URL and decode its JSON body. `what` names the payload in parse errors.
pub fn get_json(
  url: String,
  headers: List(#(String, String)),
  decoder: decode.Decoder(a),
  what: String,
) -> Result(a, ServiceError) {
  use body <- result.try(get(url, headers))
  json.parse(body, decoder)
  |> result.replace_error(ParseError("Failed to parse " <> what))
}

/// Make an HTTP GET request
pub fn get(
  url: String,
  headers: List(#(String, String)),
) -> Result(String, ServiceError) {
  use req <- result.try(new_request(url))
  use resp <- result.try(send(add_headers(req, headers), url))
  case resp.status {
    200 -> Ok(resp.body)
    status -> Error(status_error(status, url))
  }
}

/// Make an HTTP POST request and return body + set-cookie header
pub fn post_with_cookie(
  url: String,
  body: String,
  headers: List(#(String, String)),
) -> Result(HttpResponse, ServiceError) {
  use req <- result.try(new_request(url))
  let req =
    req
    |> request.set_method(gleam_http.Post)
    |> request.set_body(body)
    |> add_headers(headers)
  use resp <- result.try(send(req, url))
  case resp.status >= 200 && resp.status < 300 {
    True -> Ok(HttpResponse(body: resp.body, cookie: extract_cookie(resp)))
    False -> Error(status_error(resp.status, url))
  }
}

fn new_request(url: String) -> Result(request.Request(String), ServiceError) {
  request.to(url)
  |> result.replace_error(RequestFailed("Invalid URL: " <> sanitize_url(url)))
}

fn send(
  req: request.Request(String),
  url: String,
) -> Result(Response(String), ServiceError) {
  httpc.send(req)
  |> result.replace_error(RequestFailed(
    "HTTP request failed to " <> sanitize_url(url),
  ))
}

fn status_error(status: Int, url: String) -> ServiceError {
  RequestFailed("HTTP " <> int.to_string(status) <> " from " <> sanitize_url(url))
}

/// Strip query parameters from URLs to avoid leaking secrets (e.g. SABnzbd API key)
fn sanitize_url(url: String) -> String {
  case string.split_once(url, "?") {
    Ok(#(base, _)) -> base
    Error(_) -> url
  }
}

fn add_headers(
  req: request.Request(String),
  headers: List(#(String, String)),
) -> request.Request(String) {
  list.fold(headers, req, fn(r, header) {
    request.set_header(r, header.0, header.1)
  })
}

fn extract_cookie(resp: Response(String)) -> Option(String) {
  resp.headers
  |> list.find(fn(h) { string.lowercase(h.0) == "set-cookie" })
  |> option.from_result
  |> option.map(fn(h) { h.1 })
}
