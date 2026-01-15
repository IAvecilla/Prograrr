import gleam/http/request
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/http as gleam_http
import gleam/list
import gleam/option.{type Option}
import gleam/string

pub type HttpError {
  RequestFailed(String)
  InvalidResponse(String)
}

pub type HttpResponse {
  HttpResponse(body: String, cookie: Option(String))
}

/// Get the error message from an HttpError
pub fn error_message(err: HttpError) -> String {
  case err {
    RequestFailed(msg) -> msg
    InvalidResponse(msg) -> msg
  }
}

/// Make an HTTP GET request
pub fn get(url: String, headers: List(#(String, String))) -> Result(String, HttpError) {
  case request.to(url) {
    Ok(req) -> {
      let req_with_headers = add_headers(req, headers)

      case httpc.send(req_with_headers) {
        Ok(resp) -> {
          case resp.status {
            200 -> Ok(resp.body)
            status -> Error(RequestFailed("HTTP " <> int.to_string(status) <> " from " <> url))
          }
        }
        Error(_) -> Error(RequestFailed("HTTP request failed to " <> url))
      }
    }
    Error(_) -> Error(InvalidResponse("Invalid URL: " <> url))
  }
}

/// Make an HTTP GET request with a cookie
pub fn get_with_cookie(url: String, headers: List(#(String, String)), cookie: String) -> Result(String, HttpError) {
  let headers_with_cookie = [#("Cookie", cookie), ..headers]
  get(url, headers_with_cookie)
}

/// Make an HTTP POST request and return body + set-cookie header
pub fn post_with_cookie(url: String, body: String, headers: List(#(String, String))) -> Result(HttpResponse, HttpError) {
  case request.to(url) {
    Ok(req) -> {
      let req_with_body =
        req
        |> request.set_method(gleam_http.Post)
        |> request.set_body(body)

      let req_with_headers = add_headers(req_with_body, headers)

      case httpc.send(req_with_headers) {
        Ok(resp) -> {
          case resp.status >= 200 && resp.status < 300 {
            True -> {
              let cookie = extract_cookie(resp)
              Ok(HttpResponse(body: resp.body, cookie: cookie))
            }
            False -> Error(RequestFailed("HTTP " <> int.to_string(resp.status) <> " from " <> url))
          }
        }
        Error(_) -> Error(RequestFailed("HTTP request failed to " <> url))
      }
    }
    Error(_) -> Error(InvalidResponse("Invalid URL: " <> url))
  }
}

/// Make an HTTP POST request (simple version)
pub fn post(url: String, body: String, headers: List(#(String, String))) -> Result(String, HttpError) {
  case post_with_cookie(url, body, headers) {
    Ok(resp) -> Ok(resp.body)
    Error(e) -> Error(e)
  }
}

fn add_headers(req: request.Request(String), headers: List(#(String, String))) -> request.Request(String) {
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
