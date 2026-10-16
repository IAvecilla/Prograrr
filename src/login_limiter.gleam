import gleam/dict.{type Dict}
import gleam/erlang/process.{type Subject}
import gleam/otp/actor
import gleam/result

const max_attempts = 5

const window_seconds = 900

/// Limits login attempts per client IP: after `max_attempts` within
/// `window_seconds`, further attempts are rejected until the window ends.
pub opaque type Limiter {
  Limiter(subject: Subject(Message))
}

type Message {
  Attempt(ip: String, reply: Subject(Bool))
  Reset(ip: String)
}

/// Attempts made since `since`, in seconds
type Entry {
  Entry(attempts: Int, since: Int)
}

pub fn start() -> Result(Limiter, actor.StartError) {
  actor.new(dict.new())
  |> actor.on_message(handle_message)
  |> actor.start
  |> result.map(fn(started) { Limiter(started.data) })
}

/// Record a login attempt from `ip`, returning False when it's locked out.
/// Every attempt counts until `reset` is called after a successful login, so
/// parallel requests can't slip past the limit.
pub fn attempt(limiter: Limiter, ip: String) -> Bool {
  process.call(limiter.subject, 1000, Attempt(ip, _))
}

pub fn reset(limiter: Limiter, ip: String) -> Nil {
  process.send(limiter.subject, Reset(ip))
}

fn handle_message(
  state: Dict(String, Entry),
  message: Message,
) -> actor.Next(Dict(String, Entry), Message) {
  let now = now()
  let state =
    dict.filter(state, fn(_, entry) { now - entry.since < window_seconds })
  case message {
    Attempt(ip, reply) -> {
      let entry = dict.get(state, ip) |> result.unwrap(Entry(0, now))
      let allowed = entry.attempts < max_attempts
      process.send(reply, allowed)
      case allowed {
        True ->
          actor.continue(dict.insert(
            state,
            ip,
            Entry(..entry, attempts: entry.attempts + 1),
          ))
        False -> actor.continue(state)
      }
    }
    Reset(ip) -> actor.continue(dict.delete(state, ip))
  }
}

type TimeUnit {
  Second
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic_time(unit: TimeUnit) -> Int

fn now() -> Int {
  monotonic_time(Second)
}
