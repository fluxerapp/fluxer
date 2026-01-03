//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

import gleam/erlang/process
import gleam/http/request
import gleam/httpc
import gleam/option.{type Option}
import wisp.{type Response}

const product_hunt_url = "https://api.producthunt.com/widgets/embed-image/v1/featured.svg?post_id=1057558&theme=light"

const stale_after_ms = 300_000

const receive_timeout_ms = 5000

const fetch_timeout_ms = 4500

pub opaque type Cache {
  Cache(subject: process.Subject(ServerMessage))
}

type ServerMessage {
  Get(process.Subject(Option(String)))
  RefreshDone(fetched_at: Int, svg: Option(String))
}

type CacheEntry {
  CacheEntry(svg: String, fetched_at: Int)
}

type State {
  State(cache: Option(CacheEntry), is_refreshing: Bool)
}

pub fn start_cache() -> Cache {
  let started = process.new_subject()
  let _ = process.spawn_unlinked(fn() { run(started) })

  let assert Ok(subject) = process.receive(started, within: 1000)
  Cache(subject: subject)
}

fn run(started: process.Subject(process.Subject(ServerMessage))) {
  let subject = process.new_subject()
  process.send(started, subject)

  let initial = State(cache: option.None, is_refreshing: False)
  loop(subject, initial)
}

fn loop(subject: process.Subject(ServerMessage), state: State) {
  let new_state = case process.receive(subject, within: stale_after_ms) {
    Ok(Get(reply_to)) -> handle_get(subject, reply_to, state)

    Ok(RefreshDone(fetched_at, svg)) ->
      handle_refresh_done(fetched_at, svg, state)

    Error(_) -> maybe_refresh_in_background(subject, state)
  }

  loop(subject, new_state)
}

fn handle_get(
  subject: process.Subject(ServerMessage),
  reply_to: process.Subject(Option(String)),
  state: State,
) -> State {
  let now = monotonic_time_ms()

  case state.cache {
    option.None -> {
      let svg = fetch_badge_svg()
      process.send(reply_to, svg)

      let new_cache = case svg {
        option.Some(content) ->
          option.Some(CacheEntry(svg: content, fetched_at: now))
        option.None -> option.None
      }

      State(cache: new_cache, is_refreshing: False)
    }

    option.Some(entry) -> {
      let is_stale = now - entry.fetched_at > stale_after_ms

      process.send(reply_to, option.Some(entry.svg))

      case is_stale && !state.is_refreshing {
        True -> {
          spawn_refresh(subject)
          State(..state, is_refreshing: True)
        }
        False -> state
      }
    }
  }
}

fn handle_refresh_done(
  fetched_at: Int,
  svg: Option(String),
  state: State,
) -> State {
  let new_cache = case svg {
    option.Some(content) ->
      option.Some(CacheEntry(svg: content, fetched_at: fetched_at))
    option.None -> state.cache
  }

  State(cache: new_cache, is_refreshing: False)
}

fn maybe_refresh_in_background(
  subject: process.Subject(ServerMessage),
  state: State,
) -> State {
  let now = monotonic_time_ms()

  case state.cache, state.is_refreshing {
    option.Some(entry), False if now - entry.fetched_at > stale_after_ms -> {
      spawn_refresh(subject)
      State(..state, is_refreshing: True)
    }

    _, _ -> state
  }
}

fn spawn_refresh(subject: process.Subject(ServerMessage)) {
  let _ =
    process.spawn_unlinked(fn() {
      let fetched_at = monotonic_time_ms()
      let svg = fetch_badge_svg()
      process.send(subject, RefreshDone(fetched_at, svg))
    })

  Nil
}

pub fn get_badge(cache: Cache) -> Option(String) {
  let reply_to = process.new_subject()
  process.send(cache.subject, Get(reply_to))

  case process.receive(reply_to, within: receive_timeout_ms) {
    Ok(svg) -> svg
    Error(_) -> option.None
  }
}

pub fn product_hunt(cache: Cache) -> Response {
  case get_badge(cache) {
    option.Some(content) -> {
      wisp.response(200)
      |> wisp.set_header("content-type", "image/svg+xml")
      |> wisp.set_header(
        "cache-control",
        "public, max-age=300, stale-while-revalidate=600",
      )
      |> wisp.set_header("vary", "Accept")
      |> wisp.string_body(content)
    }

    option.None -> {
      wisp.response(503)
      |> wisp.set_header("content-type", "text/plain")
      |> wisp.set_header("retry-after", "60")
      |> wisp.string_body("Badge temporarily unavailable")
    }
  }
}

fn fetch_badge_svg() -> Option(String) {
  let assert Ok(req0) = request.to(product_hunt_url)
  let req =
    req0
    |> request.prepend_header("accept", "image/svg+xml")
    |> request.prepend_header("user-agent", "FluxerMarketing/1.0")

  let config =
    httpc.configure()
    |> httpc.timeout(fetch_timeout_ms)

  case httpc.dispatch(config, req) {
    Ok(resp) if resp.status >= 200 && resp.status < 300 ->
      option.Some(resp.body)
    _ -> option.None
  }
}

type TimeUnit {
  Millisecond
}

@external(erlang, "erlang", "monotonic_time")
fn erlang_monotonic_time(unit: TimeUnit) -> Int

fn monotonic_time_ms() -> Int {
  erlang_monotonic_time(Millisecond)
}
