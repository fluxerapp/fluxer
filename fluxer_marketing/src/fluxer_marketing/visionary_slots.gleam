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

import gleam/dynamic/decode
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/json
import gleam/result
import gleam/string
import wisp

pub type VisionarySlots {
  VisionarySlots(total: Int, bought: Int, remaining: Int)
}

pub type Settings {
  Settings(api_host: String, rpc_secret: String)
}

pub opaque type Cache {
  Cache(name: process.Name(ServerMessage))
}

type ServerMessage {
  Get(process.Subject(VisionarySlots))
}

const refresh_interval_ms = 300_000

const receive_timeout_ms = 200

const log_prefix = "[visionary_slots]"

const response_snippet_limit = 256

const initial_backoff_ms = 1000

const max_backoff_ms = 30_000

pub fn start(settings: Settings) -> Cache {
  let name = process.new_name("fluxer_marketing_visionary_slots")
  process.spawn(fn() { run(name, settings) })
  Cache(name: name)
}

fn run(name: process.Name(ServerMessage), settings: Settings) {
  let _ = process.register(process.self(), name)
  let subject = process.named_subject(name)
  let initial = fetch_slots(settings) |> result.unwrap(default_slots())
  loop(subject, settings, initial)
}

fn loop(
  subject: process.Subject(ServerMessage),
  settings: Settings,
  state: VisionarySlots,
) {
  case process.receive(subject, within: refresh_interval_ms) {
    Ok(Get(reply_to)) -> {
      process.send(reply_to, state)
      loop(subject, settings, state)
    }
    Error(_) -> {
      let updated = fetch_slots(settings) |> result.unwrap(state)
      loop(subject, settings, updated)
    }
  }
}

pub fn current(cache: Cache) -> VisionarySlots {
  let subject = process.named_subject(cache.name)
  let reply_to = process.new_subject()
  process.send(subject, Get(reply_to))

  case process.receive(reply_to, within: receive_timeout_ms) {
    Ok(slots) -> slots
    Error(_) -> default_slots()
  }
}

fn fetch_slots(settings: Settings) -> Result(VisionarySlots, Nil) {
  fetch_slots_with_retry(settings, 0)
}

fn fetch_slots_with_retry(
  settings: Settings,
  attempt: Int,
) -> Result(VisionarySlots, Nil) {
  case rpc_url(settings.api_host) {
    "" -> {
      log_missing_api_host(settings.api_host)
      Error(Nil)
    }
    url -> {
      let body =
        json.object([#("type", json.string("get_visionary_slots"))])
        |> json.to_string

      let assert Ok(req) = request.to(url)
      let req =
        req
        |> request.set_method(http.Post)
        |> request.prepend_header("content-type", "application/json")
        |> request.prepend_header(
          "Authorization",
          "Bearer " <> settings.rpc_secret,
        )
        |> request.set_body(body)

      log_rpc_request(url)
      case httpc.send(req) {
        Ok(resp) if resp.status >= 200 && resp.status < 300 ->
          decode_response(resp.body, settings.api_host)
        Ok(resp) -> {
          log_rpc_status(
            settings.api_host,
            resp.status,
            response_snippet(resp.body),
          )
          retry_after_backoff(settings, attempt)
        }
        Error(error) -> {
          log_rpc_request_error(settings.api_host, string.inspect(error))
          retry_after_backoff(settings, attempt)
        }
      }
    }
  }
}

fn retry_after_backoff(
  settings: Settings,
  attempt: Int,
) -> Result(VisionarySlots, Nil) {
  let backoff_ms = calculate_backoff(attempt)
  log_retry(attempt + 1, backoff_ms)
  process.sleep(backoff_ms)
  fetch_slots_with_retry(settings, attempt + 1)
}

fn calculate_backoff(attempt: Int) -> Int {
  let multiplier = pow2(attempt)
  int.min(initial_backoff_ms * multiplier, max_backoff_ms)
}

fn pow2(n: Int) -> Int {
  case n {
    0 -> 1
    _ -> 2 * pow2(n - 1)
  }
}

fn decode_response(
  body: String,
  api_host: String,
) -> Result(VisionarySlots, Nil) {
  let slots_decoder = {
    use total <- decode.field("total", decode.int)
    use bought <- decode.field("bought", decode.int)
    use remaining <- decode.field("remaining", decode.int)
    decode.success(#(total, bought, remaining))
  }

  let response_decoder = {
    use type_ <- decode.field("type", decode.string)
    use data <- decode.field("data", slots_decoder)
    decode.success(#(type_, data))
  }

  case json.parse(body, response_decoder) {
    Ok(#("get_visionary_slots", #(total, bought, remaining))) -> {
      let normalized = normalize_slots(total, bought, remaining)
      log_rpc_success(api_host, normalized)
      Ok(normalized)
    }
    Ok(#(_, _)) -> {
      log_rpc_unexpected_payload(api_host, response_snippet(body))
      Error(Nil)
    }
    Error(error) -> {
      log_rpc_decode_error(
        api_host,
        string.inspect(error),
        response_snippet(body),
      )
      Error(Nil)
    }
  }
}

fn normalize_slots(total: Int, bought: Int, remaining: Int) -> VisionarySlots {
  let clean_total = int.max(total, 0)
  let clean_bought = bought |> int.max(0) |> int.min(clean_total)
  let computed_remaining = int.max(clean_total - clean_bought, 0)
  let clean_remaining = remaining |> int.max(0) |> int.min(clean_total)
  let final_remaining = int.max(computed_remaining, clean_remaining)

  VisionarySlots(
    total: clean_total,
    bought: clean_bought,
    remaining: final_remaining,
  )
}

fn log_missing_api_host(_api_host: String) -> Nil {
  wisp.log_warning(
    string.concat([
      log_prefix,
      " API host is missing; skipping visionary slot RPC polling.",
    ]),
  )
}

fn log_rpc_status(api_host: String, status: Int, body_snippet: String) -> Nil {
  let host = host_display(api_host)
  wisp.log_warning(
    string.concat([
      log_prefix,
      " RPC responded with status ",
      int.to_string(status),
      " for ",
      host,
      " (body: ",
      body_snippet,
      ")",
    ]),
  )
}

fn log_rpc_request_error(api_host: String, reason: String) -> Nil {
  let host = host_display(api_host)
  wisp.log_error(
    string.concat([
      log_prefix,
      " failed to reach RPC endpoint at ",
      host,
      ": ",
      reason,
    ]),
  )
}

fn log_rpc_unexpected_payload(api_host: String, body_snippet: String) -> Nil {
  let host = host_display(api_host)
  wisp.log_warning(
    string.concat([
      log_prefix,
      " RPC returned an unexpected payload for ",
      host,
      ": ",
      body_snippet,
    ]),
  )
}

fn log_rpc_decode_error(
  api_host: String,
  reason: String,
  body_snippet: String,
) -> Nil {
  let host = host_display(api_host)
  wisp.log_warning(
    string.concat([
      log_prefix,
      " failed to decode RPC response for ",
      host,
      ": ",
      reason,
      " (payload: ",
      body_snippet,
      ")",
    ]),
  )
}

fn host_display(api_host: String) -> String {
  let trimmed = string.trim(api_host)
  case trimmed {
    "" -> "(not set)"
    _ -> trimmed
  }
}

fn response_snippet(body: String) -> String {
  case string.length(body) > response_snippet_limit {
    True -> string.slice(body, 0, response_snippet_limit) <> "..."
    False -> body
  }
}

fn log_rpc_request(url: String) -> Nil {
  wisp.log_info(
    string.concat([
      log_prefix,
      " requesting visionary slots from ",
      url,
    ]),
  )
}

fn log_retry(attempt: Int, backoff_ms: Int) -> Nil {
  wisp.log_warning(
    string.concat([
      log_prefix,
      " retrying RPC request (attempt ",
      int.to_string(attempt),
      ") after ",
      int.to_string(backoff_ms),
      "ms",
    ]),
  )
}

fn log_rpc_success(api_host: String, slots: VisionarySlots) -> Nil {
  let host = host_display(api_host)
  wisp.log_info(
    string.concat([
      log_prefix,
      " fetched visionary slots from ",
      host,
      ": total=",
      int.to_string(slots.total),
      ", bought=",
      int.to_string(slots.bought),
      ", remaining=",
      int.to_string(slots.remaining),
    ]),
  )
}

fn rpc_url(api_host: String) -> String {
  let host = string.trim(api_host)
  case host {
    "" -> ""
    _ -> {
      let base = case string.contains(host, "://") {
        True -> host
        False -> "http://" <> host
      }

      let normalized = case string.ends_with(base, "/") {
        True -> string.slice(base, 0, string.length(base) - 1)
        False -> base
      }

      normalized <> "/_rpc"
    }
  }
}

fn default_slots() -> VisionarySlots {
  VisionarySlots(total: 0, bought: 0, remaining: 0)
}
