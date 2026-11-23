// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/result
import support/api_breaker
import support/fake_http

pub type Call {
  Call(kind: String, body: Dynamic)
}

pub type Reply {
  Reply(status: Int, body: String)
}

pub opaque type Stub {
  Stub(route: fake_http.Route, previous: settings.Settings)
}

pub fn data(payload: json.Json) -> Reply {
  Reply(200, json.to_string(json.object([#("data", payload)])))
}

pub fn only(kind: String, handler: fn(Call) -> Reply) -> fn(Call) -> Reply {
  fn(call: Call) {
    case call.kind == kind {
      True -> handler(call)
      False -> data(json.object([]))
    }
  }
}

pub fn start(handler: fn(Call) -> Reply) -> Stub {
  let route =
    fake_http.route(fn(request) {
      let reply = handler(decode_call(request.body))
      fake_http.Answer(reply.status, reply.body)
    })
  let previous = settings.get()
  settings.store(
    settings.Settings(
      ..previous,
      api_rpc_endpoint: Some(fake_http.url(route) <> "/"),
    ),
  )
  let _ = api_breaker.reset()
  Stub(route:, previous:)
}

pub fn stop(stub: Stub) -> Nil {
  settings.store(stub.previous)
  fake_http.unroute(stub.route)
}

fn decode_call(body: BitArray) -> Call {
  let parsed = json.parse_bits(body, decode.dynamic)
  let kind =
    parsed
    |> result.replace_error(Nil)
    |> result.try(fn(found) {
      decode.run(found, decode.at(["type"], decode.string))
      |> result.replace_error(Nil)
    })
    |> result.unwrap("")
  Call(kind:, body: result.unwrap(parsed, dynamic.nil()))
}

pub fn user_ids(call: Call) -> List(Int) {
  decode.run(call.body, decode.at(["user_ids"], decode.list(decode.string)))
  |> result.unwrap([])
  |> list.filter_map(int.parse)
}
