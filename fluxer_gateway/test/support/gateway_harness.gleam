// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/http as app_http
import fluxer_gateway/config/settings.{Settings}
import fluxer_gateway/gateway/ip_connections
import fluxer_gateway/session/manager/registry
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/list
import gleam/option
import gleam/otp/static_supervisor
import glisten
import support/api_breaker
import support/gateway_api_stub as stub
import support/gateway_ws_client as ws
import support/session_harness as sh
import support/wait

pub type Timeout {
  Timeout(seconds: Float, test_fn: fn() -> Nil)
}

pub fn within(seconds: Float, test_fn: fn() -> Nil) -> Timeout {
  Timeout(seconds, test_fn)
}

pub type Gateway {
  Gateway(port: Int, supervisor: Pid)
}

pub fn start_gateway() -> Gateway {
  let name = process.new_name("gateway_harness_listener")
  let assert Ok(started) =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(app_http.listening(
      Settings(..settings.get(), port: 0),
      name,
    ))
    |> static_supervisor.start
  process.unlink(started.pid)
  let port = glisten.get_server_info(name, 5000).port
  Gateway(port:, supervisor: started.pid)
}

pub fn stop_gateway(gateway: Gateway) -> Nil {
  process.kill(gateway.supervisor)
}

pub type Scenario {
  Scenario(gateway: Gateway, api: stub.Stub)
}

@external(erlang, "ets", "whereis")
fn ets_whereis(table: Atom) -> Dynamic

@external(erlang, "ets", "match_delete")
fn ets_match_delete(table: Atom, pattern: #(#(String, Atom), Atom)) -> Bool

fn forget_loopback_budgets() -> Nil {
  let any = atom.create("_")
  ["gateway_shared_ip_rate", "gateway_identify_rate"]
  |> list.each(fn(name) {
    let table = atom.create(name)
    case decode.run(ets_whereis(table), atom.decoder()) {
      Ok(_) -> Nil
      Error(_) -> {
        ets_match_delete(table, #(#("127.0.0.1", any), any))
        Nil
      }
    }
  })
}

pub fn run(respond: stub.Responder, body: fn(Scenario) -> a) -> a {
  let assert True = api_breaker.reset()
  ip_connections.reclaim_dead_owners()
  forget_loopback_budgets()
  let api = stub.start(respond)
  let gateway = start_gateway()
  let outcome = body(Scenario(gateway:, api:))
  stop_gateway(gateway)
  stub.stop(api)
  outcome
}

pub fn sessions_for(user_id: String) -> stub.Responder {
  fn(kind, _body) {
    case kind {
      "session" -> #(200, stub.session_reply(user_id))
      _ -> #(200, "{\"data\":{}}")
    }
  }
}

pub fn identify_payload(token: String) -> json.Json {
  json.object([
    #("op", json.int(2)),
    #(
      "d",
      json.object([
        #("token", json.string(token)),
        #(
          "properties",
          json.object([
            #("os", json.string("linux")),
            #("browser", json.string("scenario")),
            #("device", json.string("scenario")),
          ]),
        ),
      ]),
    ),
  ])
}

pub fn heartbeat_payload(seq: json.Json) -> json.Json {
  json.object([#("op", json.int(1)), #("d", seq)])
}

pub fn resume_payload(
  token: String,
  session_id: String,
  seq: Int,
) -> json.Json {
  json.object([
    #("op", json.int(6)),
    #(
      "d",
      json.object([
        #("token", json.string(token)),
        #("session_id", json.string(session_id)),
        #("seq", json.int(seq)),
      ]),
    ),
  ])
}

pub fn hello(port: Int) -> ws.Client {
  let client = ws.connect(port, "?v=1&encoding=json")
  let #(client, payload) = ws.expect_payload(client, 2000)
  let assert 10 = payload.op
  client
}

pub fn session_id(ready: ws.Payload) -> String {
  let assert Ok(id) =
    decode.run(ready.d, decode.at(["session_id"], decode.string))
  id
}

pub fn trust_forwarded_for() -> Nil {
  settings.store(Settings(..settings.get(), trust_client_ip_header: True))
  Nil
}

pub fn hello_from(port: Int, query: String, address: String) -> ws.Client {
  let client = ws.connect_with(port, query, [#("X-Forwarded-For", address)])
  let #(client, payload) = ws.expect_payload(client, 2000)
  let assert 10 = payload.op
  client
}

pub fn await_detached(session_id: String) -> Bool {
  wait.until(5000, fn() {
    case registry.lookup(session_id) {
      Ok(ref) -> sh.is_detached(ref)
      Error(Nil) -> False
    }
  })
}

pub fn await_gone(session_id: String) -> Bool {
  wait.until(5000, fn() { registry.lookup(session_id) == Error(Nil) })
}

pub fn ready_count(client: ws.Client, quiet_ms: Int) -> Int {
  count_ready(client, quiet_ms, 0)
}

fn count_ready(client: ws.Client, quiet_ms: Int, seen: Int) -> Int {
  case ws.next(client, quiet_ms) {
    #(client, ws.Text(text)) ->
      case ws.decode_payload(text).t {
        option.Some("READY") -> count_ready(client, quiet_ms, seen + 1)
        _ -> count_ready(client, quiet_ms, seen)
      }
    #(client, ws.Ping(_)) | #(client, ws.Pong(_)) ->
      count_ready(client, quiet_ms, seen)
    #(_, _) -> seen
  }
}
