// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/client as api
import fluxer_gateway/config/settings.{Settings}
import fluxer_gateway/core/clock
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleam/string
import support/api_breaker
import support/gateway_api_stub as stub
import support/gateway_harness as h

type Atomics

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(Nil)) -> Atomics

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

fn with_stub(respond: stub.Responder, body: fn(stub.Stub) -> a) -> a {
  let assert True = api_breaker.reset()
  let api_stub = stub.start(respond)
  let outcome = body(api_stub)
  stub.stop(api_stub)
  outcome
}

pub fn server_errors_are_retried_until_success_test_() {
  use <- h.within(30.0)
  let attempts = atomics_new(1, [])
  use api_stub <- with_stub(fn(_kind, _body) {
    case atomics_add_get(attempts, 1, 1) {
      1 -> #(503, "busy")
      _ -> #(200, "{\"data\":{\"ok\":true}}")
    }
  })
  let assert Ok(data) =
    api.call_with_retry("probe", [], clock.deadline_in(5000))
  assert decode.run(data, decode.at(["ok"], decode.bool)) == Ok(True)
  assert list.length(stub.calls(api_stub, "probe")) == 2
}

pub fn unauthorised_is_not_retried_test_() {
  use <- h.within(30.0)
  use api_stub <- with_stub(fn(_kind, _body) { #(401, "{}") })
  assert api.call_with_retry("probe", [], clock.deadline_in(5000))
    == Error(api.ApiHttp(401))
  assert list.length(stub.calls(api_stub, "probe")) == 1
}

pub fn client_errors_carry_the_body_test_() {
  use <- h.within(30.0)
  use api_stub <- with_stub(fn(_kind, _body) { #(403, "{\"code\":\"NOPE\"}") })
  assert api.call_with_retry("probe", [], clock.deadline_in(5000))
    == Error(api.ApiRejected(403, "{\"code\":\"NOPE\"}"))
  assert list.length(stub.calls(api_stub, "probe")) == 1
}

pub fn malformed_reply_is_bad_reply_test_() {
  use <- h.within(30.0)
  use _api_stub <- with_stub(fn(_kind, _body) { #(200, "not json") })
  assert api.call("probe", [], clock.deadline_in(5000))
    == Error(api.ApiBadReply)
}

pub fn slow_api_times_out_within_the_deadline_test_() {
  use <- h.within(30.0)
  use _api_stub <- with_stub(fn(_kind, _body) {
    process.sleep(2000)
    #(200, "{\"data\":{}}")
  })
  let started = clock.monotonic_ms()
  assert api.call_with_retry("probe", [], clock.deadline_in(400))
    == Error(api.ApiTimeout)
  assert clock.monotonic_ms() - started < 1500
}

pub fn request_carries_type_fields_auth_and_client_ip_test_() {
  use <- h.within(30.0)
  use api_stub <- with_stub(fn(_kind, _body) { #(200, "{\"data\":{}}") })
  settings.store(Settings(..settings.get(), rpc_auth_token: "sekret"))
  let assert Ok(_) =
    api.call(
      "probe",
      [#("ip", json.string("10.1.2.3")), #("n", json.int(7))],
      clock.deadline_in(5000),
    )
  let assert [call] = stub.received(api_stub, "probe")
  let head = string.lowercase(call.head)
  assert string.starts_with(call.head, "POST /internal/rpc HTTP/1.1")
  assert string.contains(head, "x-fluxer-rpc-auth: sekret")
  assert string.contains(head, "x-forwarded-for: 10.1.2.3")
  assert string.contains(head, "content-type: application/json")
  assert json.parse(call.body, {
      use kind <- decode.field("type", decode.string)
      use n <- decode.field("n", decode.int)
      decode.success(#(kind, n))
    })
    == Ok(#("probe", 7))
}

pub fn rpc_endpoint_overrides_the_internal_url_test_() {
  use <- h.within(30.0)
  use api_stub <- with_stub(fn(_kind, _body) { #(200, "{\"data\":{}}") })
  settings.store(
    Settings(
      ..settings.get(),
      api_internal_url: "http://127.0.0.1:1",
      api_rpc_endpoint: Some(
        " http://127.0.0.1:" <> int.to_string(api_stub.port) <> "/custom/ ",
      ),
    ),
  )
  let assert Ok(_) = api.call("probe", [], clock.deadline_in(5000))
  let assert [call] = stub.received(api_stub, "probe")
  assert string.starts_with(call.head, "POST /custom HTTP/1.1")
}

fn calls_until(deadline_ms: Int, check: fn() -> Bool) -> Bool {
  case check(), deadline_ms <= 0 {
    True, _ -> True
    False, True -> False
    False, False -> {
      process.sleep(100)
      calls_until(deadline_ms - 100, check)
    }
  }
}

pub fn breaker_opens_on_failures_and_recovers_through_probes_test_() {
  use <- h.within(60.0)
  let healthy = atomics_new(1, [])
  use api_stub <- with_stub(fn(_kind, _body) {
    case atomics_add_get(healthy, 1, 0) {
      0 -> #(500, "down")
      _ -> #(200, "{\"data\":{}}")
    }
  })
  api_breaker.hold_background_callers()
  list.repeat(Nil, 20)
  |> list.each(fn(_) {
    let _ = api.call("probe", [], clock.deadline_in(2000))
    Nil
  })
  assert calls_until(2000, api.is_open)
  let before = list.length(stub.calls(api_stub, "probe"))
  assert before >= 1 && before <= 20
  assert api.call("probe", [], clock.deadline_in(2000))
    == Error(api.ApiUnavailable)
  assert stub.calls(api_stub, "probe") == []
  let _ = atomics_add_get(healthy, 1, 1)
  assert calls_until(25_000, fn() { !api.is_open() })
  list.repeat(Nil, 4)
  |> list.each(fn(_) {
    let assert Ok(_) = api.call("probe", [], clock.deadline_in(2000))
    process.sleep(50)
  })
  process.sleep(100)
  list.repeat(Nil, 10)
  |> list.each(fn(_) {
    let assert Ok(_) = api.call("probe", [], clock.deadline_in(2000))
    Nil
  })
}
