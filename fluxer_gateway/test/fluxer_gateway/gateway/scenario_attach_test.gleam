// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SocketRef}
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/state
import gleam/bit_array
import gleam/erlang/process
import gleam/json
import gleam/option.{Some}
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/session_harness as sh

fn stray_id() -> session.SessionId {
  session.new_session_id()
}

fn ready_with_socket(
  port: Int,
  token: String,
) -> #(ws.Client, ws.Payload, SocketRef) {
  let client = h.hello(port)
  ws.send_json(client, h.identify_payload(token))
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let assert state.Attached(socket, _) = sh.state_of(ref).socket
  #(client, ready, socket)
}

fn stray_ready(socket: SocketRef, id: session.SessionId) -> Nil {
  process.send(
    socket.outbox,
    session.Send(session: id, frames: [
      session.DispatchFrame(
        seq: 1,
        event: "READY",
        payload: raw_json.wrap(bit_array.from_string(
          "{\"session_id\":\"" <> session.session_id_to_string(id) <> "\"}",
        )),
      ),
    ]),
  )
}

pub fn frames_from_a_session_never_attached_are_not_sent_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000831"))
  let #(client, first, socket) =
    ready_with_socket(scenario.gateway.port, "t-a831")
  let assert Ok(ref) = registry.lookup(h.session_id(first))
  process.kill(sh.session_pid(ref))
  let #(client, invalid) = ws.wait_for(client, 5000, fn(p) { p.op == 9 })
  assert invalid.op == 9
  let stray = stray_id()
  stray_ready(socket, stray)
  process.send(
    socket.outbox,
    session.Close(session: stray, code: error.AuthenticationFailed, reason: ""),
  )
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 5000)
  assert ack.op == 11
  ws.send_json(client, h.identify_payload("t-a831"))
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  assert h.session_id(ready) != session.session_id_to_string(stray)
  assert registry.lookup(h.session_id(ready)) |> is_ok
  assert h.ready_count(client, 500) == 0
}

pub fn stray_frames_while_starting_do_not_reach_the_client_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(fn(kind, _body) {
    case kind {
      "session" -> {
        process.sleep(600)
        #(200, stub.session_reply("1000000000000832"))
      }
      _ -> #(200, "{\"data\":{}}")
    }
  })
  let #(client, first, socket) =
    ready_with_socket(scenario.gateway.port, "t-a832")
  let assert Ok(ref) = registry.lookup(h.session_id(first))
  process.kill(sh.session_pid(ref))
  let #(client, _) = ws.wait_for(client, 5000, fn(p) { p.op == 9 })
  ws.send_json(client, h.identify_payload("t-a832"))
  process.sleep(100)
  stray_ready(socket, stray_id())
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  assert registry.lookup(h.session_id(ready)) |> is_ok
  assert h.ready_count(client, 500) == 0
}

fn is_ok(result: Result(a, b)) -> Bool {
  case result {
    Ok(_) -> True
    Error(_) -> False
  }
}
