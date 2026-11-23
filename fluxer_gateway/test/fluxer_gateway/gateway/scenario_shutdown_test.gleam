// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/gateway/ip_connections
import fluxer_gateway/gateway/socket
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/wait

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: String) -> List(#(String, Int))

@external(erlang, "supervisor", "which_children")
fn which_children(supervisor: Pid) -> List(#(Int, Dynamic, Atom, Dynamic))

@external(erlang, "supervisor", "terminate_child")
fn terminate_child(supervisor: Pid, id: Int) -> Dynamic

fn open_connections(address: String) -> Int {
  case ets_lookup(atom.create("gateway_ip_connections"), address) {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn ready(client: ws.Client, token: String) -> #(ws.Client, ws.Payload) {
  ws.send_json(client, h.identify_payload(token))
  ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
}

fn expect_reconnect_close(client: ws.Client) -> Nil {
  let #(client, reconnect) = ws.wait_for(client, 5000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let #(code, reason) = ws.expect_close(client, 5000)
  assert code == Some(4000)
  assert reason == "Session drain requested; reconnect to continue"
  assert ws.expect_eof(client, 5000)
}

pub fn drain_reconnects_unidentified_identifying_and_ready_sockets_test_() {
  use <- h.within(60.0)
  h.trust_forwarded_for()
  let address = "192.0.2.41"
  use scenario <- h.run(h.sessions_for("1000000000000801"))
  let port = scenario.gateway.port
  let query = "?v=1&encoding=json"
  let fresh = h.hello_from(port, query, address)
  let #(attached, first) = ready(h.hello_from(port, query, address), "t-d801")
  let assert Some(seq) = first.s
  draining.set_draining()
  let held = h.hello_from(port, query, address)
  ws.send_json(held, h.identify_payload("t-d801-held"))
  expect_reconnect_close(held)
  assert wait.until(5000, fn() { open_connections(address) == 2 })
  assert socket.reconnect_all() >= 2
  expect_reconnect_close(fresh)
  expect_reconnect_close(attached)
  assert wait.until(5000, fn() { open_connections(address) == 0 })
  assert stub.token_calls(scenario.api, "session", "t-d801-held") == []
  assert h.await_detached(h.session_id(first))
  let again = h.hello_from(port, query, address)
  ws.send_json(again, h.resume_payload("t-d801", h.session_id(first), seq))
  let #(_again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
}

pub fn zstd_drain_sends_a_compressed_reconnect_and_a_plain_close_test_() {
  use <- h.within(30.0)
  use scenario <- h.run(h.sessions_for("1000000000000802"))
  let client =
    ws.connect(
      scenario.gateway.port,
      "?v=1&encoding=json&compress=zstd-stream&stream=1",
    )
  let assert #(client, ws.BinaryFrame(_hello)) = ws.next(client, 5000)
  assert socket.reconnect_all() >= 1
  let assert #(client, ws.BinaryFrame(_reconnect)) = ws.next(client, 5000)
  let #(code, reason) = ws.expect_close(client, 5000)
  assert code == Some(4000)
  assert reason == "Session drain requested; reconnect to continue"
}

pub fn listener_shutdown_detaches_sessions_for_resume_elsewhere_test_() {
  use <- h.within(60.0)
  use scenario <- h.run(h.sessions_for("1000000000000803"))
  ip_connections.reclaim_dead_owners()
  let before = open_connections("127.0.0.1")
  let #(client, first) = ready(h.hello(scenario.gateway.port), "t-d803")
  let assert Some(seq) = first.s
  let id = h.session_id(first)
  let assert [#(child, _, _, _), ..] =
    which_children(scenario.gateway.supervisor)
  let _ = terminate_child(scenario.gateway.supervisor, child)
  assert ws.expect_eof(client, 5000)
  assert h.await_detached(id)
  ip_connections.reclaim_dead_owners()
  assert wait.until(5000, fn() { open_connections("127.0.0.1") == before })
  let other = h.start_gateway()
  let again = h.hello(other.port)
  ws.send_json(again, h.resume_payload("t-d803", id, seq))
  let #(again, resumed) =
    ws.wait_for(again, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  ws.send_json(again, h.heartbeat_payload(json.int(seq)))
  let #(_again, ack) = ws.wait_for(again, 5000, fn(p) { p.op == 11 })
  assert ack.op == 11
  h.stop_gateway(other)
}

pub fn resume_and_identify_during_drain_test_() {
  use <- h.within(60.0)
  use scenario <- h.run(h.sessions_for("1000000000000804"))
  let #(client, first) = ready(h.hello(scenario.gateway.port), "t-d804")
  let assert Some(seq) = first.s
  ws.close_socket(client)
  assert h.await_detached(h.session_id(first))
  draining.set_draining()
  let identify = h.hello(scenario.gateway.port)
  ws.send_json(identify, h.identify_payload("t-d804-new"))
  let resume = h.hello(scenario.gateway.port)
  ws.send_json(resume, h.resume_payload("t-d804", h.session_id(first), seq))
  let #(_resume, resumed) =
    ws.wait_for(resume, 5000, fn(p) { p.t == Some("RESUMED") })
  assert resumed.op == 0
  expect_reconnect_close(identify)
  assert stub.token_calls(scenario.api, "session", "t-d804-new") == []
  draining.clear_draining()
  let again = h.hello(scenario.gateway.port)
  ws.send_json(again, h.identify_payload("t-d804-new"))
  let #(_again, fresh) =
    ws.wait_for(again, 8000, fn(p) { p.t == Some("READY") })
  assert h.session_id(fresh) != h.session_id(first)
  assert list.length(stub.token_calls(scenario.api, "session", "t-d804-new"))
    == 1
}
