// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/supervisor
import fluxer_gateway/config/settings.{
  type Durations, Durations, GatewayDurations,
}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/isolation

@external(erlang, "application", "get_supervisor")
fn get_supervisor(app: Atom) -> Result(Pid, Dynamic)

@external(erlang, "supervisor", "terminate_child")
fn terminate_child(supervisor: Pid, id: Int) -> Dynamic

@external(erlang, "supervisor", "restart_child")
fn restart_child(supervisor: Pid, id: Int) -> Dynamic

fn quick_retry(durations: Durations) -> Durations {
  Durations(
    ..durations,
    gateway: GatewayDurations(
      ..durations.gateway,
      socket_pending_identify_retry_base_ms: 100,
      socket_pending_identify_retry_jitter_ms: 1,
    ),
  )
}

fn child_index(child: supervisor.Child) -> Int {
  let assert Ok(#(index, _)) =
    supervisor.plan(isolation.pristine_settings())
    |> list.index_map(fn(planned, index) { #(index, planned) })
    |> list.find(fn(pair) { pair.1 == child })
  index
}

fn with_child_stopped(
  child: supervisor.Child,
  body: fn(fn() -> Nil) -> a,
) -> a {
  let assert Ok(app) = get_supervisor(atom.create("fluxer_gateway"))
  let index = child_index(child)
  let _ = terminate_child(app, index)
  body(fn() {
    let _ = restart_child(app, index)
    Nil
  })
}

pub fn hello_and_heartbeats_work_before_the_session_manager_is_up_test_() {
  use <- h.within(60.0)
  isolation.set_durations(quick_retry)
  use scenario <- h.run(h.sessions_for("1000000000000811"))
  use bring_up <- with_child_stopped(supervisor.SessionManager)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  ws.send_json(client, h.identify_payload("t-b811"))
  let #(client, quiet) = ws.silent(client, 600)
  assert quiet
  ws.send_json(client, h.heartbeat_payload(json.null()))
  let #(client, ack) = ws.expect_payload(client, 2000)
  assert ack.op == 11
  bring_up()
  let #(client, ready) =
    ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
  assert h.ready_count(client, 600) == 0
  assert list.length(stub.token_calls(scenario.api, "session", "t-b811")) == 1
}

pub fn resume_before_the_session_manager_is_up_closes_resumable_test_() {
  use <- h.within(60.0)
  use scenario <- h.run(h.sessions_for("1000000000000812"))
  use bring_up <- with_child_stopped(supervisor.SessionManager)
  let client = h.hello(scenario.gateway.port)
  ws.send_json(
    client,
    h.resume_payload("t-b812", "0123456789abcdef0123456789abcdef", 3),
  )
  let #(code, reason) = ws.expect_close(client, 3000)
  bring_up()
  assert code == Some(4000)
  assert reason == "Session unavailable"
}

pub fn identify_during_a_session_manager_restart_starts_once_test_() {
  use <- h.within(60.0)
  isolation.set_durations(quick_retry)
  use scenario <- h.run(h.sessions_for("1000000000000813"))
  let clients =
    list.repeat(Nil, 4)
    |> list.map(fn(_) { h.hello(scenario.gateway.port) })
  use bring_up <- with_child_stopped(supervisor.SessionManager)
  list.each(clients, fn(client) {
    ws.send_json(client, h.identify_payload("t-b813"))
  })
  process.sleep(250)
  bring_up()
  let readies =
    list.map(clients, fn(client) {
      let #(client, ready) =
        ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
      assert h.ready_count(client, 300) == 0
      h.session_id(ready)
    })
  assert list.length(list.unique(readies)) == 4
  assert list.length(stub.token_calls(scenario.api, "session", "t-b813")) == 4
}
