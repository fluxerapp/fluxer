// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/app/drain_report
import fluxer_gateway/app/http as app_http
import fluxer_gateway/app/status
import fluxer_gateway/cluster/handoff
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings.{BouncerSettings, Settings}
import fluxer_gateway/core/node
import fluxer_gateway/session/manager/registry
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process
import gleam/http/request
import gleam/httpc
import gleam/int
import gleam/list
import gleam/option.{Some}
import support/gateway_api_stub as stub
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/session_harness as sh
import support/wait

@external(erlang, "ets", "lookup")
fn ets_lookup(table: Atom, key: String) -> List(#(String, Int))

fn open_connections(address: String) -> Int {
  case ets_lookup(atom.create("gateway_ip_connections"), address) {
    [#(_, count)] -> count
    _ -> 0
  }
}

fn as_role(role: node.Role) -> Nil {
  settings.store(Settings(..settings.get(), role:))
  Nil
}

fn ready(client: ws.Client, token: String) -> #(ws.Client, ws.Payload) {
  ws.send_json(client, h.identify_payload(token))
  ws.wait_for(client, 8000, fn(p) { p.t == Some("READY") })
}

fn expect_single_reconnect_then_close(client: ws.Client) -> Nil {
  let #(client, reconnect) = ws.wait_for(client, 5000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(client, ws.Close(code, reason)) = ws.next(client, 5000)
  assert code == Some(4000)
  assert reason == "Session drain requested; reconnect to continue"
  assert ws.expect_eof(client, 5000)
}

pub fn all_roles_drain_reaches_unidentified_identifying_and_ready_sockets_test_() {
  use <- h.within(60.0)
  h.trust_forwarded_for()
  as_role(node.AllRoles)
  let address = "192.0.2.75"
  use scenario <- h.run(h.sessions_for("1000000000000751"))
  let port = scenario.gateway.port
  let query = "?v=1&encoding=json"
  let fresh = h.hello_from(port, query, address)
  let #(attached, first) = ready(h.hello_from(port, query, address), "t-d751")
  draining.set_draining()
  let held = h.hello_from(port, query, address)
  ws.send_json(held, h.identify_payload("t-d751-held"))
  expect_single_reconnect_then_close(held)
  assert wait.until(5000, fn() { open_connections(address) == 2 })
  handoff.reconnect_sockets()
  expect_single_reconnect_then_close(fresh)
  expect_single_reconnect_then_close(attached)
  assert wait.until(5000, fn() { open_connections(address) == 0 })
  assert stub.token_calls(scenario.api, "session", "t-d751-held") == []
  assert h.await_detached(h.session_id(first))
}

pub fn all_roles_drain_reaches_a_socket_whose_start_is_in_flight_test_() {
  use <- h.within(60.0)
  h.trust_forwarded_for()
  as_role(node.AllRoles)
  let address = "192.0.2.76"
  let answered = process.new_subject()
  let respond = fn(kind, body) {
    let reply = h.sessions_for("1000000000000752")(kind, body)
    case kind {
      "session" -> {
        process.sleep(600)
        process.send(answered, Nil)
        reply
      }
      _ -> reply
    }
  }
  use scenario <- h.run(respond)
  let before = registry.all()
  let started = fn() {
    registry.all()
    |> list.filter(fn(ref) { !list.contains(before, ref) })
  }
  let starting =
    h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  ws.send_json(starting, h.identify_payload("t-d752"))
  assert wait.until(5000, fn() {
    stub.token_calls(scenario.api, "session", "t-d752") != []
  })
  handoff.reconnect_sockets()
  expect_single_reconnect_then_close(starting)
  assert wait.until(5000, fn() { open_connections(address) == 0 })
  let assert Ok(Nil) = process.receive(answered, 5000)
  process.sleep(1000)
  assert wait.until(5000, fn() { started() == [] })
}

pub fn websocket_role_drain_still_reaches_unidentified_sockets_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  as_role(node.Websocket)
  let address = "192.0.2.77"
  use scenario <- h.run(h.sessions_for("1000000000000753"))
  let fresh = h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  handoff.reconnect_sockets()
  expect_single_reconnect_then_close(fresh)
  assert wait.until(5000, fn() { open_connections(address) == 0 })
}

pub fn non_socket_role_drain_leaves_sockets_alone_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  as_role(node.Guilds)
  let address = "192.0.2.78"
  use scenario <- h.run(h.sessions_for("1000000000000754"))
  let fresh = h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  handoff.reconnect_sockets()
  let #(_fresh, quiet) = ws.silent(fresh, 400)
  assert quiet
  assert open_connections(address) == 1
}

fn as_bouncer_worker(role: node.Role, notice_delay_ms: Int) -> Nil {
  as_bouncer_worker_until(role, notice_delay_ms, 600_000)
}

fn as_bouncer_worker_until(
  role: node.Role,
  notice_delay_ms: Int,
  deadline_ms: Int,
) -> Nil {
  let current = settings.get()
  settings.store(
    Settings(
      ..current,
      role:,
      bouncer: Some(BouncerSettings(
        dns_name: "gateway-bouncer.test.invalid",
        basename: "gateway-bouncer",
        count: 3,
        pool: "default",
        template_hash: "",
      )),
      durations: settings.Durations(
        ..current.durations,
        app: settings.AppDurations(
          drain_deadline_ms: deadline_ms,
          drain_frame_flush_ms: 20,
          drain_handoff_wait_ms: 2000,
          drain_socket_notice_delay_ms: notice_delay_ms,
        ),
      ),
    ),
  )
  Nil
}

fn drop_sessions() -> Nil {
  list.each(registry.all(), sh.terminate)
  assert wait.until(5000, fn() { registry.count() == 0 })
}

fn reset_drain() -> Nil {
  let assert Ok(Nil) = drain.undrain()
  assert !draining.is_draining()
}

pub fn bouncer_mode_socket_drain_sends_reconnect_before_drained_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  let address = "192.0.2.79"
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000755"))
  as_bouncer_worker(node.WebsocketSessions, 300)
  let fresh = h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  drain.start()
  assert draining.is_draining()
  assert drain.drained() == Ok(False)
  expect_single_reconnect_then_close(fresh)
  assert wait.until(5000, fn() { drain.drained() == Ok(True) })
  assert wait.until(5000, fn() { open_connections(address) == 0 })
  reset_drain()
  assert drain.drained() == Ok(False)
}

pub fn bouncer_mode_shutdown_reconnects_sockets_without_a_drain_call_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  let address = "192.0.2.80"
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000756"))
  as_bouncer_worker(node.WebsocketSessions, 60_000)
  let fresh = h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  drain.run()
  expect_single_reconnect_then_close(fresh)
  assert drain.drained() == Ok(True)
  assert wait.until(5000, fn() { open_connections(address) == 0 })
  reset_drain()
}

pub fn bouncer_mode_non_socket_drain_leaves_sockets_alone_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  let address = "192.0.2.81"
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000757"))
  as_bouncer_worker(node.Calls, 20)
  let fresh = h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address)
  drain.start()
  assert wait.until(5000, fn() { drain.drained() == Ok(True) })
  let #(_fresh, quiet) = ws.silent(fresh, 400)
  assert quiet
  assert open_connections(address) == 1
  reset_drain()
}

pub fn bouncer_mode_sessions_drain_waits_for_every_session_to_leave_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  as_role(node.AllRoles)
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000758"))
  let address = "192.0.2.82"
  let #(_client, first) =
    ready(
      h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address),
      "t-d758",
    )
  let assert Ok(held) = registry.lookup(h.session_id(first))
  as_bouncer_worker(node.Sessions, 20)
  drain.start()
  process.sleep(1500)
  assert registry.lookup(h.session_id(first)) == Ok(held)
  assert drain.drained() == Ok(False)
  sh.terminate(held)
  assert wait.until(5000, fn() { drain.drained() == Ok(True) })
  reset_drain()
}

fn drained_answer(port: Int) -> #(Int, String) {
  let assert Ok(req) =
    request.to("http://127.0.0.1:" <> int.to_string(port) <> "/_health/drained")
  let assert Ok(answer) = httpc.send(req)
  #(answer.status, answer.body)
}

pub fn bouncer_mode_drain_that_cannot_move_a_session_fails_visibly_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  as_role(node.AllRoles)
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000759"))
  let address = "192.0.2.83"
  let #(_client, first) =
    ready(
      h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address),
      "t-d759",
    )
  let assert Ok(held) = registry.lookup(h.session_id(first))
  as_bouncer_worker(node.Sessions, 20)
  let assert Ok(port) = app_http.bound_port()
  drain.start()
  assert drained_answer(port) == #(503, "DRAINING")
  let assert Ok(running) = drain.outcome()
  assert running.phase == drain_report.Draining
  assert running.left == 1
  drain.run()
  let assert Ok(failed) = drain.outcome()
  assert failed.phase == drain_report.Failed
  assert failed.reason == "deadline"
  assert failed.left == 1
  assert drain.drained() == Ok(False)
  assert drained_answer(port) == #(503, "DRAIN_FAILED")
  assert registry.lookup(h.session_id(first)) == Ok(held)
  assert drain_report.last() == failed
  let snapshot = status.snapshot()
  assert snapshot.drain.phase == drain_report.Failed
  assert snapshot.last_drain == failed
  assert snapshot.sessions == 1
  sh.terminate(held)
  assert wait.until(5000, fn() { registry.count() == 0 })
  assert drained_answer(port) == #(503, "DRAIN_FAILED")
  reset_drain()
  assert drain_report.current(drain.left()).phase == drain_report.Idle
  assert drain_report.last() == failed
}

pub fn bouncer_mode_drain_waits_for_a_lingering_socket_to_close_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  let address = "192.0.2.84"
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000760"))
  let #(fresh, _) =
    ready(
      h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address),
      "t-d760",
    )
  as_bouncer_worker(node.Websocket, 20)
  drain.start()
  let #(fresh, reconnect) = ws.wait_for(fresh, 5000, fn(p) { p.op == 7 })
  assert reconnect.op == 7
  let assert #(fresh, ws.Close(Some(4000), _)) = ws.next(fresh, 5000)
  process.sleep(300)
  assert drain.drained() == Ok(False)
  let assert Ok(running) = drain.outcome()
  assert running.left == 1
  assert ws.expect_eof(fresh, 5000)
  assert wait.until(5000, fn() { drain.drained() == Ok(True) })
  reset_drain()
}

pub fn bouncer_mode_drain_stuck_past_its_deadline_fails_while_polled_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  as_role(node.AllRoles)
  drop_sessions()
  use scenario <- h.run(h.sessions_for("1000000000000761"))
  let address = "192.0.2.85"
  let #(_client, first) =
    ready(
      h.hello_from(scenario.gateway.port, "?v=1&encoding=json", address),
      "t-d761",
    )
  let assert Ok(held) = registry.lookup(h.session_id(first))
  as_bouncer_worker_until(node.Sessions, 20, 1500)
  let assert Ok(port) = app_http.bound_port()
  drain.start()
  assert drained_answer(port) == #(503, "DRAINING")
  assert wait.until(8000, fn() {
    case drain.outcome() {
      Ok(report) -> report.phase == drain_report.Failed
      Error(Nil) -> False
    }
  })
  let assert Ok(failed) = drain.outcome()
  assert failed.reason == "deadline"
  assert failed.left == 1
  sh.terminate(held)
  assert wait.until(5000, fn() { registry.count() == 0 })
  reset_drain()
}

pub fn bouncer_mode_drained_answers_not_draining_unless_a_drain_runs_test_() {
  use <- h.within(30.0)
  h.trust_forwarded_for()
  drop_sessions()
  use _scenario <- h.run(h.sessions_for("1000000000000762"))
  as_bouncer_worker(node.Calls, 20)
  let assert Ok(port) = app_http.bound_port()
  assert !draining.is_draining()
  assert drained_answer(port) == #(503, "NOT_DRAINING")
  drain.start()
  assert wait.until(5000, fn() { drained_answer(port) == #(200, "DRAINED") })
  reset_drain()
  assert drained_answer(port) == #(503, "NOT_DRAINING")
  assert drain.drained() == Ok(False)
}
