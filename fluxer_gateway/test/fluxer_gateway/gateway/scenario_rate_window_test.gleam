// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, type GatewayDurations, Durations, GatewayDurations,
}
import fluxer_gateway/session/manager/registry
import gleam/bit_array
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/gateway_harness as h
import support/gateway_ws_client as ws
import support/isolation
import support/wait

@external(erlang, "os", "system_time")
fn os_system_time(unit: Int) -> Int

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: Atom) -> #(Atom, List(Pid))

fn monitored_by(pid: Pid) -> List(Pid) {
  process_info(pid, atom.create("monitored_by")).1
}

fn now_ms() -> Int {
  os_system_time(1000)
}

fn gateway(
  durations: Durations,
  change: fn(GatewayDurations) -> GatewayDurations,
) -> Durations {
  Durations(..durations, gateway: change(durations.gateway))
}

fn away_from_edges(window_ms: Int) -> Int {
  case window_ms - now_ms() % window_ms < 30_000 {
    True -> away_from_edges(window_ms + 60_000)
    False -> window_ms
  }
}

fn query() -> String {
  "?v=1&encoding=json"
}

fn beats(count: Int, data: String) -> BitArray {
  let beat =
    ws.frame(1, bit_array.from_string("{\"op\":1,\"d\":" <> data <> "}"), True)
  bit_array.concat(list.repeat(beat, count))
}

fn take_acks(client: ws.Client, wanted: Int) -> ws.Client {
  case wanted {
    0 -> client
    _ -> {
      let #(client, _) = ws.wait_for(client, 5000, fn(p) { p.op == 11 })
      take_acks(client, wanted - 1)
    }
  }
}

fn acks_until_close(client: ws.Client, acks: Int) -> #(Int, Int, String) {
  case ws.next(client, 5000) {
    #(client, ws.Text(text)) ->
      case ws.decode_payload(text).op {
        11 -> acks_until_close(client, acks + 1)
        _ -> acks_until_close(client, acks)
      }
    #(client, ws.Ping(_)) | #(client, ws.Pong(_)) ->
      acks_until_close(client, acks)
    #(_, ws.Close(code: Some(code), reason:)) -> #(acks, code, reason)
    #(_, _) -> #(acks, 0, "no close frame")
  }
}

pub fn shared_user_budget_carries_across_resume_test_() {
  use <- h.within(60.0)
  let window = away_from_edges(3_600_000)
  isolation.set_durations(
    gateway(_, fn(g) {
      GatewayDurations(..g, rate_limit_shared_window_ms: window)
    }),
  )
  h.trust_forwarded_for()
  let address = "198.51.100.21"
  use scenario <- h.run(h.sessions_for("1000000000000821"))
  let port = scenario.gateway.port
  let first = h.hello_from(port, query(), address)
  ws.send_json(first, h.identify_payload("t-r821"))
  let #(first, ready) = ws.wait_for(first, 8000, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.send_frame(first, beats(400, "null"))
  let first = take_acks(first, 400)
  ws.close_socket(first)
  assert h.await_detached(h.session_id(ready))
  let second = h.hello_from(port, query(), address)
  ws.send_json(second, h.resume_payload("t-r821", h.session_id(ready), seq))
  let #(second, _) = ws.wait_for(second, 5000, fn(p) { p.t == Some("RESUMED") })
  ws.send_frame(second, beats(250, "null"))
  let #(acks, code, reason) = acks_until_close(second, 0)
  assert acks == 200
  assert code == 4008
  assert reason == "Rate limited"
}

pub fn heartbeats_after_resumed_count_before_the_resume_reply_lands_test_() {
  use <- h.within(60.0)
  let window = away_from_edges(3_600_000)
  isolation.set_durations(
    gateway(_, fn(g) {
      GatewayDurations(..g, rate_limit_shared_window_ms: window)
    }),
  )
  h.trust_forwarded_for()
  let address = "198.51.100.24"
  use scenario <- h.run(h.sessions_for("1000000000000824"))
  let port = scenario.gateway.port
  let first = h.hello_from(port, query(), address)
  ws.send_json(first, h.identify_payload("t-r824"))
  let #(first, ready) = ws.wait_for(first, 8000, fn(p) { p.t == Some("READY") })
  let assert Some(seq) = ready.s
  ws.send_frame(first, beats(400, "null"))
  let first = take_acks(first, 400)
  ws.close_socket(first)
  assert h.await_detached(h.session_id(ready))
  let assert Ok(ref) = registry.lookup(h.session_id(ready))
  let assert Ok(session_pid) = process.subject_owner(ref.inbox)
  let watchers = monitored_by(session_pid)
  assert suspend_process(session_pid)
  let second = h.hello_from(port, query(), address)
  ws.send_json(second, h.resume_payload("t-r824", h.session_id(ready), seq))
  let askers = fn() {
    list.filter(monitored_by(session_pid), fn(pid) {
      !list.contains(watchers, pid)
    })
  }
  assert wait.until(5000, fn() { askers() != [] })
  let held = askers()
  assert list.all(held, suspend_process)
  assert resume_process(session_pid)
  let #(second, _) = ws.wait_for(second, 5000, fn(p) { p.t == Some("RESUMED") })
  ws.send_frame(second, beats(250, "null"))
  let #(acks, code, reason) = acks_until_close(second, 0)
  assert list.all(held, resume_process)
  assert acks == 200
  assert code == 4008
  assert reason == "Rate limited"
}

pub fn connection_window_slides_test_() {
  use <- h.within(60.0)
  isolation.set_durations(
    gateway(_, fn(g) {
      GatewayDurations(..g, rate_limit_connection_window_ms: 3000)
    }),
  )
  h.trust_forwarded_for()
  use scenario <- h.run(h.sessions_for("1000000000000822"))
  let client = h.hello_from(scenario.gateway.port, query(), "198.51.100.22")
  ws.send_frame(client, beats(600, "null"))
  let client = take_acks(client, 600)
  process.sleep(3300)
  ws.send_frame(client, beats(601, "null"))
  let #(acks, code, reason) = acks_until_close(client, 0)
  assert acks == 600
  assert code == 4008
  assert reason == "Rate limited"
}

pub fn resumes_and_identifies_share_the_per_address_budget_test_() {
  use <- h.within(60.0)
  let window_secs = away_from_edges(3_600_000) / 1000
  isolation.set_durations(
    gateway(_, fn(g) {
      GatewayDurations(..g, abuse_protection_identify_window_secs: window_secs)
    }),
  )
  h.trust_forwarded_for()
  let address = "198.51.100.23"
  use scenario <- h.run(h.sessions_for("1000000000000823"))
  let port = scenario.gateway.port
  let client = h.hello_from(port, query(), address)
  let resume =
    ws.frame(
      1,
      bit_array.from_string(
        json.to_string(h.resume_payload("t-r823", "short", 1)),
      ),
      True,
    )
  ws.send_frame(client, bit_array.concat(list.repeat(resume, 301)))
  let #(invalid, code, reason) = invalid_until_close(client, 0)
  assert invalid == 300
  assert code == 4008
  assert reason == "Resume rate limited"
  let other = h.hello_from(port, query(), address)
  ws.send_json(other, h.identify_payload("t-r823"))
  let #(code, reason) = ws.expect_close(other, 5000)
  assert code == Some(4008)
  assert reason == "Identify rate limited"
  let elsewhere = h.hello_from(port, query(), "198.51.100.24")
  ws.send_json(elsewhere, h.identify_payload("t-r823"))
  let #(_elsewhere, ready) =
    ws.wait_for(elsewhere, 8000, fn(p) { p.t == Some("READY") })
  assert ready.op == 0
}

fn invalid_until_close(client: ws.Client, seen: Int) -> #(Int, Int, String) {
  case ws.next(client, 5000) {
    #(client, ws.Text(text)) ->
      case ws.decode_payload(text).op {
        9 -> invalid_until_close(client, seen + 1)
        _ -> invalid_until_close(client, seen)
      }
    #(_, ws.Close(code: Some(code), reason:)) -> #(seen, code, reason)
    #(_, _) -> #(seen, 0, "no close frame")
  }
}
