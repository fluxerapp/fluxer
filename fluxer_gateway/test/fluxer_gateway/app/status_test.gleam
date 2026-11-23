// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/app/drain_report.{Left}
import fluxer_gateway/app/health
import fluxer_gateway/app/status
import fluxer_gateway/app/supervisor
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/build_info
import fluxer_gateway/config/draining
import fluxer_gateway/config/env
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/bit_array
import gleam/bytes_tree.{type BytesTree}
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/atom
import gleam/erlang/process
import gleam/http
import gleam/http/request
import gleam/http/response.{type Response}
import gleam/httpc
import gleam/int
import gleam/json
import gleam/list
import gleam/otp/static_supervisor
import gleam/string
import glisten

const report_keys = [
  "state_code", "failed_flag", "started_at_ms", "finished_at_ms", "moved",
  "failed", "left", "buckets_left", "fenced",
]

const top_keys = [
  "wire_epoch", "wire_epoch_floor", "epoch_mismatch", "epoch_refused_peers",
  "bridged_peers", "ready", "in_service", "draining", "tier_grace",
  "lease_grace", "fenced", "fence_age_seconds", "joined", "bouncers_seen",
  "bouncers_configured", "bouncer_quorum", "join_expiries",
  "session_move_failures", "sessions", "guilds", "presences", "buckets",
  "moves_in_flight",
]

fn analysis_keys() -> List(String) {
  list.flatten([
    top_keys,
    list.map(report_keys, fn(key) { "drain_" <> key }),
    list.map(report_keys, fn(key) { "last_drain_" <> key }),
  ])
}

fn from(address: glisten.IpAddress, method: http.Method, path: String) {
  request.new()
  |> request.set_method(method)
  |> request.set_path(path)
  |> request.set_body(address)
}

fn body_of(response: Response(BytesTree)) -> String {
  let assert Ok(text) =
    bit_array.to_string(bytes_tree.to_bit_array(response.body))
  text
}

fn remote() -> glisten.IpAddress {
  glisten.IpV4(10, 0, 0, 7)
}

fn decoded(text: String) {
  let numbers = decode.dict(decode.string, decode.int)
  let report = {
    use state <- decode.field("state", decode.string)
    use reason <- decode.field("reason", decode.string)
    use code <- decode.field("state_code", decode.int)
    decode.success(#(state, reason, code))
  }
  let shape = {
    use role <- decode.field("role", decode.string)
    use _template <- decode.field("template", decode.string)
    use _fence_age <- decode.field("fence_age_seconds", decode.int)
    use version <- decode.field("version", decode.string)
    use revision <- decode.field("revision", decode.string)
    use wire_epoch <- decode.field("wire_epoch", decode.int)
    use ready <- decode.field("ready", decode.int)
    use reason <- decode.field("ready_reason", decode.string)
    use current <- decode.field("drain", report)
    use last <- decode.field("last_drain", report)
    use seen <- decode.subfield(["bouncers", "seen"], decode.int)
    use quorum <- decode.subfield(["bouncers", "quorum"], decode.int)
    use sessions <- decode.field("sessions", decode.int)
    use analysis <- decode.field("analysis", numbers)
    decode.success(#(
      #(role, version, revision, wire_epoch, ready, reason),
      #(current, last, seen, quorum, sessions),
      analysis,
    ))
  }
  let assert Ok(found) = json.parse(text, shape)
  found
}

pub fn the_snapshot_reports_the_node_identity_and_holdings_test() {
  let snapshot = status.snapshot()
  let configured = settings.get()
  let held = drain.holdings()
  let #(code, reason) = health.readiness()
  assert snapshot.role == node.role_name(configured.role)
  assert snapshot.version == build_info.version()
  assert snapshot.revision == build_info.revision()
  assert snapshot.wire_epoch == build_info.wire_epoch
  assert snapshot.wire_epoch_floor == wire.floor()
  assert snapshot.epoch_refused_peers == 0
  assert snapshot.ready == { code == 200 }
  assert snapshot.ready_reason == reason
  assert snapshot.sessions == held.sessions
  assert snapshot.guilds == held.guilds
  assert snapshot.presences == held.presences
  assert snapshot.buckets == held.buckets
  assert snapshot.drain.phase == drain_report.Idle
  assert snapshot.bouncers_configured == 0
  assert !snapshot.bouncer_quorum
  assert !snapshot.fenced
}

pub fn the_snapshot_counts_join_expiries_session_move_failures_and_moves_in_flight_test() {
  drain_report.reset()
  let _ = drain_report.begin()
  draining.set_draining()
  let before = status.snapshot()
  metrics.counter("gateway_guild_joins_expired_total", [], 2)
  metrics.counter(drain_report.session_moves, [#("outcome", "failed")], 3)
  metrics.counter(drain_report.session_moves, [#("outcome", "moved")], 5)
  list.each(["a", "b", "c", "a"], drain_report.entity_failed("session", _))
  metrics.gauge("fluxer_gateway_handoff_moves_in_flight", [], 4)
  let after = status.snapshot()
  assert after.join_expiries == before.join_expiries + 2
  assert after.session_move_failures == before.session_move_failures + 3
  assert after.moves_in_flight == 4
  metrics.gauge("fluxer_gateway_handoff_moves_in_flight", [], 0)
  draining.clear_draining()
  drain_report.reset()
}

pub fn the_snapshot_carries_the_current_and_last_drain_test() {
  drain_report.reset()
  let _ = drain_report.begin()
  draining.set_draining()
  let failed = drain_report.settle(Left(entities: 2, buckets: 1), False, True)
  let snapshot = status.snapshot()
  assert snapshot.drain == failed
  assert snapshot.last_drain == failed
  let analysis = dict.from_list(status.numbers(snapshot))
  assert dict.get(analysis, "drain_state_code") == Ok(3)
  assert dict.get(analysis, "drain_failed_flag") == Ok(1)
  assert dict.get(analysis, "last_drain_left") == Ok(2)
  assert dict.get(analysis, "last_drain_buckets_left") == Ok(1)
  draining.clear_draining()
  drain_report.reset()
  let idle = dict.from_list(status.numbers(status.snapshot()))
  assert dict.get(idle, "drain_state_code") == Ok(0)
  assert dict.get(idle, "last_drain_state_code") == Ok(3)
  assert dict.get(idle, "last_drain_failed_flag") == Ok(1)
}

pub fn the_json_names_every_field_with_numbers_for_analysis_test() {
  let snapshot = status.snapshot()
  let #(identity, drains, analysis) = decoded(status.to_json(snapshot))
  assert identity
    == #(
      snapshot.role,
      snapshot.version,
      snapshot.revision,
      build_info.wire_epoch,
      case snapshot.ready {
        True -> 1
        False -> 0
      },
      snapshot.ready_reason,
    )
  let #(current, last, seen, quorum, sessions) = drains
  assert current.0 == drain_report.phase_name(snapshot.drain.phase)
  assert current.2 == drain_report.phase_code(snapshot.drain.phase)
  assert last.0 == drain_report.phase_name(snapshot.last_drain.phase)
  assert seen == snapshot.bouncers_seen
  assert quorum == 0
  assert sessions == snapshot.sessions
  assert list.sort(dict.keys(analysis), string.compare)
    == list.sort(analysis_keys(), string.compare)
  assert dict.to_list(analysis) |> list.sort(by_key)
    == status.numbers(snapshot) |> list.sort(by_key)
}

fn by_key(a: #(String, Int), b: #(String, Int)) {
  string.compare(a.0, b.0)
}

pub fn metrics_export_only_catalogued_families_with_allowed_labels_test() {
  metrics.counter(
    drain_report.handoff_moves,
    [#("ns", "1"), #("outcome", "moved")],
    1,
  )
  metrics.counter(
    drain_report.handoff_moves,
    [#("guild_id", "1234567890"), #("outcome", "moved")],
    1,
  )
  metrics.counter(
    "fluxer_gateway_bouncer_beat_mismatch_total",
    [#("bouncer", "0")],
    1,
  )
  let text = status.metrics_text(status.snapshot())
  let constant =
    "{role=\"" <> node.role_name(settings.get().role) <> "\",pool=\"default\"} "
  list.each(analysis_keys(), fn(key) {
    assert string.contains(
      text,
      "# TYPE fluxer_gateway_status_"
        <> key
        <> " gauge\nfluxer_gateway_status_"
        <> key
        <> constant,
    )
  })
  assert string.contains(
    text,
    "# TYPE fluxer_gateway_bouncer_fences_total counter",
  )
  assert !string.contains(text, "fluxer_gateway_handoff_moves_total")
  assert !string.contains(text, "ns=")
  assert !string.contains(text, "guild_id")
  assert !string.contains(text, "1234567890")
  assert !string.contains(text, "beat_mismatch")
  assert !string.contains(text, node.to_string(node.self()))
  assert !string.contains(text, "@")
  string.split(text, "\n")
  |> list.filter(fn(line) { line != "" && !string.starts_with(line, "# ") })
  |> list.each(fn(line) {
    let assert Ok(#(_, value)) = string.split_once(line, " ")
    let assert Ok(_) = int.parse(value)
    Nil
  })
}

pub fn status_answers_any_peer_and_serves_only_reads_test() {
  let served = status.handle(from(remote(), http.Get, "/_status"))
  assert served.status == 200
  assert response_header(served, "content-type") == Ok("application/json")
  let _ = decoded(body_of(served))
  let scraped = status.handle(from(remote(), http.Get, "/_metrics"))
  assert scraped.status == 200
  assert string.contains(body_of(scraped), "fluxer_gateway_status_ready{role=")
  assert status.handle(from(remote(), http.Post, "/_status")).status == 405
  assert status.handle(from(remote(), http.Delete, "/_metrics")).status == 405
  list.each(
    [
      "/_health/drain", "/_health/undrain", "/_health/ready", "/",
      "/_status/../_health/drain",
    ],
    fn(path) {
      assert status.handle(from(remote(), http.Get, path)).status == 404
      assert status.handle(from(remote(), http.Post, path)).status == 404
    },
  )
  assert !draining.is_draining()
  assert status.route("/_status") == status.StatusRoute
  assert status.route("/_metrics") == status.MetricsRoute
  assert status.route("/_health/drained") == status.NotFound
}

fn response_header(
  found: Response(BytesTree),
  name: String,
) -> Result(String, Nil) {
  list.key_find(found.headers, name)
}

pub fn the_listener_serves_status_over_http_on_its_own_port_test() {
  let name = env.process_name("fluxer_gateway_status_listener_test")
  let assert Ok(started) =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.add(status.listening(0, name))
    |> static_supervisor.start
  let assert Ok(port) = status.port_of(name)
  assert port > 0
  let url = "http://127.0.0.1:" <> int.to_string(port)
  let assert Ok(req) = request.to(url <> "/_status")
  let assert Ok(answer) = httpc.send(req)
  assert answer.status == 200
  let #(identity, _, _) = decoded(answer.body)
  assert identity.3 == build_info.wire_epoch
  let assert Ok(req) = request.to(url <> "/_health/drain")
  let assert Ok(answer) = httpc.send(req)
  assert answer.status == 404
  assert !draining.is_draining()
  process.unlink(started.pid)
  let monitor = process.monitor(started.pid)
  shutdown(started.pid, atom.create("shutdown"))
  let assert Ok(Nil) =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  assert status.port_of(name) == Error(Nil)
}

@external(erlang, "erlang", "exit")
fn shutdown(pid: process.Pid, reason: atom.Atom) -> Bool

pub fn the_status_listener_is_planned_only_with_a_port_test() {
  let with = fn(port) {
    supervisor.plan(settings.Settings(..settings.defaults(), status_port: port))
  }
  assert list.last(with(8082)) == Ok(supervisor.HttpListener)
  assert list.contains(with(8082), supervisor.StatusListener)
  assert !list.contains(with(0), supervisor.StatusListener)
  assert settings.defaults().status_port == 8082
}

fn with_scrape_header(incoming: request.Request(glisten.IpAddress)) {
  request.set_header(incoming, "x-prometheus-scrape-timeout-seconds", "10")
}

pub fn only_a_scraper_get_moves_the_drain_scrape_hold_test() {
  let before = rollout_metrics.last_scrape_ms()
  process.sleep(5)
  assert status.handle(from(remote(), http.Get, "/_metrics")).status == 200
  assert status.handle(from(remote(), http.Head, "/_metrics")).status == 200
  assert status.handle(from(remote(), http.Get, "/_status")).status == 200
  assert rollout_metrics.last_scrape_ms() == before
  let scraped =
    status.handle(with_scrape_header(from(remote(), http.Get, "/_metrics")))
  assert scraped.status == 200
  assert rollout_metrics.last_scrape_ms() > before
}

pub fn a_scrape_is_stamped_with_the_time_its_snapshot_was_read_test() {
  let inside = process.new_subject()
  let _ =
    status.scrape(True, fn() {
      process.sleep(5)
      process.send(inside, clock.system_ms())
      "text"
    })
  let assert Ok(rendered_at) = process.receive(inside, 1000)
  assert rollout_metrics.last_scrape_ms() < rendered_at
  let before = rollout_metrics.last_scrape_ms()
  let _ = status.scrape(False, fn() { "text" })
  assert rollout_metrics.last_scrape_ms() == before
}

pub fn the_fence_age_is_negative_until_the_first_fence_test() {
  assert status.fence_age_seconds(Error(Nil), 90_000) == -1
  assert status.fence_age_seconds(Ok(30_000), 90_500) == 60
  assert status.fence_age_seconds(Ok(90_000), 90_999) == 0
  assert status.fence_age_seconds(Ok(91_000), 90_000) == 0
}

pub fn the_status_names_the_template_and_the_fence_age_test() {
  let snapshot =
    status.Status(
      ..status.snapshot(),
      template: "7d9f8c6b5",
      fence_age_seconds: 42,
    )
  let assert Ok(fields) =
    json.parse(status.to_json(snapshot), {
      use template <- decode.field("template", decode.string)
      use age <- decode.field("fence_age_seconds", decode.int)
      use analysis <- decode.subfield(
        ["analysis", "fence_age_seconds"],
        decode.int,
      )
      decode.success(#(template, age, analysis))
    })
  assert fields == #("7d9f8c6b5", 42, 42)
  assert string.contains(
    status.metrics_text(snapshot),
    "\nfluxer_gateway_status_fence_age_seconds{role=\"",
  )
}
