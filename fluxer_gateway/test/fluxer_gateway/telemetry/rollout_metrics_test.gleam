// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import fluxer_gateway/app/drain_report.{Left}
import fluxer_gateway/app/status
import fluxer_gateway/core/node
import fluxer_gateway/gateway/socket
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/source
import fluxer_gateway/telemetry/rollout_metrics.{
  type Family, Counter, Gauge, Histogram,
}
import gleam/dynamic
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import gleam/string

const roles = [
  node.Guilds,
  node.Presence,
  node.Sessions,
  node.Calls,
  node.Push,
  node.Websocket,
]

type Line {
  Line(name: String, labels: List(#(String, String)), value: Int)
}

fn parse(text: String) -> List(Line) {
  string.split(text, "\n")
  |> list.filter(fn(line) { line != "" && !string.starts_with(line, "#") })
  |> list.map(fn(line) {
    let assert Ok(#(series, value)) = string.split_once(line, " ")
    let assert Ok(value) = int.parse(value)
    case string.split_once(series, "{") {
      Error(Nil) -> Line(series, [], value)
      Ok(#(name, rest)) -> Line(name, pairs(string.drop_end(rest, 1)), value)
    }
  })
}

fn pairs(text: String) -> List(#(String, String)) {
  string.split(text, ",")
  |> list.map(fn(pair) {
    let assert Ok(#(key, quoted)) = string.split_once(pair, "=")
    #(key, string.drop_start(string.drop_end(quoted, 1), 1))
  })
}

fn rendered() -> String {
  status.metrics_text(status.snapshot())
}

fn render_as(role: node.Role) -> String {
  let snapshot = status.snapshot()
  rollout_metrics.render(
    status.catalogue(snapshot),
    role,
    status.constant_labels(),
    status.computed(snapshot, role),
  )
}

fn family_of(catalogue: List(Family), line: Line) -> Result(Family, Nil) {
  list.find(catalogue, fn(found) {
    case found.kind {
      Histogram(_) ->
        line.name == found.name <> "_bucket"
        || line.name == found.name <> "_sum"
        || line.name == found.name <> "_count"
      Counter | Gauge -> line.name == found.name
    }
  })
}

pub fn every_family_is_registered_with_help_and_type_test() {
  let text = rendered()
  let catalogue = status.catalogue(status.snapshot())
  list.each(catalogue, fn(found) {
    let kind = case found.kind {
      Counter -> "counter"
      Gauge -> "gauge"
      Histogram(_) -> "histogram"
    }
    assert string.contains(text, "# HELP " <> found.name <> " ")
    assert string.contains(text, "# TYPE " <> found.name <> " " <> kind <> "\n")
  })
  let names = list.map(catalogue, fn(found) { found.name })
  assert list.unique(names) == names
  list.each(rollout_metrics.families(), fn(found) {
    assert rollout_metrics.family(found.name) == Ok(found)
  })
}

pub fn every_sample_carries_only_allowed_labels_and_values_test() {
  rollout_metrics.count(
    rollout_metrics.moves_aborted,
    [#("kind", "guild"), #("reason", "guild 1234567890"), #("guild_id", "77")],
    1,
  )
  rollout_metrics.observe(
    rollout_metrics.move_freeze,
    [#("kind", "presence"), #("size_class", "under_64k"), #("user_id", "9")],
    120,
  )
  list.each(roles, fn(role) {
    let text = render_as(role)
    let catalogue = status.catalogue(status.snapshot())
    assert !string.contains(text, "1234567890")
    assert !string.contains(text, "guild_id")
    assert !string.contains(text, "user_id")
    assert !string.contains(text, node.to_string(node.self()))
    let lines = parse(text)
    assert lines != []
    list.each(lines, fn(line) {
      let assert Ok(found) = family_of(catalogue, line)
      assert list.take(line.labels, 2) == status.constant_labels()
      list.each(list.drop(line.labels, 2), fn(label) {
        let #(key, value) = label
        assert list.contains(rollout_metrics.allowed_labels, key) || key == "le"
        case key {
          "le" -> {
            let assert Histogram(bounds) = found.kind
            assert value == "+Inf"
              || list.contains(list.map(bounds, int.to_string), value)
          }
          _ -> {
            let assert Ok(values) = list.key_find(found.labels, key)
            assert list.contains(values, value) || value == "other"
          }
        }
      })
    })
  })
}

pub fn unknown_values_fold_into_other_and_unknown_names_are_ignored_test() {
  let before =
    rollout_metrics.total(rollout_metrics.moves_aborted, [
      #("kind", "guild"),
      #("reason", "other"),
    ])
  rollout_metrics.count(
    rollout_metrics.moves_aborted,
    [#("kind", "guild"), #("reason", "a reason nobody declared")],
    2,
  )
  assert rollout_metrics.total(rollout_metrics.moves_aborted, [
      #("kind", "guild"),
      #("reason", "other"),
    ])
    == before + 2
  rollout_metrics.count("fluxer_gateway_not_in_the_catalogue_total", [], 1)
  assert !string.contains(rendered(), "fluxer_gateway_not_in_the_catalogue")
  rollout_metrics.set(rollout_metrics.moves_started, [#("kind", "guild")], 99)
  rollout_metrics.observe(rollout_metrics.moves_started, [], 5)
  assert rollout_metrics.observed(rollout_metrics.moves_started, []) == 0
}

pub fn histograms_are_cumulative_with_few_buckets_test() {
  let labels = [#("kind", "call"), #("size_class", "under_1m")]
  let before = rollout_metrics.observed(rollout_metrics.move_freeze, labels)
  rollout_metrics.observe(rollout_metrics.move_freeze, labels, 90)
  rollout_metrics.observe(rollout_metrics.move_freeze, labels, 9000)
  assert rollout_metrics.observed(rollout_metrics.move_freeze, labels)
    == before + 2
  let lines =
    parse(rendered())
    |> list.filter(fn(line) {
      line.name == rollout_metrics.move_freeze <> "_bucket"
      && list.contains(line.labels, #("kind", "call"))
      && list.contains(line.labels, #("size_class", "under_1m"))
    })
  let at = fn(bound) {
    list.find(lines, fn(line) { list.contains(line.labels, #("le", bound)) })
    |> result.map(fn(line) { line.value })
  }
  assert list.length(lines) <= 8
  assert at("50") == Ok(before)
  assert at("100") == Ok(before + 1)
  assert at("5000") == Ok(before + 1)
  assert at("+Inf") == Ok(before + 2)
  list.each(rollout_metrics.families(), fn(found) {
    case found.kind {
      Histogram(bounds) -> {
        assert list.length(bounds) <= 7
        assert list.sort(bounds, int.compare) == bounds
      }
      Counter | Gauge -> Nil
    }
  })
}

pub fn a_finished_drain_counts_its_outcome_and_duration_once_test() {
  let labels = [#("outcome", "drained"), #("reason", "none")]
  let before = rollout_metrics.total(rollout_metrics.drain_outcomes, labels)
  let observed =
    rollout_metrics.observed(rollout_metrics.drain_duration, [
      #("outcome", "drained"),
    ])
  let _ = drain_report.begin()
  let done = drain_report.settle(Left(0, 0), True, False)
  assert done.phase == drain_report.Drained
  let _ = drain_report.settle(Left(0, 0), True, True)
  assert rollout_metrics.total(rollout_metrics.drain_outcomes, labels)
    == before + 1
  assert rollout_metrics.observed(rollout_metrics.drain_duration, [
      #("outcome", "drained"),
    ])
    == observed + 1
  drain_report.reset()
}

pub fn the_first_ready_answer_is_timed_once_test() {
  let before = rollout_metrics.observed(rollout_metrics.time_to_ready, [])
  rollout_metrics.ready_seen(False)
  rollout_metrics.ready_seen(True)
  rollout_metrics.ready_seen(True)
  let after = rollout_metrics.observed(rollout_metrics.time_to_ready, [])
  assert after <= 1
  assert after >= before
  assert rollout_metrics.uptime_ms() > 0
}

pub fn move_outcomes_name_their_abort_reason_test() {
  let assert Ok(at) = epoch.from_int(3)
  assert move.abort_reason(source.Thawed(at)) == Ok("thawed")
  assert move.abort_reason(source.Fenced) == Ok("fenced")
  assert move.abort_reason(source.NotMoved(source.LeaseLost))
    == Ok("lease_lost")
  assert move.abort_reason(source.NotMoved(source.AdmissionTimeout))
    == Ok("admission_timeout")
  assert move.abort_reason(source.NotMoved(source.AbortedEarly))
    == Ok("aborted_early")
  assert move.abort_reason(source.NotMoved(source.FreezeFailed("x")))
    == Ok("freeze_failed")
  list.each(rollout_metrics.frozen_abort_reasons, fn(reason) {
    assert list.contains(rollout_metrics.abort_reasons, reason)
  })
}

pub fn a_session_exit_is_a_crash_unless_normal_or_handed_off_test() {
  assert !socket.crashed(process.Normal)
  assert socket.crashed(process.Killed)
  assert socket.crashed(process.Abnormal(dynamic.string("boom")))
  assert !socket.crashed(
    process.Abnormal(
      dynamic.array([
        atom.to_dynamic(atom.create("shutdown")),
        atom.to_dynamic(atom.create("handoff")),
      ]),
    ),
  )
}

pub fn readiness_reasons_map_to_bounded_labels_test() {
  assert status.ready_reason_label("OK") == "ok"
  assert status.ready_reason_label("DRAINING") == "draining"
  assert status.ready_reason_label("JOINING") == "joining"
  assert status.ready_reason_label("NATS_UNAVAILABLE") == "nats_unavailable"
  assert status.ready_reason_label("EPOCH_MISMATCH local=5 peer=3")
    == "epoch_mismatch"
  assert status.ready_reason_label("anything else") == "other"
  let reasons =
    parse(rendered())
    |> list.filter(fn(line) { line.name == rollout_metrics.ready_reason })
  assert list.length(reasons) == list.length(rollout_metrics.ready_reasons)
  assert list.fold(reasons, 0, fn(sum, line) { sum + line.value }) == 1
}

pub fn seeded_series_exist_before_any_event_test() {
  let guilds = parse(render_as(node.Guilds))
  let has = fn(lines: List(Line), name, labels) {
    list.any(lines, fn(line) {
      line.name == name
      && list.all(labels, fn(label) { list.contains(line.labels, label) })
    })
  }
  assert has(guilds, rollout_metrics.moves_aborted, [
    #("kind", "guild"),
    #("reason", "fenced"),
  ])
  assert has(guilds, rollout_metrics.fences, [#("reason", "isolation_cap")])
  assert has(guilds, rollout_metrics.joins_expired, [])
  assert !has(guilds, rollout_metrics.session_rebinds, [])
  let sessions = parse(render_as(node.Sessions))
  assert has(sessions, rollout_metrics.rebind_fallbacks, [
    #("kind", "guild"),
    #("reason", "not_rebound"),
  ])
  assert has(sessions, rollout_metrics.session_moves, [#("outcome", "failed")])
  assert !has(sessions, rollout_metrics.moves_started, [])
  let ws = parse(render_as(node.Websocket))
  assert has(ws, rollout_metrics.invalid_session, [#("reason", "session_down")])
  assert has(ws, rollout_metrics.op7_sent, [#("reason", "drain")])
  let push = parse(render_as(node.Push))
  assert has(push, rollout_metrics.push_jobs_drain, [#("outcome", "lost")])
}

pub fn a_shed_guild_presence_broadcast_is_on_the_guild_status_port_test() {
  let count = fn() {
    parse(render_as(node.Guilds))
    |> list.filter(fn(line) {
      line.name == rollout_metrics.presence_broadcast_drops
    })
    |> list.map(fn(line) { line.value })
  }
  let assert [before] = count()
  rollout_metrics.count(rollout_metrics.presence_broadcast_drops, [], 1)
  assert count() == [before + 1]
  assert !list.any(parse(render_as(node.Sessions)), fn(line) {
    line.name == rollout_metrics.presence_broadcast_drops
  })
}

pub fn histograms_a_gate_reads_are_seeded_at_zero_test() {
  let guilds = parse(render_as(node.Guilds))
  let zero = fn(lines: List(Line), name, labels) {
    list.any(lines, fn(line) {
      line.name == name
      && line.value == 0
      && list.all(labels, fn(label) { list.contains(line.labels, label) })
    })
  }
  list.each(rollout_metrics.size_classes, fn(class) {
    assert zero(guilds, rollout_metrics.move_freeze <> "_count", [
      #("kind", "guild"),
      #("size_class", class),
    ])
  })
  assert zero(guilds, rollout_metrics.move_duration <> "_count", [
    #("kind", "guild"),
  ])
  assert zero(guilds, rollout_metrics.drain_duration <> "_count", [
    #("outcome", "failed"),
  ])
  let ws = parse(render_as(node.Websocket))
  assert !list.any(ws, fn(line) {
    line.name == rollout_metrics.move_freeze <> "_count"
  })
}

pub fn series_per_pod_stay_within_budget_test() {
  list.each(roles, fn(role) {
    let count = list.length(parse(render_as(role)))
    assert count <= 160
  })
}
