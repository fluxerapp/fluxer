// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/draining
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Role}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import fluxer_gateway/util/table
import gleam/int
import gleam/list
import logging

pub type Phase {
  Idle
  Draining
  Drained
  Failed
}

pub type Left {
  Left(entities: Int, buckets: Int)
}

pub type PushJobs {
  PushJobs(moved: Int, lost: Int, finished: Int)
}

pub type Report {
  Report(
    serial: Int,
    phase: Phase,
    started_at_ms: Int,
    finished_at_ms: Int,
    moved: Int,
    failed: Int,
    left: Int,
    buckets_left: Int,
    fenced: Bool,
    reason: String,
    push_jobs: PushJobs,
  )
}

type Counts {
  Counts(moved: Int, fences: Int, push_jobs: PushJobs)
}

type Stored {
  Stored(current: Report, last: Report, baseline: Counts)
}

type Key {
  FluxerGatewayDrainReport
}

type Put

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Stored) -> Put

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Stored) -> Stored

@external(erlang, "erlang", "unique_integer")
fn unique_integer(modifiers: List(Modifier)) -> Int

type Modifier {
  Positive
  Monotonic
}

pub const handoff_moves = "fluxer_gateway_handoff_moves_total"

pub const session_moves = "fluxer_gateway_session_moves_total"

pub const fences = "fluxer_gateway_bouncer_fences_total"

pub fn idle() -> Report {
  Report(
    serial: 0,
    phase: Idle,
    started_at_ms: 0,
    finished_at_ms: 0,
    moved: 0,
    failed: 0,
    left: 0,
    buckets_left: 0,
    fenced: False,
    reason: "",
    push_jobs: no_push_jobs(),
  )
}

fn no_push_jobs() -> PushJobs {
  PushJobs(moved: 0, lost: 0, finished: 0)
}

fn load() -> Stored {
  get_term(
    FluxerGatewayDrainReport,
    Stored(
      current: idle(),
      last: idle(),
      baseline: Counts(0, 0, no_push_jobs()),
    ),
  )
}

fn store(stored: Stored) -> Nil {
  put_term(FluxerGatewayDrainReport, stored)
  Nil
}

fn outcome_is(labels: List(#(String, String)), wanted: String) -> Bool {
  list.key_find(labels, "outcome") == Ok(wanted)
}

fn counts() -> Counts {
  let moved = fn(labels) { outcome_is(labels, "moved") }
  Counts(
    moved: metrics.total(handoff_moves, moved)
      + metrics.total(session_moves, moved),
    fences: metrics.total(fences, fn(_) { True }),
    push_jobs: PushJobs(
      moved: push_jobs_of("moved"),
      lost: push_jobs_of("lost"),
      finished: push_jobs_of("finished"),
    ),
  )
}

fn push_jobs_of(outcome: String) -> Int {
  metrics.total(rollout_metrics.push_jobs_drain, fn(labels) {
    outcome_is(labels, outcome)
  })
}

fn failures() -> table.Table {
  table.named("fluxer_gateway_drain_failed_entities")
}

type AnyId

fn failed_rows() -> List(#(#(String, AnyId), Nil)) {
  case table.ensure_public(failures()) {
    True -> table.to_list(failures())
    False -> []
  }
}

pub fn entity_failed(kind: String, id: key) -> Nil {
  case draining.is_draining() && table.ensure_public(failures()) {
    True -> table.insert(failures(), #(#(kind, id), Nil))
    False -> Nil
  }
}

pub fn entity_moved(kind: String, id: key) -> Nil {
  case table.ensure_public(failures()) {
    True -> table.delete(failures(), #(kind, id))
    False -> Nil
  }
}

pub fn failed_of(kind: String) -> Int {
  list.count(failed_rows(), fn(row) { row.0.0 == kind })
}

fn clear_failures() -> Nil {
  list.each(failed_rows(), fn(row) { table.delete(failures(), row.0) })
}

pub fn begin() -> Int {
  let stored = load()
  case draining.is_draining(), stored.current.phase {
    False, _ | True, Idle -> {
      let serial = unique_integer([Positive, Monotonic])
      clear_failures()
      store(
        Stored(
          ..stored,
          current: Report(
            ..idle(),
            serial:,
            phase: Draining,
            started_at_ms: clock.system_ms(),
          ),
          baseline: counts(),
        ),
      )
      logging.log(logging.Info, "Gateway drain started")
      serial
    }
    True, Draining | True, Drained | True, Failed -> stored.current.serial
  }
}

pub fn serial() -> Int {
  load().current.serial
}

pub fn current(left: Left) -> Report {
  live(load(), left)
}

pub fn last() -> Report {
  load().last
}

fn live(stored: Stored, left: Left) -> Report {
  case stored.current.phase {
    Draining -> {
      let now = counts()
      Report(
        ..stored.current,
        moved: now.moved - stored.baseline.moved,
        failed: list.length(failed_rows()),
        left: left.entities,
        buckets_left: left.buckets,
        fenced: now.fences > stored.baseline.fences,
        push_jobs: PushJobs(
          moved: now.push_jobs.moved - stored.baseline.push_jobs.moved,
          lost: now.push_jobs.lost - stored.baseline.push_jobs.lost,
          finished: now.push_jobs.finished - stored.baseline.push_jobs.finished,
        ),
      )
    }
    Idle | Drained | Failed -> stored.current
  }
}

pub fn settle(left: Left, done: Bool, final: Bool) -> Report {
  let stored = load()
  let report = live(stored, left)
  let empty = left.entities == 0 && left.buckets == 0
  case report.phase, report.fenced, done && empty, final {
    Draining, True, _, _ -> finish(stored, report, Failed, "fenced")
    Draining, False, True, _ -> finish(stored, report, Drained, "")
    Draining, False, False, True -> finish(stored, report, Failed, "deadline")
    _, _, _, _ -> report
  }
}

fn finish(
  stored: Stored,
  report: Report,
  phase: Phase,
  reason: String,
) -> Report {
  let report =
    Report(..report, phase:, reason:, finished_at_ms: clock.system_ms())
  store(Stored(..stored, current: report, last: report))
  log(report)
  record(report)
  report
}

fn recorded() -> table.Table {
  table.named("fluxer_gateway_drain_recorded")
}

fn record(report: Report) -> Nil {
  case
    table.ensure_public(recorded())
    && table.insert_new(recorded(), #(report.serial, report.phase))
  {
    True -> record_once(report)
    False -> Nil
  }
}

fn record_once(report: Report) -> Nil {
  failed_rows()
  |> list.map(fn(row) { row.0.0 })
  |> list.unique
  |> list.each(fn(kind) {
    rollout_metrics.count(
      rollout_metrics.drain_entities,
      [#("kind", kind), #("outcome", "failed")],
      failed_of(kind),
    )
  })
  let outcome = phase_name(report.phase)
  let reason = case report.reason {
    "" -> "none"
    found -> found
  }
  rollout_metrics.count(
    rollout_metrics.drain_outcomes,
    [#("outcome", outcome), #("reason", reason)],
    1,
  )
  rollout_metrics.observe(
    rollout_metrics.drain_duration,
    [#("outcome", outcome)],
    report.finished_at_ms - report.started_at_ms,
  )
}

fn log(report: Report) -> Nil {
  let counts = summary(report, settings.get().role)
  case report.phase {
    Failed ->
      logging.log(
        logging.Error,
        "Gateway drain failed: reason=" <> report.reason <> counts,
      )
    Idle | Draining | Drained ->
      logging.log(logging.Info, "Gateway drain drained:" <> counts)
  }
}

pub fn summary(report: Report, role: Role) -> String {
  " moved="
  <> int.to_string(report.moved)
  <> " failed="
  <> int.to_string(report.failed)
  <> " left="
  <> int.to_string(report.left)
  <> " buckets_left="
  <> int.to_string(report.buckets_left)
  <> " duration_ms="
  <> int.to_string(report.finished_at_ms - report.started_at_ms)
  <> push_summary(report.push_jobs, role)
}

fn push_summary(jobs: PushJobs, role: Role) -> String {
  case node.runs(role, node.Push) || node.runs(role, node.Calls) {
    True ->
      " push_jobs_moved="
      <> int.to_string(jobs.moved)
      <> " push_jobs_lost="
      <> int.to_string(jobs.lost)
      <> " push_jobs_finished="
      <> int.to_string(jobs.finished)
    False -> ""
  }
}

pub fn reset() -> Nil {
  let stored = load()
  store(Stored(..stored, current: idle()))
}

pub fn phase_name(phase: Phase) -> String {
  case phase {
    Idle -> "idle"
    Draining -> "draining"
    Drained -> "drained"
    Failed -> "failed"
  }
}

pub fn phase_code(phase: Phase) -> Int {
  case phase {
    Idle -> 0
    Draining -> 1
    Drained -> 2
    Failed -> 3
  }
}
