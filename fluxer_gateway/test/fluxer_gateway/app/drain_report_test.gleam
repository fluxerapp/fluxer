// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain_report.{Drained, Draining, Failed, Idle, Left}
import fluxer_gateway/app/health
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/rollout_metrics
import gleam/string

fn none_left() -> drain_report.Left {
  Left(entities: 0, buckets: 0)
}

fn moved(count: Int) -> Nil {
  metrics.counter(
    drain_report.handoff_moves,
    [#("ns", "1"), #("outcome", "moved")],
    count,
  )
}

fn session_failed(count: Int) -> Nil {
  metrics.counter(drain_report.session_moves, [#("outcome", "failed")], count)
}

fn start() -> Int {
  drain_report.reset()
  let serial = drain_report.begin()
  draining.set_draining()
  serial
}

fn stop() -> Nil {
  draining.clear_draining()
  drain_report.reset()
}

pub fn a_drain_counts_moves_and_failures_from_its_start_test() {
  moved(5)
  let serial = start()
  assert serial > 0
  let started = drain_report.current(Left(entities: 3, buckets: 2))
  assert started.phase == Draining
  assert started.moved == 0 && started.failed == 0
  assert started.left == 3 && started.buckets_left == 2
  assert started.started_at_ms > 0
  moved(2)
  session_failed(1)
  metrics.counter(drain_report.session_moves, [#("outcome", "moved")], 4)
  metrics.counter(
    drain_report.handoff_moves,
    [#("ns", "1"), #("outcome", "thawed")],
    1,
  )
  drain_report.entity_failed("session", "s1")
  drain_report.entity_failed("guild", "7")
  let running = drain_report.current(Left(entities: 1, buckets: 1))
  assert running.moved == 6
  assert running.failed == 2
  assert !running.fenced
  stop()
}

pub fn an_entity_refused_then_moved_is_not_counted_as_failed_test() {
  let _ = start()
  let kind = [#("kind", "guild"), #("outcome", "failed")]
  let before = rollout_metrics.total(rollout_metrics.drain_entities, kind)
  metrics.counter(
    drain_report.handoff_moves,
    [#("ns", "0"), #("outcome", "not_moved")],
    2,
  )
  drain_report.entity_failed("guild", "41")
  drain_report.entity_failed("guild", "41")
  assert drain_report.current(none_left()).failed == 1
  drain_report.entity_moved("guild", "41")
  moved(1)
  assert drain_report.current(none_left()).failed == 0
  let done = drain_report.settle(none_left(), True, False)
  assert done.phase == Drained
  assert done.failed == 0
  assert rollout_metrics.total(rollout_metrics.drain_entities, kind) == before
  stop()
}

pub fn an_entity_still_failed_at_the_end_is_counted_once_test() {
  let _ = start()
  let kind = [#("kind", "guild"), #("outcome", "failed")]
  let before = rollout_metrics.total(rollout_metrics.drain_entities, kind)
  drain_report.entity_failed("guild", "42")
  drain_report.entity_failed("guild", "42")
  let done = drain_report.settle(Left(entities: 1, buckets: 0), False, True)
  assert done.phase == Failed
  assert done.failed == 1
  assert rollout_metrics.total(rollout_metrics.drain_entities, kind)
    == before + 1
  stop()
}

pub fn a_drain_that_empties_the_node_ends_drained_and_is_kept_as_the_last_test() {
  let serial = start()
  assert drain_report.settle(Left(entities: 1, buckets: 0), True, False).phase
    == Draining
  assert drain_report.settle(none_left(), False, False).phase == Draining
  let done = drain_report.settle(none_left(), True, False)
  assert done.phase == Drained
  assert done.reason == ""
  assert done.serial == serial
  assert done.finished_at_ms >= done.started_at_ms
  assert drain_report.settle(Left(entities: 9, buckets: 9), False, True) == done
  assert drain_report.last() == done
  assert drain_report.begin() == serial
  stop()
  assert drain_report.current(none_left()).phase == Idle
  assert drain_report.last() == done
}

pub fn a_drain_that_still_holds_work_at_the_deadline_fails_test() {
  let _ = start()
  session_failed(2)
  drain_report.entity_failed("session", "d1")
  drain_report.entity_failed("session", "d2")
  let failed = drain_report.settle(Left(entities: 4, buckets: 1), False, True)
  assert failed.phase == Failed
  assert failed.reason == "deadline"
  assert failed.left == 4
  assert failed.buckets_left == 1
  assert failed.failed == 2
  assert drain_report.last() == failed
  assert drain_report.settle(none_left(), True, False).phase == Failed
  assert drain_report.begin() == failed.serial
  assert drain_report.current(none_left()).phase == Failed
  stop()
}

pub fn a_fence_during_the_drain_fails_it_even_when_nothing_is_left_test() {
  let _ = start()
  metrics.counter(drain_report.fences, [], 1)
  assert drain_report.current(none_left()).fenced
  let failed = drain_report.settle(none_left(), True, False)
  assert failed.phase == Failed
  assert failed.reason == "fenced"
  assert failed.fenced
  stop()
}

pub fn a_new_drain_after_a_restart_of_draining_starts_a_fresh_report_test() {
  let first = start()
  let _ = drain_report.settle(Left(entities: 1, buckets: 0), False, True)
  draining.clear_draining()
  let second = drain_report.begin()
  assert second != first
  assert drain_report.current(none_left()).phase == Draining
  assert drain_report.last().serial == first
  stop()
}

pub fn drained_answers_follow_the_outcome_test() {
  assert health.drained_status(Drained) == #(200, "DRAINED")
  assert health.drained_status(Failed) == #(503, "DRAIN_FAILED")
  assert health.drained_status(Draining) == #(503, "DRAINING")
  assert health.drained_status(Idle) == #(503, "NOT_DRAINING")
}

pub fn an_idle_pod_does_not_claim_to_be_draining_test() {
  drain_report.reset()
  let idle = drain_report.current(none_left())
  assert idle.phase == Idle
  assert health.drained_reply(idle, True, 0, 1_000_000)
    == #(503, "NOT_DRAINING")
  assert health.drained_reply(idle, False, 0, 0) == #(503, "NOT_DRAINING")
  let _ = start()
  assert health.drained_reply(
      drain_report.current(none_left()),
      False,
      0,
      clock.system_ms(),
    )
    == #(503, "DRAINING")
  let _ = drain_report.settle(none_left(), True, False)
  stop()
  let undrained = drain_report.current(none_left())
  assert undrained.phase == Idle
  assert health.drained_reply(undrained, True, 0, clock.system_ms())
    == #(503, "NOT_DRAINING")
}

fn push_jobs(outcome: String, count: Int) -> Nil {
  rollout_metrics.count(
    rollout_metrics.push_jobs_drain,
    [#("outcome", outcome)],
    count,
  )
}

fn with_role(role: node.Role, run: fn() -> Nil) -> Nil {
  let before = settings.get()
  settings.store(settings.Settings(..before, role:))
  run()
  settings.store(before)
  Nil
}

pub fn a_push_drain_reports_the_push_jobs_of_that_drain_test() {
  use <- with_role(node.Push)
  push_jobs("moved", 3)
  push_jobs("lost", 1)
  let _ = start()
  push_jobs("moved", 8)
  push_jobs("finished", 2)
  let report = drain_report.settle(none_left(), True, False)
  assert report.phase == Drained
  assert report.push_jobs
    == drain_report.PushJobs(moved: 8, lost: 0, finished: 2)
  assert drain_report.last().push_jobs == report.push_jobs
  let text = drain_report.summary(report, node.Push)
  assert string.ends_with(
    text,
    " push_jobs_moved=8 push_jobs_lost=0 push_jobs_finished=2",
  )
  assert string.starts_with(text, " moved=0 failed=0 left=0 buckets_left=0 ")
  assert drain_report.summary(report, node.Calls) == text
  assert drain_report.summary(report, node.AllRoles) == text
  assert !string.contains(drain_report.summary(report, node.Guilds), "push")
  assert !string.contains(
    drain_report.summary(report, node.Sessions),
    "push_jobs",
  )
  stop()
}

pub fn a_failed_push_drain_reports_the_lost_push_jobs_test() {
  use <- with_role(node.Calls)
  let _ = start()
  push_jobs("moved", 4)
  push_jobs("lost", 5)
  let report = drain_report.settle(Left(entities: 5, buckets: 0), False, True)
  assert report.phase == Failed
  assert report.push_jobs
    == drain_report.PushJobs(moved: 4, lost: 5, finished: 0)
  assert string.contains(
    drain_report.summary(report, node.Calls),
    " push_jobs_moved=4 push_jobs_lost=5 push_jobs_finished=0",
  )
  stop()
}

pub fn a_finished_drain_waits_for_one_scrape_before_it_answers_test() {
  let report =
    drain_report.Report(
      ..drain_report.idle(),
      phase: Drained,
      finished_at_ms: 10_000,
    )
  assert health.drained_reply(report, True, 9000, 12_000) == #(503, "DRAINING")
  assert health.drained_reply(report, True, 11_000, 12_000) == #(200, "DRAINED")
  assert health.drained_reply(
      report,
      True,
      9000,
      10_000 + health.scrape_hold_ms,
    )
    == #(200, "DRAINED")
  assert health.drained_reply(report, False, 0, 10_001) == #(200, "DRAINED")
  let failed = drain_report.Report(..report, phase: Failed)
  assert health.drained_reply(failed, True, 9000, 12_000) == #(503, "DRAINING")
  assert health.drained_reply(failed, True, 11_000, 12_000)
    == #(503, "DRAIN_FAILED")
  let draining = drain_report.Report(..report, phase: Draining)
  assert health.drained_reply(draining, True, 11_000, 12_000)
    == #(503, "DRAINING")
}

pub fn a_metrics_scrape_is_remembered_test() {
  rollout_metrics.mark_scraped(4242)
  assert rollout_metrics.last_scrape_ms() == 4242
}

pub fn phases_have_stable_names_and_codes_test() {
  assert drain_report.phase_name(Idle) == "idle"
  assert drain_report.phase_name(Draining) == "draining"
  assert drain_report.phase_name(Drained) == "drained"
  assert drain_report.phase_name(Failed) == "failed"
  assert drain_report.phase_code(Idle) == 0
  assert drain_report.phase_code(Draining) == 1
  assert drain_report.phase_code(Drained) == 2
  assert drain_report.phase_code(Failed) == 3
}
