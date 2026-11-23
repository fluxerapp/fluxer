// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/push/admission

fn live(_plan: Int) -> Bool {
  True
}

fn admit(a: admission.Admission(Int), plan: Int, now: Int) {
  let #(verdict, pruned, counts) =
    admission.admit(
      a,
      now_ms: now,
      limits: admission.default_limits(),
      is_live: live,
    )
  case verdict {
    admission.Drop -> #(verdict, admission.note_dropped(a, pruned), counts)
    _ -> #(verdict, admission.note_started(pruned, plan, now), counts)
  }
}

pub fn runs_two_then_waits_on_the_second_newest_test() {
  let a = admission.new()
  let assert #(admission.Run, a, _) = admit(a, 1, 0)
  let assert #(admission.Run, a, _) = admit(a, 2, 0)
  let #(verdict, _, counts) = admit(a, 3, 0)
  let assert admission.Wait(1) = verdict
  let assert 1 = counts.queued
}

pub fn reclaims_at_60_seconds_not_before_test() {
  let a = admission.new()
  let assert #(admission.Run, a, _) = admit(a, 1, 0)
  let assert #(admission.Run, a, _) = admit(a, 2, 0)
  let assert #(admission.Wait(_), _, _) = admit(a, 3, 59_999)
  let assert #(admission.Run, _, counts) = admit(a, 3, 60_000)
  let assert 2 = counts.slot_reclaimed
}

pub fn drops_beyond_the_queue_test() {
  let full =
    admission.admit(
      admission.new(),
      now_ms: 0,
      limits: admission.Limits(concurrency: 1, queue: 0),
      is_live: live,
    )
  let assert #(admission.Run, pruned, _) = full
  let a = admission.note_started(pruned, 1, 0)
  let assert #(admission.Drop, _, counts) =
    admission.admit(
      a,
      now_ms: 1,
      limits: admission.Limits(concurrency: 1, queue: 0),
      is_live: live,
    )
  let assert 1 = counts.dropped_at_limit
}

pub fn escape_after_60_seconds_test() {
  let assert False = admission.released(0, 59_999, True)
  let assert True = admission.released(0, 60_000, True)
  let assert True = admission.released(0, 1, False)
}

pub fn in_flight_counts_only_live_workers_admitted_in_time_test() {
  let a = admission.new()
  let assert #(admission.Run, a, _) = admit(a, 1, 1000)
  let assert #(admission.Run, a, _) = admit(a, 2, 5000)
  let only = fn(wanted) { fn(plan) { plan == wanted } }
  assert admission.in_flight(a, admitted_by: 5000, now_ms: 6000, is_live: live)
  assert admission.in_flight(
    a,
    admitted_by: 4999,
    now_ms: 6000,
    is_live: only(1),
  )
  assert !admission.in_flight(
    a,
    admitted_by: 4999,
    now_ms: 6000,
    is_live: only(2),
  )
  assert !admission.in_flight(a, admitted_by: 999, now_ms: 6000, is_live: live)
  assert !admission.in_flight(
    a,
    admitted_by: 5000,
    now_ms: 185_000,
    is_live: live,
  )
}
