// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set

pub fn max_age_ms() -> Int {
  durations.guild().admission_max_age_ms
}

const concurrency_limit = 2

const queue_limit = 64

pub type Tracked(p) {
  Tracked(plan: p, admitted_at_ms: Int)
}

pub opaque type Admission(p) {
  Admission(tracked: List(Tracked(p)), last: Option(p))
}

pub type Verdict(p) {
  Run
  Wait(predecessor: p)
  Drop
}

pub type Counts {
  Counts(
    queued: Int,
    dropped_at_limit: Int,
    slot_reclaimed: Int,
    slot_deduped: Int,
    worker_started: Int,
  )
}

pub type Limits {
  Limits(concurrency: Int, queue: Int)
}

pub fn new() -> Admission(p) {
  Admission(tracked: [], last: None)
}

pub fn default_limits() -> Limits {
  Limits(concurrency: concurrency_limit, queue: queue_limit)
}

fn no_counts() -> Counts {
  Counts(0, 0, 0, 0, 0)
}

fn merge_last(admission: Admission(p), now_ms: Int) -> List(Tracked(p)) {
  case admission.last {
    None -> admission.tracked
    Some(plan) ->
      case list.any(admission.tracked, fn(tracked) { tracked.plan == plan }) {
        True -> admission.tracked
        False -> list.append(admission.tracked, [Tracked(plan, now_ms)])
      }
  }
}

fn prune(
  tracked: List(Tracked(p)),
  now_ms: Int,
  is_live: fn(p) -> Bool,
  counts: Counts,
) -> #(List(Tracked(p)), Counts) {
  let #(kept, _, counts) =
    list.fold(tracked, #([], set.new(), counts), fn(acc, item) {
      let #(kept, seen, counts) = acc
      case set.contains(seen, item.plan), is_live(item.plan) {
        True, _ -> #(
          kept,
          seen,
          Counts(..counts, slot_deduped: counts.slot_deduped + 1),
        )
        False, False -> #(kept, seen, counts)
        False, True ->
          case now_ms - item.admitted_at_ms < max_age_ms() {
            True -> #([item, ..kept], set.insert(seen, item.plan), counts)
            False -> #(
              kept,
              seen,
              Counts(..counts, slot_reclaimed: counts.slot_reclaimed + 1),
            )
          }
      }
    })
  #(list.reverse(kept), counts)
}

pub fn admit(
  admission: Admission(p),
  now_ms now_ms: Int,
  limits limits: Limits,
  is_live is_live: fn(p) -> Bool,
) -> #(Verdict(p), List(Tracked(p)), Counts) {
  let #(pruned, counts) =
    prune(merge_last(admission, now_ms), now_ms, is_live, no_counts())
  let count = list.length(pruned)
  case count < limits.concurrency {
    True -> #(Run, pruned, counts)
    False ->
      case
        count - limits.concurrency < limits.queue,
        nth(pruned, limits.concurrency)
      {
        True, Ok(predecessor) -> #(
          Wait(predecessor.plan),
          pruned,
          Counts(..counts, queued: counts.queued + 1),
        )
        _, _ -> #(
          Drop,
          pruned,
          Counts(..counts, dropped_at_limit: counts.dropped_at_limit + 1),
        )
      }
  }
}

pub fn note_started(
  pruned: List(Tracked(p)),
  plan: p,
  now_ms: Int,
) -> Admission(p) {
  Admission(tracked: [Tracked(plan, now_ms), ..pruned], last: Some(plan))
}

pub fn note_dropped(
  admission: Admission(p),
  pruned: List(Tracked(p)),
) -> Admission(p) {
  Admission(..admission, tracked: pruned)
}

fn nth(items: List(a), position: Int) -> Result(a, Nil) {
  case position < 1 {
    True -> Error(Nil)
    False ->
      items
      |> list.drop(position - 1)
      |> list.first
  }
}

pub fn in_flight(
  admission: Admission(p),
  admitted_by admitted_by_ms: Int,
  now_ms now_ms: Int,
  is_live is_live: fn(p) -> Bool,
) -> Bool {
  list.any(admission.tracked, fn(item) {
    item.admitted_at_ms <= admitted_by_ms
    && now_ms - item.admitted_at_ms < 3 * max_age_ms()
    && is_live(item.plan)
  })
}

pub fn released(
  waiting_since_ms: Int,
  now_ms: Int,
  predecessor_live: Bool,
) -> Bool {
  !predecessor_live || now_ms - waiting_since_ms >= max_age_ms()
}
