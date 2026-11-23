// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/job
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import gleam/int
import gleam/list
import gleam/option.{None}

fn limits() -> state.Limits {
  state.default_limits(100_000)
}

fn message_job(class: rule.Class, users: List(Int), message: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel: 5, message:), class, users)
  built
}

fn starts(effects: List(state.Effect)) -> List(Int) {
  list.filter_map(effects, fn(effect) {
    case effect {
      state.Start(attempt:, ..) -> Ok(attempt)
      _ -> Error(Nil)
    }
  })
}

fn fill_slots(
  outbox: state.Outbox,
  count: Int,
  class: rule.Class,
) -> state.Outbox {
  int.range(1, count + 1, outbox, fn(outbox, index) {
    let #(outbox, _) =
      state.enqueue(outbox, message_job(class, [index], index), 0)
    outbox
  })
}

pub fn direct_starts_while_bulk_waits_above_56_slots_test() {
  let outbox = fill_slots(state.new(limits(), True), 56, rule.Bulk)
  let #(outbox, effects) =
    state.enqueue(outbox, message_job(rule.Bulk, [900], 900), 0)
  let assert [] = starts(effects)
  let #(_, effects) =
    state.enqueue(outbox, message_job(rule.Direct, [901], 901), 0)
  let assert [_] = starts(effects)
}

pub fn slot_frees_at_the_deadline_test() {
  let outbox = state.new(limits(), True)
  let #(outbox, effects) =
    state.enqueue(outbox, message_job(rule.Direct, [1], 1), 0)
  let assert [attempt] = starts(effects)
  let outbox = fill_slots(outbox, 63, rule.Direct)
  let #(outbox, effects) =
    state.enqueue(outbox, message_job(rule.Direct, [2], 2), 0)
  let assert [] = starts(effects)
  let #(outbox, effects) = state.slot_deadline(outbox, attempt, 5000)
  let assert [_] = starts(effects)
  let assert True = state.has_attempt(outbox, attempt)
}

pub fn bulk_expires_at_30_seconds_test() {
  let outbox = fill_slots(state.new(limits(), True), 56, rule.Bulk)
  let #(outbox, _) =
    state.enqueue(outbox, message_job(rule.Bulk, [700], 700), 0)
  let #(_, effects) = state.pump(outbox, 29_999)
  let assert [] = starts(effects)
  let assert False =
    list.any(effects, fn(effect) {
      case effect {
        state.Drop(reason: "expired", ..) -> True
        _ -> False
      }
    })
}

pub fn read_truncates_queued_jobs_test() {
  let outbox = fill_slots(state.new(limits(), True), 64, rule.Direct)
  let #(outbox, _) =
    state.enqueue(outbox, message_job(rule.Direct, [42, 43], 100), 0)
  let #(_, effects) =
    state.truncate_read(outbox, user: 42, channel: 5, message: 100, now_ms: 1)
  let assert True =
    list.any(effects, fn(effect) {
      case effect {
        state.Count(name: "truncations_total", ..) -> True
        _ -> False
      }
    })
}

pub fn in_flight_reply_waits_at_least_five_seconds_test() {
  let outbox = state.new(limits(), True)
  let #(outbox, effects) =
    state.enqueue(outbox, message_job(rule.Direct, [1], 1), 0)
  let assert [attempt] = starts(effects)
  let #(_, effects) = state.reply(outbox, attempt, state.InFlightElsewhere, 10)
  let assert True =
    list.any(effects, fn(effect) {
      case effect {
        state.ArmRetry(after_ms:, ..) -> after_ms >= 5000
        _ -> False
      }
    })
}

pub fn clear_jobs_are_shed_last_test() {
  let small = state.Limits(..limits(), max_records: 1, slots: 0, bulk_slots: 0)
  let outbox = state.new(small, True)
  let #(outbox, _) =
    state.enqueue(
      outbox,
      job.clear_job(user: 1, channel: 5, message: 9, after: None),
      0,
    )
  let #(outbox, effects) =
    state.enqueue(outbox, message_job(rule.Bulk, [2], 10), 0)
  let assert True =
    list.any(effects, fn(effect) {
      case effect {
        state.Drop(kind: job.MessageJob, reason: "shed", ..) -> True
        _ -> False
      }
    })
  let assert 1 = state.record_count(outbox)
}
