// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/job
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/rule
import gleam/list

const channel = 5

fn fresh() -> state.Outbox {
  state.new(state.default_limits(100_000), True)
}

fn message_job(user: Int, message: Int) -> job.Job {
  let assert Ok(built) =
    job.for_users(job.template([], channel:, message:), rule.Direct, [user])
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

fn retries(effects: List(state.Effect)) -> List(#(Int, Int)) {
  list.filter_map(effects, fn(effect) {
    case effect {
      state.ArmRetry(seq:, after_ms:) -> Ok(#(seq, after_ms))
      _ -> Error(Nil)
    }
  })
}

fn handed(effects: List(state.Effect)) -> List(state.Carried) {
  list.flat_map(effects, fn(effect) {
    case effect {
      state.HandOver(carried:, ..) -> carried
      _ -> []
    }
  })
}

fn target() -> state.Target {
  state.Target(node: "target@host", outbox: "outbox")
}

fn failed_once(user: Int) -> #(state.Outbox, Int) {
  let #(outbox, effects) = state.enqueue(fresh(), message_job(user, 10), 0)
  let assert [attempt] = starts(effects)
  let #(outbox, effects) = state.reply(outbox, attempt, state.Failed("x"), 10)
  let assert [#(seq, _)] = retries(effects)
  #(outbox, seq)
}

pub fn an_exported_job_is_never_sent_by_the_source_test() {
  let #(outbox, seq) = failed_once(1)
  let bucket = state.bucket_of(message_job(1, 10))
  let assert Ok(#(outbox, carried, _)) = state.export(outbox, bucket, 20)
  assert list.map(carried, fn(found) { found.job.users }) == [[1]]
  assert state.export(outbox, bucket, 20) == Error(Nil)
  let #(_, effects) = state.retry_due(outbox, seq, 2000)
  assert starts(effects) == []
}

pub fn a_job_admitted_while_frozen_waits_for_the_verdict_test() {
  let #(outbox, _) = failed_once(1)
  let bucket = state.bucket_of(message_job(1, 10))
  let assert Ok(#(outbox, carried, _)) = state.export(outbox, bucket, 20)
  let #(outbox, effects) = state.enqueue(outbox, message_job(2, 11), 30)
  assert starts(effects) == []
  assert handed(effects) == []
  let #(_, effects) = state.reclaim(outbox, bucket, carried, 40)
  assert list.length(starts(effects)) == 2
}

pub fn a_retry_handed_after_commit_keeps_its_backoff_test() {
  let #(outbox, effects) = state.enqueue(fresh(), message_job(3, 12), 0)
  let assert [attempt] = starts(effects)
  let bucket = state.bucket_of(message_job(3, 12))
  let assert Ok(#(outbox, [], _)) = state.export(outbox, bucket, 5)
  let #(outbox, effects) = state.reply(outbox, attempt, state.Failed("x"), 10)
  assert handed(effects) == []
  let #(outbox, effects) = state.hand(outbox, bucket, target())
  let assert [moved] = handed(effects)
  assert moved.attempts == 1
  assert moved.retry_in_ms > 0
  assert state.is_idle(outbox)
  let #(peer, effects) = state.adopt(fresh(), [moved], [], 100)
  assert starts(effects) == []
  let assert [#(seq, after_ms)] = retries(effects)
  assert after_ms == moved.retry_in_ms
  let #(_, effects) = state.retry_due(peer, seq, 100 + after_ms)
  assert list.length(starts(effects)) == 1
  let #(_, effects) = state.enqueue(outbox, message_job(4, 13), 30)
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[4]]
}

pub fn hand_all_moves_idle_jobs_and_every_later_one_test() {
  let #(outbox, _) = failed_once(1)
  let #(outbox, effects) = state.hand_all(outbox, target(), 50)
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[1]]
  assert state.is_idle(outbox)
  let #(outbox, effects) = state.enqueue(outbox, message_job(2, 14), 60)
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[2]]
  assert state.is_idle(outbox)
}

pub fn an_undrained_outbox_runs_later_jobs_here_and_hands_on_the_next_drain_test() {
  let #(outbox, _) = state.hand_all(fresh(), target(), 50)
  let outbox = state.undrained(outbox)
  let #(outbox, effects) = state.enqueue(outbox, message_job(1, 10), 60)
  assert handed(effects) == []
  let assert [attempt] = starts(effects)
  let #(outbox, effects) = state.reply(outbox, attempt, state.Failed("x"), 70)
  assert handed(effects) == []
  assert !state.is_idle(outbox)
  let #(outbox, effects) = state.hand_all(outbox, target(), 80)
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[1]]
  let #(outbox, effects) = state.enqueue(outbox, message_job(2, 11), 90)
  assert starts(effects) == []
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[2]]
  assert state.is_idle(outbox)
}

fn windows(effects: List(state.Effect)) -> List(#(Int, Int)) {
  list.filter_map(effects, fn(effect) {
    case effect {
      state.ArmWindowEnd(attempt:, after_ms:) -> Ok(#(attempt, after_ms))
      _ -> Error(Nil)
    }
  })
}

pub fn hand_all_bounds_an_in_flight_attempt_once_test() {
  let outbox = fresh()
  let window = state.default_limits(100_000).drain_window_ms
  let #(outbox, effects) = state.enqueue(outbox, message_job(1, 10), 0)
  let assert [attempt] = starts(effects)
  let #(outbox, effects) = state.hand_all(outbox, target(), 50)
  assert windows(effects) == [#(attempt, window)]
  assert handed(effects) == []
  let #(outbox, effects) = state.hand_all(outbox, target(), 60)
  assert windows(effects) == []
  let #(outbox, effects) = state.window_end(outbox, attempt, 50 + window)
  let assert [moved] = handed(effects)
  assert moved.job.users == [1]
  assert moved.attempts == 1
  assert state.is_idle(outbox)
}

pub fn an_attempt_delivered_while_handing_is_never_handed_test() {
  let #(outbox, effects) = state.enqueue(fresh(), message_job(1, 10), 0)
  let assert [attempt] = starts(effects)
  let #(outbox, _) = state.hand_all(outbox, target(), 50)
  let #(outbox, effects) = state.reply(outbox, attempt, state.Delivered, 70)
  assert handed(effects) == []
  let #(outbox, effects) = state.window_end(outbox, attempt, 100_000)
  assert effects == []
  assert state.is_idle(outbox)
}

pub fn jobs_adopted_or_reclaimed_while_handing_go_on_to_the_owner_test() {
  let #(outbox, seq) = failed_once(1)
  let bucket = state.bucket_of(message_job(1, 10))
  let assert Ok(#(outbox, carried, _)) = state.export(outbox, bucket, 20)
  let #(outbox, _) = state.hand_all(outbox, target(), 30)
  let #(outbox, effects) = state.reclaim(outbox, bucket, carried, 40)
  assert starts(effects) == []
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[1]]
  let #(outbox, effects) = state.adopt(outbox, carried, [], 50)
  assert starts(effects) == []
  assert list.map(handed(effects), fn(found) { found.job.users }) == [[1]]
  assert state.is_idle(outbox)
  let #(_, effects) = state.retry_due(outbox, seq, 5000)
  assert starts(effects) == []
}

pub fn jobs_kept_after_a_failed_hand_over_run_here_test() {
  let #(outbox, _) = failed_once(1)
  let #(outbox, effects) = state.hand_all(outbox, target(), 50)
  let carried = handed(effects)
  let #(outbox, effects) = state.keep(outbox, carried, 60)
  assert list.length(starts(effects)) == 1
  let #(_, effects) = state.enqueue(outbox, message_job(2, 11), 70)
  assert handed(effects) == []
  assert list.length(starts(effects)) == 1
}
