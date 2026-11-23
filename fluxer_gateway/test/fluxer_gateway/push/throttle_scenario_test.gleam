// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, PushDurations}
import fluxer_gateway/core/clock
import fluxer_gateway/push/throttle
import gleam/erlang/process
import gleam/int
import gleam/list
import support/isolation
import support/push_throttle

pub fn an_outage_keeps_a_trickle_and_accepts_restore_admission_test() {
  push_throttle.calm(throttle.ReadState)
  push_throttle.saturate(throttle.ReadState)
  let admitted = push_throttle.admitted(throttle.ReadState, 2000, 0)
  assert admitted >= 100 && admitted <= 500
  push_throttle.calm(throttle.ReadState)
  assert push_throttle.admitted(throttle.ReadState, 500, 0) == 500
}

pub fn request_types_are_throttled_independently_test() {
  push_throttle.calm(throttle.AdultContent)
  push_throttle.calm(throttle.ReadState)
  push_throttle.saturate(throttle.AdultContent)
  assert push_throttle.admitted(throttle.ReadState, 500, 0) == 500
  assert push_throttle.admitted(throttle.AdultContent, 1000, 0) < 400
  push_throttle.calm(throttle.AdultContent)
}

pub fn concurrent_offers_and_accepts_stay_admitted_test() {
  push_throttle.calm(throttle.ReadState)
  let done = process.new_subject()
  list.each(int.range(0, 32, [], fn(acc, worker) { [worker, ..acc] }), fn(_) {
    process.spawn(fn() {
      let rejected =
        int.range(0, 2000, 0, fn(rejected, _) {
          case throttle.offer(throttle.ReadState) {
            throttle.Admitted -> {
              throttle.record_accept(throttle.ReadState)
              rejected
            }
            throttle.Rejected -> rejected + 1
          }
        })
      process.send(done, rejected)
    })
  })
  let rejected =
    int.range(0, 32, 0, fn(total, _) {
      let assert Ok(count) = process.receive(done, 4000)
      total + count
    })
  assert rejected == 0
}

pub fn no_offer_is_lost_when_buckets_rotate_under_contention_test() {
  isolation.set_durations(fn(durations) {
    Durations(
      ..durations,
      push: PushDurations(..durations.push, throttle_bucket_ms: 40),
    )
  })
  throttle.reset()
  let done = process.new_subject()
  let stop_at = clock.monotonic_ms() + 300
  let workers = 32
  int.range(0, workers, Nil, fn(_, _) {
    let _ = process.spawn(fn() { process.send(done, offer_until(stop_at, 0)) })
    Nil
  })
  let offered =
    int.range(0, workers, 0, fn(total, _) {
      let assert Ok(count) = process.receive(done, 4000)
      total + count
    })
  let #(requests, _) = throttle.counted(throttle.ReadState)
  throttle.reset()
  assert requests == offered
}

fn offer_until(stop_at: Int, count: Int) -> Int {
  case clock.monotonic_ms() >= stop_at {
    True -> count
    False -> {
      let _ = throttle.offer(throttle.ReadState)
      offer_until(stop_at, count + 1)
    }
  }
}
