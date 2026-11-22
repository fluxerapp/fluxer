// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/api/breaker
import gleam/list

fn record_many(
  start: breaker.Breaker,
  outcome: breaker.Outcome,
  count: Int,
  now: Int,
) -> breaker.Breaker {
  list.fold(list.repeat(outcome, count), start, fn(acc, item) {
    breaker.record(acc, item, now, 5000)
  })
}

pub fn stays_closed_below_min_calls_test() {
  let tested =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Failure, 19, 1000)
  assert breaker.state(tested) == breaker.Closed
}

pub fn opens_on_majority_failures_test() {
  let tested =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Success, 9, 1000)
    |> record_many(breaker.Failure, 11, 1000)
  assert breaker.state(tested) == breaker.Open(6000)
}

pub fn half_failures_do_not_open_test() {
  let tested =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Success, 10, 1000)
    |> record_many(breaker.Failure, 10, 1000)
  assert breaker.state(tested) == breaker.Closed
}

pub fn old_buckets_leave_the_window_test() {
  let tested =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Failure, 15, 1000)
    |> record_many(breaker.Failure, 10, 20_000)
  assert breaker.state(tested) == breaker.Closed
}

pub fn half_open_then_close_test() {
  let opened =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Failure, 20, 1000)
  assert breaker.state(breaker.expire(opened, 5999)) == breaker.Open(6000)
  let half = breaker.expire(opened, 6000)
  assert breaker.state(half) == breaker.HalfOpen(0)
  let closed = record_many(half, breaker.Success, 4, 6100)
  assert breaker.state(closed) == breaker.Closed
}

pub fn half_open_failure_reopens_test() {
  let half =
    breaker.new(breaker.default_config())
    |> record_many(breaker.Failure, 20, 1000)
    |> breaker.expire(6000)
    |> breaker.record(breaker.Success, 6100, 5000)
    |> breaker.record(breaker.Failure, 6200, 5000)
  assert breaker.state(half) == breaker.Open(11_200)
}

pub fn failure_threshold_holds_the_breaker_closed_test() {
  let config = breaker.Config(..breaker.default_config(), min_failures: 30)
  let held =
    breaker.new(config)
    |> record_many(breaker.Failure, 29, 1000)
  assert breaker.state(held) == breaker.Closed
  let opened = record_many(held, breaker.Failure, 1, 1000)
  assert breaker.state(opened) == breaker.Open(6000)
}

pub fn reset_keeps_the_config_test() {
  let config = breaker.Config(..breaker.default_config(), min_failures: 30)
  let reset =
    breaker.new(config)
    |> record_many(breaker.Failure, 29, 1000)
    |> breaker.reset
    |> record_many(breaker.Failure, 29, 2000)
  assert breaker.state(reset) == breaker.Closed
}
