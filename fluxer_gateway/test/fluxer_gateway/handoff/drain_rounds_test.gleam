// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain_report
import fluxer_gateway/config/draining
import fluxer_gateway/core/clock
import fluxer_gateway/handoff/service
import gleam/erlang/process

type Counter

type Stored

type CounterOption {
  Signed(Bool)
}

@external(erlang, "atomics", "new")
fn counter_new(arity: Int, options: List(CounterOption)) -> Counter

@external(erlang, "atomics", "add_get")
fn add_get(counter: Counter, index: Int, by: Int) -> Int

@external(erlang, "atomics", "get")
fn get(counter: Counter, index: Int) -> Int

@external(erlang, "atomics", "put")
fn put(counter: Counter, index: Int, value: Int) -> Stored

pub fn drain_rounds_stop_at_undrain_test() {
  let calls = counter_new(2, [Signed(True)])
  let deadline = clock.monotonic_ms() + 60_000
  let done = process.new_subject()
  process.spawn(fn() {
    service.rounds(deadline, 10, fn() { get(calls, 2) == 0 }, fn(_) {
      case add_get(calls, 1, 1) {
        2 -> {
          let _ = put(calls, 2, 1)
          Nil
        }
        _ -> Nil
      }
      True
    })
    process.send(done, Nil)
  })
  let assert Ok(Nil) = process.receive(done, 2000)
  process.sleep(100)
  assert get(calls, 1) == 2
}

pub fn drain_rounds_stop_when_a_round_says_so_test() {
  let calls = counter_new(1, [Signed(True)])
  service.rounds(clock.monotonic_ms() + 60_000, 10, fn() { True }, fn(_) {
    add_get(calls, 1, 1) < 3
  })
  assert get(calls, 1) == 3
}

pub fn a_new_drain_retires_the_rounds_of_the_old_one_test() {
  draining.clear_draining()
  let first = drain_report.begin()
  draining.set_draining()
  assert service.same_drain(first)
  draining.clear_draining()
  assert !service.same_drain(first)
  let second = drain_report.begin()
  draining.set_draining()
  assert !service.same_drain(first)
  assert service.same_drain(second)
  draining.clear_draining()
}
