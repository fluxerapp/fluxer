// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/guild/manager/fetch
import gleam/dict
import gleam/dynamic
import gleam/erlang/process

pub fn gather_collects_every_result_test() {
  let outcome =
    fetch.gather(
      ["a", "b"],
      fn(name) { Ok(dynamic.string(name)) },
      clock.deadline_in(5000),
    )
  assert outcome
    == Ok(
      dict.from_list([#("a", dynamic.string("a")), #("b", dynamic.string("b"))]),
    )
}

pub fn gather_kills_running_workers_after_a_failure_test() {
  let started = process.new_subject()
  let outcome =
    fetch.gather(
      ["slow", "broken"],
      fn(name) {
        case name {
          "slow" -> {
            process.send(started, process.self())
            process.sleep_forever()
            Ok(dynamic.nil())
          }
          _ -> {
            process.sleep(50)
            Error(fetch.BadCollection(name))
          }
        }
      },
      clock.deadline_in(5000),
    )
  assert outcome == Error(fetch.BadCollection("broken"))
  let assert Ok(slow) = process.receive(started, 1000)
  assert !process.is_alive(slow)
}

pub fn gather_kills_running_workers_after_a_timeout_test() {
  let started = process.new_subject()
  let outcome =
    fetch.gather(
      ["slow"],
      fn(_) {
        process.send(started, process.self())
        process.sleep_forever()
        Ok(dynamic.nil())
      },
      clock.deadline_in(100),
    )
  assert outcome == Error(fetch.FetchTimeout)
  let assert Ok(slow) = process.receive(started, 1000)
  assert !process.is_alive(slow)
}

fn burn(left: Int) -> Int {
  case left {
    0 -> 0
    _ -> burn(left - 1)
  }
}

fn gather_instant(round: Int, crashed: Int) -> Int {
  case round {
    0 -> crashed
    _ -> {
      let slack = burn(round % 4000)
      let outcome =
        fetch.gather(
          ["a", "b", "c"],
          fn(name) { Ok(dynamic.string(name)) },
          clock.deadline_in(5000 + slack),
        )
      case outcome {
        Ok(_) -> gather_instant(round - 1, crashed)
        Error(_) -> gather_instant(round - 1, crashed + 1)
      }
    }
  }
}

pub fn gather_never_reports_a_worker_that_finished_before_its_monitor_test() {
  assert gather_instant(20_000, 0) == 0
}
