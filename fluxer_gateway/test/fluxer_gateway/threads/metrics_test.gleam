// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics as telemetry
import fluxer_gateway/threads/metrics
import gleam/erlang/process
import gleam/list
import support/wait

pub fn concurrent_holders_are_summed_until_they_exit_test() {
  let before = metrics.held()
  let done = process.new_subject()
  let release = process.new_subject()
  let workers = 64
  let pids =
    list.map(list.repeat(Nil, workers), fn(_) {
      process.spawn_unlinked(fn() {
        metrics.threads_held(3)
        metrics.threads_held(2)
        process.send(done, Nil)
        let _ = process.receive_forever(release)
        Nil
      })
    })
  list.each(pids, fn(_) {
    let assert Ok(Nil) = process.receive(done, 5000)
  })
  assert metrics.held() == before + workers * 2
  assert telemetry.value(
      telemetry.read_all(),
      "fluxer_gateway_thread_store_threads",
      [],
    )
    == before + workers * 2
  list.each(pids, process.kill)
  assert wait.until(2000, fn() { metrics.held() == before })
}

pub fn a_zero_count_drops_the_holder_test() {
  let before = metrics.held()
  let done = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      metrics.threads_held(0)
      metrics.threads_held(5)
      metrics.threads_held(0)
      process.send(done, Nil)
      process.sleep_forever()
    })
  let assert Ok(Nil) = process.receive(done, 5000)
  assert metrics.held() == before
  process.kill(pid)
}
