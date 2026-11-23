// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/session/session_metrics
import fluxer_gateway/telemetry/metrics
import gleam/erlang/process
import gleam/list

fn observed(name: String, labels: List(#(String, String))) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) { series.name == name && series.labels == labels })
  |> list.fold(0, fn(total, series) { total + series.count })
}

fn value(name: String, labels: List(#(String, String))) -> Int {
  metrics.value(metrics.read_all(), name, labels)
}

pub fn moves_are_timed_by_outcome_test() {
  let moved = observed(session_metrics.move_ms, [#("outcome", "moved")])
  let failed = observed(session_metrics.move_ms, [#("outcome", "failed")])
  session_metrics.moved(True, 12)
  session_metrics.moved(False, 3000)
  assert observed(session_metrics.move_ms, [#("outcome", "moved")]) - moved == 1
  assert observed(session_metrics.move_ms, [#("outcome", "failed")]) - failed
    == 1
}

pub fn replays_count_paths_and_dispatches_test() {
  let labels = [#("path", "reattach")]
  let replays = value(session_metrics.replays, labels)
  let dispatches = value(session_metrics.replayed_dispatches, labels)
  session_metrics.replayed(session_metrics.ReattachReplay, 7)
  session_metrics.replayed(session_metrics.ReattachReplay, 0)
  assert value(session_metrics.replays, labels) - replays == 2
  assert value(session_metrics.replayed_dispatches, labels) - dispatches == 7
}

pub fn detached_sessions_drop_out_when_attached_or_dead_test() {
  session_metrics.register()
  let before = value(session_metrics.detached, [])
  let waiting = process.spawn_unlinked(fn() { process.sleep_forever() })
  let gone = process.spawn_unlinked(fn() { process.sleep_forever() })
  session_metrics.detach(waiting)
  session_metrics.detach(gone)
  assert value(session_metrics.detached, []) - before == 2
  process.kill(gone)
  process.sleep(10)
  assert value(session_metrics.detached, []) - before == 1
  session_metrics.attach(waiting)
  assert value(session_metrics.detached, []) == before
  process.kill(waiting)
}
