// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/periodic_gc
import gleam/dict
import gleam/erlang/process
import gleam/list

pub fn scan_candidates_keeps_tracked_pids_outside_the_window_test() {
  let tracked = process.spawn_unlinked(fn() { process.sleep_forever() })
  let scanned = process.spawn_unlinked(fn() { process.sleep_forever() })
  let candidates =
    periodic_gc.scan_candidates([scanned], dict.from_list([#(tracked, 1)]))
  assert list.contains(candidates, tracked)
  assert list.contains(candidates, scanned)
  assert list.length(candidates) == 2
  process.kill(tracked)
  process.kill(scanned)
}

pub fn scan_candidates_checks_a_tracked_pid_once_when_also_scanned_test() {
  let pid = process.spawn_unlinked(fn() { process.sleep_forever() })
  let candidates =
    periodic_gc.scan_candidates([pid], dict.from_list([#(pid, 1)]))
  assert candidates == [pid]
  process.kill(pid)
}
