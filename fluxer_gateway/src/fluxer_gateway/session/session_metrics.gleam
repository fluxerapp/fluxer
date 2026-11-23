// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/telemetry/metrics.{type Sample, Sample}
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/util/table
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list

pub type Replay {
  ResumeReplay
  ReattachReplay
}

type Scan {
  Scan(memory: Int, queued: Int, queued_max: Int)
}

pub const detached = "fluxer_gateway_sessions_detached"

pub const process_memory = "fluxer_gateway_session_process_memory_bytes"

pub const mailbox_messages = "fluxer_gateway_session_mailbox_messages"

pub const mailbox_max_messages = "fluxer_gateway_session_mailbox_max_messages"

pub const replays = "fluxer_gateway_session_replays_total"

pub const replayed_dispatches =
  "fluxer_gateway_session_replayed_dispatches_total"

pub const move_ms = "fluxer_gateway_session_move_ms"

pub const replay_paths = ["resume", "reattach"]

const collector = "fluxer_gateway_session_processes"

fn detached_table() -> table.Table {
  table.named("fluxer_gateway_detached_sessions")
}

pub fn register() -> Nil {
  let _ = table.ensure_public(detached_table())
  metrics.register_collector(collector, collect)
}

pub fn detach(pid: Pid) -> Nil {
  case table.ensure_public(detached_table()) {
    True -> table.insert(detached_table(), #(pid, Nil))
    False -> Nil
  }
}

pub fn attach(pid: Pid) -> Nil {
  table.delete(detached_table(), pid)
}

pub fn replayed(path: Replay, dispatches: Int) -> Nil {
  let labels = [#("path", path_name(path))]
  metrics.counter(replays, labels, 1)
  case dispatches > 0 {
    True -> metrics.counter(replayed_dispatches, labels, dispatches)
    False -> Nil
  }
}

fn path_name(path: Replay) -> String {
  case path {
    ResumeReplay -> "resume"
    ReattachReplay -> "reattach"
  }
}

pub fn moved(moved: Bool, elapsed_ms: Int) -> Nil {
  let outcome = case moved {
    True -> "moved"
    False -> "failed"
  }
  metrics.observe_ms(move_ms, [#("outcome", outcome)], elapsed_ms)
}

fn collect() -> List(Sample) {
  let scan =
    process_registry.entries(process_registry.sessions)
    |> list.fold(Scan(0, 0, 0), fn(scan, entry) {
      let #(_, pid) = entry
      case process_info.memory_and_queue(pid) {
        Error(Nil) -> scan
        Ok(#(memory, queued)) ->
          Scan(
            memory: scan.memory + memory,
            queued: scan.queued + queued,
            queued_max: int.max(scan.queued_max, queued),
          )
      }
    })
  [
    Sample(detached, [], count_detached()),
    Sample(process_memory, [], scan.memory),
    Sample(mailbox_messages, [], scan.queued),
    Sample(mailbox_max_messages, [], scan.queued_max),
  ]
}

fn count_detached() -> Int {
  let rows: List(#(Pid, Nil)) = table.to_list(detached_table())
  list.fold(rows, 0, fn(count, row) {
    case process.is_alive(row.0) {
      True -> count + 1
      False -> {
        table.delete_object(detached_table(), row)
        count
      }
    }
  })
}
