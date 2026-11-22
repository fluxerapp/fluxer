// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/util/spawn
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/list
import gleam/result
import gleam/string

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in: String) -> List(String)

fn read(path: String) -> String {
  read_file(path)
  |> result.replace_error(Nil)
  |> result.try(bit_array.to_string)
  |> result.unwrap("")
}

fn burn(left: Int) -> Int {
  case left {
    0 -> 0
    _ -> burn(left - 1)
  }
}

fn abnormal_downs(round: Int, abnormal: Int) -> Int {
  case round {
    0 -> abnormal
    _ -> {
      let slack = burn(round % 4000)
      let #(_, monitor) = spawn.monitored(fn() { slack })
      let reason =
        process.new_selector()
        |> process.select_specific_monitor(monitor, fn(down) { down.reason })
        |> process.selector_receive_forever
      case reason {
        process.Normal -> abnormal_downs(round - 1, abnormal)
        _ -> abnormal_downs(round - 1, abnormal + 1)
      }
    }
  }
}

pub fn a_worker_that_exits_at_once_is_seen_exiting_normally_test() {
  assert abnormal_downs(20_000, 0) == 0
}

pub fn the_monitor_reports_a_crash_reason_test() {
  let #(pid, monitor) =
    spawn.monitored(fn() { process.send_abnormal_exit(process.self(), "boom") })
  let down =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down })
    |> process.selector_receive(1000)
  let assert Ok(process.ProcessDown(pid: down_pid, reason:, ..)) = down
  assert down_pid == pid
  assert reason == process.Abnormal(dynamic.string("boom"))
}

fn spawned_binding(line: String) -> Result(String, Nil) {
  case string.trim(line) {
    "let " <> binding ->
      case string.split_once(binding, " =") {
        Ok(#(name, _)) -> Ok(name)
        Error(Nil) -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

fn late_monitors(path: String) -> List(String) {
  let lines = read(path) |> string.split("\n")
  let pairs = list.zip(lines, list.drop(lines, 1) |> list.append([""]))
  list.index_map(pairs, fn(pair, index) { #(pair, index) })
  |> list.flat_map(fn(entry) {
    let #(#(line, next), index) = entry
    let spawned =
      string.contains(line, "process.spawn_unlinked(")
      || string.contains(line, "process.spawn(")
      || string.starts_with(string.trim(next), "process.spawn_unlinked(")
      || string.starts_with(string.trim(next), "process.spawn(")
    case spawned, spawned_binding(line) {
      True, Ok(name) ->
        lines
        |> list.drop(index)
        |> list.take(40)
        |> list.filter(fn(later) {
          string.contains(later, "monitor(" <> name <> ")")
        })
        |> list.map(fn(_) { path <> ": " <> name })
      _, _ -> []
    }
  })
}

pub fn no_process_is_monitored_after_it_was_spawned_test() {
  let offenders =
    find_files(matching: "**/*.gleam", in: "src")
    |> list.flat_map(fn(file) { late_monitors("src/" <> file) })
  assert offenders == []
}
