// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/socket
import fluxer_gateway/util/table
import gleam/erlang/process.{type Subject}
import gleam/list
import gleam/result

fn registry() -> table.Table {
  table.named("fluxer_gateway_ws_sockets")
}

fn dead_pid() -> process.Pid {
  let pid = process.spawn_unlinked(fn() { Nil })
  let monitor = process.monitor(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) { down })
    |> process.selector_receive(1000)
  pid
}

pub fn dead_rows_are_pruned_and_not_counted_test() {
  let _ = table.ensure_public(registry())
  let dead = dead_pid()
  let dead_subject: Subject(socket.Message) = process.new_subject()
  table.insert(registry(), #(dead, dead_subject))
  let live = process.self()
  let live_subject: Subject(socket.Message) = process.new_subject()
  table.insert(registry(), #(live, live_subject))
  let before = table.to_list(registry())
  let count = socket.reconnect_all()
  assert table.lookup(registry(), dead) == []
  assert table.lookup(registry(), live) == [#(live, live_subject)]
  assert count < list.length(before)
  assert result.is_ok(process.receive(live_subject, 1000))
  assert result.is_error(process.receive(dead_subject, 0))
  table.delete(registry(), live)
}

pub fn prune_dead_removes_rows_of_exited_connections_test() {
  let _ = table.ensure_public(registry())
  let dead = dead_pid()
  let subject: Subject(socket.Message) = process.new_subject()
  table.insert(registry(), #(dead, subject))
  socket.prune_dead()
  assert table.lookup(registry(), dead) == []
}
