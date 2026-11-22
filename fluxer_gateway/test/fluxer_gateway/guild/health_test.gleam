// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/guild/health
import gleam/erlang/process.{type Monitor, type Pid}
import gleam/int
import gleam/list
import gleam/option.{None, Some}

fn idle() -> Pid {
  process.spawn_unlinked(fn() { process.sleep_forever() })
}

fn recording(seen: process.Subject(Monitor)) -> fn(Pid) -> Monitor {
  fn(pid) {
    let monitor = process.monitor(pid)
    process.send(seen, monitor)
    monitor
  }
}

pub fn down_resolves_only_the_guild_of_that_monitor_test() {
  let a = idle()
  let b = idle()
  let seen = process.new_subject()
  let #(watched, _) = health.watch(health.new_watched(), 1, a, recording(seen))
  let #(watched, _) = health.watch(watched, 2, b, recording(seen))
  let #(watched, _) = health.watch(watched, 3, a, recording(seen))
  let assert Ok(_) = process.receive(seen, 0)
  let assert Ok(of_b) = process.receive(seen, 0)
  let #(watched, gone) = health.down(watched, of_b)
  assert gone == Some(2)
  assert list.sort(health.watched_guilds(watched), int.compare) == [1, 3]
  let #(_, again) = health.down(watched, of_b)
  assert again == None
  process.kill(a)
  process.kill(b)
}

pub fn register_replacing_pid_hands_back_old_monitor_test() {
  let a = idle()
  let b = idle()
  let #(watched, first) =
    health.watch(health.new_watched(), 7, a, process.monitor)
  assert first == None
  let #(watched, same) = health.watch(watched, 7, a, process.monitor)
  assert same == None
  let #(watched, replaced) = health.watch(watched, 7, b, process.monitor)
  let assert Some(old) = replaced
  process.demonitor_process(old)
  let #(watched, stale) = health.down(watched, old)
  assert stale == None
  assert health.watched_guilds(watched) == [7]
  process.kill(a)
  process.kill(b)
}
