// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/erlang/atom.{type Atom}
import gleam/list

type SystemItem {
  ProcessCount
  ProcessLimit
  PortCount
  AtomCount
  SchedulersOnline
}

type Statistic {
  WallClock
}

@external(erlang, "erlang", "memory")
fn erlang_memory() -> List(#(Atom, Int))

@external(erlang, "erlang", "system_info")
fn system_info(item: SystemItem) -> Int

@external(erlang, "erlang", "statistics")
fn statistics(item: Statistic) -> #(Int, Int)

pub fn memory(of kind: String) -> Int {
  let wanted = atom.create(kind)
  case list.find(erlang_memory(), fn(entry) { entry.0 == wanted }) {
    Ok(#(_, bytes)) -> bytes
    Error(Nil) -> 0
  }
}

pub fn process_count() -> Int {
  system_info(ProcessCount)
}

pub fn process_limit() -> Int {
  system_info(ProcessLimit)
}

pub fn port_count() -> Int {
  system_info(PortCount)
}

pub fn atom_count() -> Int {
  system_info(AtomCount)
}

pub fn schedulers_online() -> Int {
  system_info(SchedulersOnline)
}

pub fn uptime_seconds() -> Int {
  let #(wall_clock_ms, _) = statistics(WallClock)
  wall_clock_ms / 1000
}
