// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/int
import gleam/list
import gleam/result

type SystemItem {
  ProcessCount
  ProcessLimit
  PortCount
  PortLimit
  AtomCount
  AtomLimit
  EtsCount
  EtsLimit
  Schedulers
  SchedulersOnline
  DirtyCpuSchedulers
  DirtyCpuSchedulersOnline
  DirtyIoSchedulers
}

type Statistic {
  WallClock
  Reductions
  ContextSwitches
}

type ListStatistic {
  RunQueueLengthsAll
}

type DynamicStatistic {
  SchedulerWallTimeAll
}

type GcStatistic {
  GarbageCollection
}

type IoStatistic {
  Io
}

type Flag {
  SchedulerWallTime
}

pub type SchedulerClass {
  Normal
  DirtyCpu
  DirtyIo
}

pub type RunQueues {
  RunQueues(normal: Int, dirty_cpu: Int, dirty_io: Int)
}

pub type WallTime {
  WallTime(class: SchedulerClass, active: Int, total: Int)
}

pub type Activity {
  Activity(
    reductions: Int,
    context_switches: Int,
    gc_runs: Int,
    gc_words_reclaimed: Int,
    io_in_bytes: Int,
    io_out_bytes: Int,
  )
}

pub const memory_types = [
  "total", "processes", "binary", "ets", "atom", "system", "code",
  "processes_used", "atom_used",
]

pub const scheduler_classes = [Normal, DirtyCpu, DirtyIo]

@external(erlang, "erlang", "memory")
fn erlang_memory() -> List(#(Atom, Int))

@external(erlang, "erlang", "system_info")
fn system_info(item: SystemItem) -> Int

@external(erlang, "erlang", "statistics")
fn statistics(item: Statistic) -> #(Int, Int)

@external(erlang, "erlang", "statistics")
fn list_statistics(item: ListStatistic) -> List(Int)

@external(erlang, "erlang", "statistics")
fn dynamic_statistics(item: DynamicStatistic) -> Dynamic

@external(erlang, "erlang", "statistics")
fn gc_statistics(item: GcStatistic) -> #(Int, Int, Int)

@external(erlang, "erlang", "statistics")
fn io_statistics(item: IoStatistic) -> #(#(Atom, Int), #(Atom, Int))

@external(erlang, "erlang", "system_flag")
fn system_flag(flag: Flag, value: Bool) -> Bool

@external(erlang, "persistent_term", "info")
fn persistent_term_info() -> Dict(Atom, Int)

pub fn memory(of kind: String) -> Int {
  let wanted = atom.create(kind)
  case list.find(erlang_memory(), fn(entry) { entry.0 == wanted }) {
    Ok(#(_, bytes)) -> bytes
    Error(Nil) -> 0
  }
}

pub fn memory_by_type() -> List(#(String, Int)) {
  let found =
    erlang_memory()
    |> list.map(fn(entry) { #(atom.to_string(entry.0), entry.1) })
  list.map(memory_types, fn(kind) {
    #(kind, list.key_find(found, kind) |> result.unwrap(0))
  })
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

pub fn port_limit() -> Int {
  system_info(PortLimit)
}

pub fn atom_count() -> Int {
  system_info(AtomCount)
}

pub fn atom_limit() -> Int {
  system_info(AtomLimit)
}

pub fn ets_count() -> Int {
  system_info(EtsCount)
}

pub fn ets_limit() -> Int {
  system_info(EtsLimit)
}

pub fn schedulers_online() -> Int {
  system_info(SchedulersOnline)
}

pub fn dirty_cpu_schedulers_online() -> Int {
  system_info(DirtyCpuSchedulersOnline)
}

pub fn dirty_io_schedulers() -> Int {
  system_info(DirtyIoSchedulers)
}

pub fn uptime_seconds() -> Int {
  let #(wall_clock_ms, _) = statistics(WallClock)
  wall_clock_ms / 1000
}

pub fn class_name(class: SchedulerClass) -> String {
  case class {
    Normal -> "normal"
    DirtyCpu -> "dirty_cpu"
    DirtyIo -> "dirty_io"
  }
}

pub fn run_queues() -> RunQueues {
  let #(normal, dirty) =
    list.split(list_statistics(RunQueueLengthsAll), system_info(Schedulers))
  case dirty {
    [dirty_cpu, dirty_io, ..] -> RunQueues(int.sum(normal), dirty_cpu, dirty_io)
    [dirty_cpu] -> RunQueues(int.sum(normal), dirty_cpu, 0)
    [] -> RunQueues(int.sum(normal), 0, 0)
  }
}

pub fn track_scheduler_wall_time() -> Nil {
  let _ = system_flag(SchedulerWallTime, True)
  Nil
}

pub fn scheduler_wall_time() -> List(WallTime) {
  let normal = system_info(Schedulers)
  let dirty_cpu = normal + system_info(DirtyCpuSchedulers)
  let entry = {
    use id <- decode.field(0, decode.int)
    use active <- decode.field(1, decode.int)
    use total <- decode.field(2, decode.int)
    decode.success(#(id, active, total))
  }
  let entries =
    decode.run(dynamic_statistics(SchedulerWallTimeAll), decode.list(entry))
    |> result.unwrap([])
  list.map(scheduler_classes, fn(class) {
    let #(active, total) =
      entries
      |> list.filter(fn(found) { class_of(found.0, normal, dirty_cpu) == class })
      |> list.fold(#(0, 0), fn(sum, found) {
        #(sum.0 + found.1, sum.1 + found.2)
      })
    WallTime(class:, active:, total:)
  })
}

fn class_of(id: Int, normal: Int, dirty_cpu: Int) -> SchedulerClass {
  case id {
    _ if id <= normal -> Normal
    _ if id <= dirty_cpu -> DirtyCpu
    _ -> DirtyIo
  }
}

pub fn activity() -> Activity {
  let #(reductions, _) = statistics(Reductions)
  let #(context_switches, _) = statistics(ContextSwitches)
  let #(gc_runs, gc_words_reclaimed, _) = gc_statistics(GarbageCollection)
  let #(#(_, io_in_bytes), #(_, io_out_bytes)) = io_statistics(Io)
  Activity(
    reductions:,
    context_switches:,
    gc_runs:,
    gc_words_reclaimed:,
    io_in_bytes:,
    io_out_bytes:,
  )
}

pub fn persistent_terms() -> #(Int, Int) {
  let info = persistent_term_info()
  #(
    dict.get(info, atom.create("count")) |> result.unwrap(0),
    dict.get(info, atom.create("memory")) |> result.unwrap(0),
  )
}
