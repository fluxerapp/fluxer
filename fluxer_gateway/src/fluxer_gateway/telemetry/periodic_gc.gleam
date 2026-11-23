// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/telemetry/process_info.{type Mfa}
import fluxer_gateway/telemetry/process_registry
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import logging

fn init_timeout_ms() -> Int {
  durations.telemetry().periodic_gc_init_timeout_ms
}

fn gc_interval_ms() -> Int {
  durations.telemetry().periodic_gc_gc_interval_ms
}

fn health_interval_ms() -> Int {
  durations.telemetry().periodic_gc_health_interval_ms
}

fn registry_sweep_interval_ms() -> Int {
  durations.telemetry().periodic_gc_registry_sweep_interval_ms
}

const large_guild_bytes = 268_435_456

const stuck_mailbox_threshold = 100

const stuck_max_reductions = 1_000_000

const stuck_sustained_cycles = 3

const max_process_scan = 5000

fn graceful_stop_wait_ms() -> Int {
  durations.telemetry().periodic_gc_graceful_stop_wait_ms
}

pub opaque type Message {
  RunGc
  RunHealthCheck
  RunRegistrySweep
}

type Progress {
  Progress(cycles: Int, reductions: Int)
}

type State {
  State(self: Subject(Message), stuck: Dict(Pid, Progress))
}

type Flag {
  FullsweepAfter
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Flag, value: Int) -> Int

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let _ = process_flag(FullsweepAfter, 50)
    process.send_after(self, gc_interval_ms(), RunGc)
    process.send_after(self, health_interval_ms(), RunHealthCheck)
    process.send_after(self, registry_sweep_interval_ms(), RunRegistrySweep)
    State(self:, stuck: dict.new())
    |> actor.initialised
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    RunGc -> {
      collect_large_idle_guilds()
      process.send_after(state.self, gc_interval_ms(), RunGc)
      actor.continue(state)
    }
    RunHealthCheck -> {
      let stuck = reap_stuck_spawns(state.stuck)
      process.send_after(state.self, health_interval_ms(), RunHealthCheck)
      actor.continue(State(..state, stuck:))
    }
    RunRegistrySweep -> {
      sweep_process_registry()
      process.send_after(
        state.self,
        registry_sweep_interval_ms(),
        RunRegistrySweep,
      )
      actor.continue(state)
    }
  }
}

fn collect_large_idle_guilds() -> Nil {
  process_registry.entries(process_registry.guilds)
  |> list.each(fn(entry) {
    let #(_, pid) = entry
    case process_info.memory(pid), process_info.message_queue_len(pid) {
      Ok(memory), Ok(0) if memory >= large_guild_bytes ->
        process_info.major_gc(pid)
      _, _ -> Nil
    }
  })
}

fn sweep_process_registry() -> Nil {
  case process_registry.sweep_dead() {
    0 -> Nil
    swept ->
      logging.log(
        logging.Info,
        "Swept " <> int.to_string(swept) <> " dead process registry rows",
      )
  }
}

fn reap_stuck_spawns(previous: Dict(Pid, Progress)) -> Dict(Pid, Progress) {
  process_info.scan_pids(max_process_scan)
  |> scan_candidates(previous)
  |> list.fold(dict.new(), fn(next, pid) { check_spawn(pid, previous, next) })
}

pub fn scan_candidates(scanned: List(a), tracked: Dict(a, b)) -> List(a) {
  list.append(dict.keys(tracked), scanned) |> list.unique
}

fn check_spawn(
  pid: Pid,
  previous: Dict(Pid, Progress),
  next: Dict(Pid, Progress),
) -> Dict(Pid, Progress) {
  case
    process_info.current_function(pid),
    process_info.message_queue_len(pid),
    process_info.reductions(pid)
  {
    Ok(current), Ok(queue_len), Ok(reductions)
      if queue_len > stuck_mailbox_threshold && reductions < stuck_max_reductions
    ->
      case
        is_known_stuck_function(current) && process_info.is_spawned_fun(pid)
      {
        True -> track(pid, queue_len, reductions, current, previous, next)
        False -> next
      }
    _, _, _ -> next
  }
}

fn track(
  pid: Pid,
  queue_len: Int,
  reductions: Int,
  current: Mfa,
  previous: Dict(Pid, Progress),
  next: Dict(Pid, Progress),
) -> Dict(Pid, Progress) {
  case dict.get(previous, pid) {
    Ok(Progress(cycles, seen)) if seen == reductions -> {
      let cycles = cycles + 1
      case cycles >= stuck_sustained_cycles {
        True -> {
          logging.log(
            logging.Warning,
            "Reaping sustained stuck spawned process "
              <> describe(pid, queue_len, current, cycles),
          )
          process_info.stop(
            pid,
            graceful_stop_wait_ms(),
            graceful_stop_wait_ms(),
          )
          next
        }
        False -> {
          logging.log(
            logging.Warning,
            "Stuck spawned process with no progress, awaiting sustained confirmation "
              <> describe(pid, queue_len, current, cycles),
          )
          dict.insert(next, pid, Progress(cycles, reductions))
        }
      }
    }
    _ -> dict.insert(next, pid, Progress(1, reductions))
  }
}

fn is_known_stuck_function(current: Mfa) -> Bool {
  case current.module, current.function {
    "gen", "do_call" -> True
    "prim_inet", "recv0" -> True
    "gen_statem", "call_clean" -> True
    _, _ -> False
  }
}

fn describe(pid: Pid, queue_len: Int, current: Mfa, cycles: Int) -> String {
  "pid="
  <> process_info.pid_text(pid)
  <> " message_queue_len="
  <> int.to_string(queue_len)
  <> " current_function="
  <> process_info.mfa_to_string(current)
  <> " cycles="
  <> int.to_string(cycles)
}
