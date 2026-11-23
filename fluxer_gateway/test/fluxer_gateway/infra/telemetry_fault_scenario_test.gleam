// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/supervisor
import fluxer_gateway/config/settings
import fluxer_gateway/infra/harness
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/vm
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/list
import gleam/result
import support/isolation
import support/wait

type MonitorOption {
  LongGc(Int)
}

@external(erlang, "erlang", "register")
fn register(name: Atom, pid: Pid) -> Bool

@external(erlang, "erlang", "system_monitor")
fn system_monitor() -> Dynamic

@external(erlang, "erlang", "system_monitor")
fn claim_system_monitor(pid: Pid, options: List(MonitorOption)) -> Dynamic

@external(erlang, "erlang", "suspend_process")
fn suspend_process(pid: Pid) -> Bool

@external(erlang, "erlang", "resume_process")
fn resume_process(pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn queue_info(pid: Pid, item: Atom) -> Dynamic

@external(erlang, "erlang", "send")
fn raw_send(pid: Pid, message: a) -> a

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

fn sink(name: String) -> Pid {
  let pid = process.spawn_unlinked(fn() { process.sleep_forever() })
  register(atom.create(name), pid)
  pid
}

fn queue_len(pid: Pid) -> Int {
  decode.run(
    queue_info(pid, atom.create("message_queue_len")),
    decode.at([1], decode.int),
  )
  |> result.unwrap(0)
}

fn flood(target: Pid, count: Int) -> Nil {
  case count {
    0 -> Nil
    _ -> {
      raw_send(target, Nil)
      flood(target, count - 1)
    }
  }
}

fn keep_flooding(targets: List(Pid)) -> Pid {
  process.spawn_unlinked(fn() { flood_loop(targets) })
}

fn flood_loop(targets: List(Pid)) -> Nil {
  list.each(targets, fn(target) {
    case process.is_alive(target) {
      True -> flood(target, 4000)
      False -> Nil
    }
  })
  process.sleep(10)
  flood_loop(targets)
}

fn fast_watchdog() -> Nil {
  isolation.set_durations(fn(d) {
    settings.Durations(
      ..d,
      telemetry: settings.TelemetryDurations(
        ..d.telemetry,
        watchdog_check_interval_ms: 100,
      ),
    )
  })
  let _ = harness.cycle(supervisor.Watchdog)
  Nil
}

pub fn watchdog_kills_a_killable_mailbox_only_after_sustained_growth_test() {
  fast_watchdog()
  let killable = sink("fluxer_gateway_dispatch_relay_9901")
  let protected = sink("fluxer_gateway_api_http_9901")
  let flat = sink("fluxer_gateway_dispatch_relay_9902")
  flood(flat, 60_000)
  let kills = watchdog_actions("kill")
  let collects = watchdog_actions("collect")
  let flooder = keep_flooding([killable, protected])
  assert wait.until(10_000, fn() { !process.is_alive(killable) })
  assert wait.until(2000, fn() { longest_mailbox("singleton") > 50_000 })
  process.kill(flooder)
  assert watchdog_actions("kill") > kills
  assert watchdog_actions("collect") > collects
  assert process.is_alive(protected)
  assert queue_len(protected) > 50_000
  process.sleep(500)
  assert process.is_alive(flat)
  process.kill(protected)
  process.kill(flat)
}

pub fn watchdog_survives_watched_processes_dying_mid_check_test() {
  fast_watchdog()
  let assert Ok(watchdog) = harness.pid_of(supervisor.Watchdog)
  int.range(from: 1, to: 41, with: Nil, run: fn(_, index) {
    let pid = sink("fluxer_gateway_dispatch_relay_97" <> int.to_string(index))
    flood(pid, 1500)
    process.kill(pid)
  })
  process.sleep(400)
  assert harness.pid_of(supervisor.Watchdog) == Ok(watchdog)
}

fn monitor_owner() -> Result(Pid, Nil) {
  decode.run(system_monitor(), decode.at([0], decode.dynamic))
  |> result.replace_error(Nil)
  |> result.try(fn(owner) {
    case dynamic.classify(owner) {
      "Pid" -> Ok(coerce(owner))
      _ -> Error(Nil)
    }
  })
}

fn watchdog_actions(action: String) -> Int {
  metrics.read_all()
  |> metrics.value("fluxer_gateway_watchdog_actions_total", [
    #("action", action),
  ])
}

fn longest_mailbox(kind: String) -> Int {
  metrics.read_all()
  |> metrics.value("fluxer_gateway_watchdog_mailbox_max_messages", [
    #("kind", kind),
  ])
}

fn stall_durations(kind: String) -> Int {
  metrics.histograms()
  |> list.filter(fn(series) {
    series.name == "fluxer_gateway_scheduler_stall_duration_ms"
    && series.labels == [#("kind", kind)]
  })
  |> list.fold(0, fn(total, series) { total + series.count })
}

fn stalls(kind: String) -> Int {
  metrics.read_all()
  |> metrics.value("fluxer_gateway_scheduler_stalls_total", [#("kind", kind)])
}

fn severe(kind: String) -> Int {
  metrics.read_all()
  |> metrics.value("fluxer_gateway_scheduler_severe_stalls_total", [
    #("kind", kind),
  ])
}

pub fn stall_monitor_turns_on_scheduler_wall_time_test() {
  let _ = harness.cycle(supervisor.StallMonitor)
  let wall = vm.scheduler_wall_time()
  assert list.map(wall, fn(entry) { entry.class }) == vm.scheduler_classes
  assert list.any(wall, fn(entry) {
    entry.class == vm.Normal && entry.total > 0 && entry.active > 0
  })
}

pub fn stall_monitor_successor_owns_the_system_monitor_test() {
  let first = harness.cycle(supervisor.StallMonitor)
  assert monitor_owner() == Ok(first)
  let supervisor_pid = harness.app_supervisor()
  process.kill(first)
  assert wait.until(2000, fn() {
    case harness.pid_of(supervisor.StallMonitor) {
      Ok(next) -> next != first && monitor_owner() == Ok(next)
      Error(Nil) -> False
    }
  })
  assert process.is_alive(supervisor_pid)
}

pub fn stall_monitor_claims_the_system_monitor_once_a_foreign_owner_leaves_test() {
  let ready = process.new_subject()
  let foreign =
    process.spawn_unlinked(fn() {
      claim_system_monitor(process.self(), [LongGc(60_000)])
      process.send(ready, Nil)
      process.sleep_forever()
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  let stall = harness.cycle(supervisor.StallMonitor)
  assert monitor_owner() == Ok(foreign)
  process.kill(foreign)
  assert wait.until(3000, fn() { monitor_owner() == Ok(stall) })
}

pub fn stall_monitor_counts_injected_events_and_late_ticks_test() {
  let stall = harness.cycle(supervisor.StallMonitor)
  let schedule = stalls("long_schedule")
  let schedule_severe = severe("long_schedule")
  let busy = stalls("busy_dist_port")
  let late = stalls("timer_late")
  let timed = stall_durations("long_schedule")
  let busy_timed = stall_durations("busy_dist_port")
  let late_timed = stall_durations("timer_late")
  let event = fn(kind: String, info: Dynamic) {
    let raw: #(Atom, Pid, Atom, Dynamic) = #(
      atom.create("monitor"),
      process.self(),
      atom.create(kind),
      info,
    )
    raw_send(stall, raw)
    Nil
  }
  event(
    "long_schedule",
    coerce([
      #(atom.create("timeout"), coerce(1500)),
      #(atom.create("in"), coerce(atom.create("x"))),
    ]),
  )
  event("busy_dist_port", coerce(process.self()))
  event("not_a_kind", coerce([]))
  assert wait.until(2000, fn() {
    stalls("long_schedule") == schedule + 1
    && stalls("busy_dist_port") == busy + 1
  })
  assert severe("long_schedule") == schedule_severe + 1
  assert stall_durations("long_schedule") == timed + 1
  assert stall_durations("busy_dist_port") == busy_timed
  suspend_process(stall)
  process.sleep(500)
  resume_process(stall)
  assert wait.until(2000, fn() { stalls("timer_late") > late })
  assert stall_durations("timer_late") > late_timed
  assert process.is_alive(stall)
}
