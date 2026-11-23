// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/telemetry/process_registry
import fluxer_gateway/telemetry/vm
import fluxer_gateway/util/liveness
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.telemetry().stall_monitor_init_timeout_ms
}

fn tick_ms() -> Int {
  durations.telemetry().stall_monitor_tick_ms
}

fn late_tick_ms() -> Int {
  durations.telemetry().stall_monitor_late_tick_ms
}

fn severe_ms() -> Int {
  durations.telemetry().stall_monitor_severe_ms
}

fn log_interval_ms() -> Int {
  durations.telemetry().stall_monitor_log_interval_ms
}

fn long_schedule_ms() -> Int {
  durations.telemetry().stall_monitor_long_schedule_ms
}

fn long_gc_ms() -> Int {
  durations.telemetry().stall_monitor_long_gc_ms
}

pub opaque type Message {
  Tick
  Stall(kind: String, ms: Int, who: Who, info: String)
  Ignored
}

type Who {
  Process(Pid)
  Printed(String)
  Nobody
}

type Detail {
  TimerLate
  MonitorEvent(who: Who, info: String)
}

type Claim {
  Owner
  Elsewhere
}

type State {
  State(
    self: Subject(Message),
    claim: Claim,
    tick_due: Int,
    last_log_ms: Int,
    suppressed: Int,
  )
}

type Flag {
  Priority
}

type PriorityLevel {
  High
}

type MonitorOption {
  LongSchedule(Int)
  LongGc(Int)
  BusyDistPort
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Flag, value: PriorityLevel) -> PriorityLevel

type MonitorSetting

@external(erlang, "erlang", "system_monitor")
fn current_system_monitor() -> MonitorSetting

@external(erlang, "erlang", "system_monitor")
fn set_system_monitor(pid: Pid, options: List(MonitorOption)) -> MonitorSetting

@external(erlang, "erlang", "is_tuple")
fn is_installed(setting: MonitorSetting) -> Bool

@external(erlang, "erlang", "element")
fn monitor_owner(index: Int, setting: MonitorSetting) -> Pid

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), initialise)
  |> actor.on_message(handle)
  |> actor.start
}

fn initialise(
  self: Subject(Message),
) -> Result(actor.Initialised(State, Message, Nil), String) {
  let _ = process_flag(Priority, High)
  vm.track_scheduler_wall_time()
  let claim = install()
  case claim {
    Owner -> Nil
    Elsewhere ->
      logging.log(
        logging.Warning,
        "Scheduler stall monitor not installed, another process owns the system monitor",
      )
  }
  let now = clock.monotonic_ms()
  let selector =
    process.new_selector()
    |> process.select(self)
    |> process.select_record(
      tag: atom.create("monitor"),
      fields: 3,
      mapping: fn(message) {
        decode.run(message, monitor_message()) |> result.unwrap(Ignored)
      },
    )
  State(
    self:,
    claim:,
    tick_due: schedule_tick(self, now),
    last_log_ms: now - log_interval_ms(),
    suppressed: 0,
  )
  |> actor.initialised
  |> actor.selecting(selector)
  |> Ok
}

fn install() -> Claim {
  let me = process.self()
  let setting = current_system_monitor()
  case is_installed(setting) {
    False -> claim(me)
    True -> {
      let owner = monitor_owner(1, setting)
      case owner == me, process.is_alive(owner) {
        True, _ -> Owner
        False, True -> Elsewhere
        False, False -> claim(me)
      }
    }
  }
}

fn claim(me: Pid) -> Claim {
  let _ =
    set_system_monitor(me, [
      LongSchedule(long_schedule_ms()),
      LongGc(long_gc_ms()),
      BusyDistPort,
    ])
  Owner
}

fn reclaim(state: State) -> State {
  case state.claim {
    Owner -> state
    Elsewhere ->
      case install() {
        Owner -> {
          logging.log(
            logging.Notice,
            "Scheduler stall monitor installed after the previous owner released the system monitor",
          )
          State(..state, claim: Owner)
        }
        Elsewhere -> state
      }
  }
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Tick -> {
      let now = clock.monotonic_ms()
      let late = now - state.tick_due
      let state =
        State(..state, tick_due: schedule_tick(state.self, now)) |> reclaim
      case late >= late_tick_ms() {
        True -> actor.continue(record(state, "timer_late", late, TimerLate))
        False -> actor.continue(state)
      }
    }
    Stall(kind:, ms:, who:, info:) ->
      actor.continue(record(state, kind, ms, MonitorEvent(who:, info:)))
    Ignored -> actor.continue(state)
  }
}

fn schedule_tick(self: Subject(Message), now: Int) -> Int {
  process.send_after(self, tick_ms(), Tick)
  now + tick_ms()
}

fn record(state: State, kind: String, ms: Int, detail: Detail) -> State {
  metrics.counter("fluxer_gateway_scheduler_stalls_total", [#("kind", kind)], 1)
  case ms > 0 {
    True ->
      metrics.observe_ms(
        "fluxer_gateway_scheduler_stall_duration_ms",
        [#("kind", kind)],
        ms,
      )
    False -> Nil
  }
  case ms >= severe_ms() {
    True ->
      metrics.counter(
        "fluxer_gateway_scheduler_severe_stalls_total",
        [#("kind", kind)],
        1,
      )
    False -> Nil
  }
  let now = clock.monotonic_ms()
  case now - state.last_log_ms >= log_interval_ms() {
    True -> {
      let #(who, info) = describe(ms, detail)
      logging.log(
        logging.Warning,
        "Scheduler stall detected kind="
          <> kind
          <> " ms="
          <> int.to_string(ms)
          <> " who="
          <> who
          <> " info="
          <> info
          <> " suppressed="
          <> int.to_string(state.suppressed),
      )
      State(..state, last_log_ms: now, suppressed: 0)
    }
    False -> State(..state, suppressed: state.suppressed + 1)
  }
}

fn monitor_message() -> Decoder(Message) {
  use kind <- decode.field(2, atom.decoder())
  let name = atom.to_string(kind)
  case name {
    "long_schedule" | "long_gc" | "busy_dist_port" -> {
      use ms <- decode.then(event_ms())
      use who <- decode.then(who())
      use info <- decode.then(compact_info(name))
      decode.success(Stall(kind: name, ms:, who:, info:))
    }
    _ -> decode.success(Ignored)
  }
}

fn describe(ms: Int, detail: Detail) -> #(String, String) {
  case detail {
    TimerLate -> #("undefined", "late_ms=" <> int.to_string(ms))
    MonitorEvent(who:, info:) -> #(describe_who(who), info)
  }
}

fn event_ms() -> Decoder(Int) {
  let number =
    decode.one_of(decode.map(decode.int, Some), [decode.success(None)])
  decode.at([3], info_pairs(number))
  |> decode.map(fn(pairs) {
    case list.key_find(pairs, "timeout") {
      Ok(Some(ms)) if ms >= 0 -> ms
      _ -> 0
    }
  })
  |> decode.one_of([decode.success(0)])
}

fn compact_info(kind: String) -> Decoder(String) {
  let keep = case kind {
    "long_schedule" -> ["in", "out"]
    "long_gc" -> ["heap_size", "old_heap_size", "mbuf_size"]
    _ -> []
  }
  let printed = decode.map(decode.dynamic, string.inspect)
  decode.one_of(
    decode.at([3], info_pairs(printed))
      |> decode.map(fn(pairs) {
        pairs
        |> list.filter(fn(pair) { list.contains(keep, pair.0) })
        |> list.map(fn(pair) { pair.0 <> "=" <> pair.1 })
        |> string.join(",")
      }),
    [
      decode.at([3], printed) |> decode.map(fn(peer) { "peer=" <> peer }),
      decode.success(""),
    ],
  )
}

fn info_pairs(value: Decoder(a)) -> Decoder(List(#(String, a))) {
  decode.list({
    use key <- decode.field(0, atom.decoder())
    use value <- decode.field(1, value)
    decode.success(#(atom.to_string(key), value))
  })
}

fn who() -> Decoder(Who) {
  decode.one_of(
    decode.at([1], process_info.pid_decoder()) |> decode.map(Process),
    [
      decode.at([1], decode.map(decode.dynamic, string.inspect))
        |> decode.map(Printed),
      decode.success(Nobody),
    ],
  )
}

fn describe_who(who: Who) -> String {
  case who {
    Process(pid) -> describe_pid(pid)
    Printed(port) -> port
    Nobody -> "undefined"
  }
}

fn describe_pid(pid: Pid) -> String {
  let base = process_info.pid_text(pid)
  case liveness.is_local(pid) {
    False -> base <> " remote"
    True -> {
      let name = case process_info.registered_name(pid) {
        Ok(name) -> " registered_name=" <> name
        Error(Nil) -> ""
      }
      let initial = case process_info.proc_lib_initial_call(pid) {
        Ok(mfa) -> " initial_call=" <> process_info.mfa_to_string(mfa)
        Error(Nil) -> ""
      }
      let guild = case process_registry.id_of(process_registry.guilds, pid) {
        Ok(guild) -> " guild_id=" <> int.to_string(guild)
        Error(Nil) -> ""
      }
      base <> name <> initial <> guild
    }
  }
}
