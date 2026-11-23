// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/telemetry/metrics
import fluxer_gateway/telemetry/process_info
import fluxer_gateway/telemetry/process_registry
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/set
import gleam/string
import logging

fn init_timeout_ms() -> Int {
  durations.telemetry().watchdog_init_timeout_ms
}

fn check_interval_ms() -> Int {
  durations.telemetry().watchdog_check_interval_ms
}

const warning_threshold = 1000

const critical_threshold = 10_000

const kill_threshold = 50_000

const sustained_samples = 3

pub opaque type Message {
  RunCheck
}

type State {
  State(self: Subject(Message), history: Dict(Pid, List(Int)))
}

type Flag {
  FullsweepAfter
}

pub type Action {
  Quiet
  Warn
  Collect
  CollectPendingGrowth
  Kill
}

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Flag, value: Int) -> Int

pub fn kill_threshold_len() -> Int {
  kill_threshold
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.worker(start)
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let _ = process_flag(FullsweepAfter, 50)
    process.send_after(self, check_interval_ms(), RunCheck)
    State(self:, history: dict.new())
    |> actor.initialised
    |> Ok
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    RunCheck -> {
      let watched = watched_pids()
      let #(history, longest) =
        list.fold(watched, #(state.history, dict.new()), fn(acc, entry) {
          let #(history, longest) = acc
          let #(pid, label) = entry
          let #(history, queue_len) = check(pid, label, history)
          #(
            history,
            dict.upsert(longest, holder_kind(label), fn(found) {
              int.max(option.unwrap(found, 0), queue_len)
            }),
          )
        })
      record_longest(longest)
      let live = watched |> list.map(fn(entry) { entry.0 }) |> set.from_list
      let history = dict.filter(history, fn(pid, _) { set.contains(live, pid) })
      process.send_after(state.self, check_interval_ms(), RunCheck)
      actor.continue(State(..state, history:))
    }
  }
}

pub fn watched_pids() -> List(#(Pid, String)) {
  let guilds =
    process_registry.entries(process_registry.guilds)
    |> list.map(fn(entry) {
      let #(guild, pid) = entry
      #(pid, "guild:" <> int.to_string(guild))
    })
  let named =
    process_info.gateway_processes()
    |> list.map(fn(entry) {
      let #(label, pid) = entry
      #(pid, label)
    })
  list.append(guilds, named)
}

pub const holder_kinds = ["guild", "singleton"]

pub const action_names = ["warn", "collect", "kill", "stuck"]

pub fn holder_kind(label: String) -> String {
  case string.starts_with(label, "guild:") {
    True -> "guild"
    False -> "singleton"
  }
}

fn record_longest(longest: Dict(String, Int)) -> Nil {
  list.each(holder_kinds, fn(kind) {
    metrics.gauge(
      "fluxer_gateway_watchdog_mailbox_max_messages",
      [#("kind", kind)],
      dict.get(longest, kind) |> result.unwrap(0),
    )
  })
}

fn count_action(name: String) -> Nil {
  metrics.counter(
    "fluxer_gateway_watchdog_actions_total",
    [#("action", name)],
    1,
  )
}

fn check(
  pid: Pid,
  label: String,
  history: Dict(Pid, List(Int)),
) -> #(Dict(Pid, List(Int)), Int) {
  case process_info.message_queue_len(pid) {
    Error(Nil) -> #(dict.delete(history, pid), 0)
    Ok(queue_len) -> {
      let samples =
        record_sample(queue_len, result.unwrap(dict.get(history, pid), []))
      act(pid, label, queue_len, samples)
      #(dict.insert(history, pid, samples), queue_len)
    }
  }
}

pub fn record_sample(queue_len: Int, previous: List(Int)) -> List(Int) {
  list.take([queue_len, ..previous], sustained_samples)
}

pub fn action(queue_len: Int, samples: List(Int)) -> Action {
  case queue_len {
    _ if queue_len > kill_threshold ->
      case is_sustained_over_kill(samples) {
        True -> Kill
        False -> CollectPendingGrowth
      }
    _ if queue_len > critical_threshold -> Collect
    _ if queue_len > warning_threshold -> Warn
    _ -> Quiet
  }
}

pub fn is_growing(samples: List(Int)) -> Bool {
  case list.length(samples) >= sustained_samples, list.last(samples) {
    True, Ok(oldest) ->
      oldest > warning_threshold && strictly_decreasing(samples)
    _, _ -> False
  }
}

fn is_sustained_over_kill(samples: List(Int)) -> Bool {
  list.length(samples) >= sustained_samples
  && list.all(samples, fn(sample) { sample > kill_threshold })
  && strictly_decreasing(samples)
}

fn strictly_decreasing(samples: List(Int)) -> Bool {
  case samples {
    [] -> False
    [_] -> True
    [newer, older, ..rest] ->
      newer > older && strictly_decreasing([older, ..rest])
  }
}

fn act(pid: Pid, label: String, queue_len: Int, samples: List(Int)) -> Nil {
  let details =
    "label="
    <> label
    <> " pid="
    <> process_info.pid_text(pid)
    <> " message_queue_len="
    <> int.to_string(queue_len)
  case action(queue_len, samples), killable(label) {
    Kill, False -> {
      count_action("collect")
      logging.log(
        logging.Critical,
        "Mailbox over sustained kill threshold on a process never killed, forcing GC "
          <> details
          <> " kill_threshold="
          <> int.to_string(kill_threshold)
          <> " samples="
          <> samples_text(samples),
      )
      process_info.major_gc(pid)
    }
    Kill, True -> {
      count_action("kill")
      logging.log(
        logging.Critical,
        "Killing process over sustained kill threshold "
          <> details
          <> " kill_threshold="
          <> int.to_string(kill_threshold)
          <> " samples="
          <> samples_text(samples),
      )
      process.kill(pid)
    }
    CollectPendingGrowth, _ -> {
      count_action("collect")
      logging.log(
        logging.Critical,
        "Mailbox over kill threshold, forcing GC pending sustained growth "
          <> details
          <> " kill_threshold="
          <> int.to_string(kill_threshold)
          <> " samples="
          <> samples_text(samples),
      )
      process_info.major_gc(pid)
    }
    Collect, _ -> {
      count_action("collect")
      logging.log(
        logging.Critical,
        "Critical mailbox size, forcing GC " <> details,
      )
      process_info.major_gc(pid)
    }
    Warn, _ -> {
      count_action("warn")
      logging.log(logging.Warning, "High mailbox size " <> details)
    }
    Quiet, _ -> Nil
  }
  case is_growing(samples) {
    True -> {
      count_action("stuck")
      logging.log(
        logging.Warning,
        "Stuck process detected, monotonically growing mailbox "
          <> details
          <> " samples="
          <> samples_text(samples),
      )
    }
    False -> Nil
  }
}

fn killable(label: String) -> Bool {
  list.contains(killable_names, label)
  || list.any(killable_prefixes, fn(prefix) {
    string.starts_with(label, prefix)
  })
}

const killable_names = [
  "fluxer_gateway_session_manager", "fluxer_voice_call_manager",
  "fluxer_gateway_push", "fluxer_gateway_nats_rpc", "fluxer_gateway_nats_pool",
  "fluxer_gateway_dispatch_relay", "fluxer_gateway@config@rollout",
]

const killable_prefixes = [
  "guild:", "fluxer_gateway_dispatch_relay_", "fluxer_gateway_session_shard_",
  "fluxer_gateway_guild_shard_", "fluxer_presence_manager_",
]

fn samples_text(samples: List(Int)) -> String {
  "[" <> string.join(list.map(samples, int.to_string), ",") <> "]"
}
