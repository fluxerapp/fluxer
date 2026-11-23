// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/rollout
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option, None, Some}

type ConfigKey {
  FluxerGatewayRolloutConfig
}

type GuardEvent {
  Watch(Pid)
  Finished(reply: Subject(Nil))
  RunnerGone
}

@external(erlang, "persistent_term", "put")
fn put_config(key: ConfigKey, value: Option(rollout.RolloutConfig)) -> Dynamic

@external(erlang, "persistent_term", "get")
fn get_config(
  key: ConfigKey,
  default: Option(rollout.RolloutConfig),
) -> Option(rollout.RolloutConfig)

pub fn with_platform_e2ee(body: fn() -> Nil) -> Nil {
  let previous = get_config(FluxerGatewayRolloutConfig, None)
  let assert Ok(config) =
    rollout.validate(
      dict.from_list([#("voice_e2ee_scope", dynamic.string("platform_wide"))]),
    )
  let guard = start_guard(previous)
  let _ = put_config(FluxerGatewayRolloutConfig, Some(config))
  assert rollout.voice_e2ee_scope() == rollout.PlatformWide
  let worker = process.spawn_unlinked(body)
  process.send(guard, Watch(worker))
  let monitor = process.monitor(worker)
  let reason =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(down) {
      case down {
        process.ProcessDown(reason:, ..) -> reason
        process.PortDown(reason:, ..) -> reason
      }
    })
    |> process.selector_receive_forever
  let ack = process.new_subject()
  process.send(guard, Finished(ack))
  let assert Ok(Nil) = process.receive(ack, 5000)
  let assert process.Normal = reason
  Nil
}

pub opaque type Paused {
  Paused(guard: Subject(GuardEvent))
}

pub fn pause_sessions() -> Paused {
  let previous = get_config(FluxerGatewayRolloutConfig, None)
  let assert Ok(config) =
    rollout.validate(
      dict.from_list([#("session_rollout_percentage", dynamic.int(0))]),
    )
  let guard = start_guard(previous)
  let _ = put_config(FluxerGatewayRolloutConfig, Some(config))
  assert rollout.session_rollout_percentage() == 0
  Paused(guard)
}

pub fn resume_sessions(paused: Paused) -> Nil {
  let ack = process.new_subject()
  process.send(paused.guard, Finished(ack))
  let assert Ok(Nil) = process.receive(ack, 5000)
  Nil
}

pub fn restored() -> Bool {
  rollout.voice_e2ee_scope() != rollout.PlatformWide
}

fn start_guard(previous: Option(rollout.RolloutConfig)) -> Subject(GuardEvent) {
  let runner = process.self()
  let handshake = process.new_subject()
  process.spawn_unlinked(fn() {
    let inbox = process.new_subject()
    process.send(handshake, inbox)
    process.new_selector()
    |> process.select(inbox)
    |> process.select_specific_monitor(process.monitor(runner), fn(_) {
      RunnerGone
    })
    |> guard(previous, None)
  })
  let assert Ok(inbox) = process.receive(handshake, 5000)
  inbox
}

fn guard(
  events: process.Selector(GuardEvent),
  previous: Option(rollout.RolloutConfig),
  worker: Option(Pid),
) -> Nil {
  case process.selector_receive_forever(events) {
    Watch(pid) -> guard(events, previous, Some(pid))
    Finished(reply) -> {
      let _ = put_config(FluxerGatewayRolloutConfig, previous)
      process.send(reply, Nil)
    }
    RunnerGone -> {
      case worker {
        Some(pid) -> process.kill(pid)
        None -> Nil
      }
      let _ = put_config(FluxerGatewayRolloutConfig, previous)
      Nil
    }
  }
}
