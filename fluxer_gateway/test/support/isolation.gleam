// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain_report
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings.{type Durations, type Settings, Settings}
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/push/throttle
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import support/api_breaker

pub opaque type Snapshot {
  Snapshot(
    settings: Settings,
    rollout: Dynamic,
    supervisor: Pid,
    children: List(Child),
    names: Set(Atom),
    app_leader: Pid,
    runner_leader: Pid,
  )
}

type Child {
  Child(id: Dynamic, name: Option(Atom))
}

type Key {
  FluxerGatewayTestIsolation
}

type RolloutKey {
  FluxerGatewayRolloutConfig
}

type ProcessInfoItem {
  GroupLeader
  RegisteredName
}

@external(erlang, "persistent_term", "put")
fn put_term(key: Key, value: Option(Snapshot)) -> Dynamic

@external(erlang, "persistent_term", "get")
fn get_term(key: Key, default: Option(Snapshot)) -> Option(Snapshot)

@external(erlang, "persistent_term", "put")
fn put_rollout(key: RolloutKey, value: Dynamic) -> Dynamic

@external(erlang, "persistent_term", "get")
fn get_rollout(key: RolloutKey, default: Dynamic) -> Dynamic

@external(erlang, "application", "get_supervisor")
fn get_supervisor(app: Atom) -> Result(Pid, Dynamic)

@external(erlang, "supervisor", "which_children")
fn which_children(supervisor: Pid) -> List(Dynamic)

@external(erlang, "supervisor", "restart_child")
fn restart_child(supervisor: Pid, id: Dynamic) -> Dynamic

@external(erlang, "erlang", "registered")
fn registered() -> List(Atom)

@external(erlang, "erlang", "whereis")
fn whereis(name: Atom) -> Dynamic

@external(erlang, "erlang", "is_pid")
fn is_pid(value: Dynamic) -> Bool

@external(erlang, "erlang", "group_leader")
fn group_leader() -> Pid

@external(erlang, "erlang", "group_leader")
fn set_group_leader(leader: Pid, pid: Pid) -> Bool

@external(erlang, "erlang", "process_info")
fn process_info(pid: Pid, item: ProcessInfoItem) -> Dynamic

@external(erlang, "gleam_stdlib", "identity")
fn coerce(value: a) -> b

pub fn snapshot() -> Nil {
  capture(group_leader(), set.new())
}

pub fn refresh_after_restart() -> Nil {
  case get_term(FluxerGatewayTestIsolation, None) {
    Some(previous) -> capture(previous.runner_leader, previous.names)
    None -> snapshot()
  }
}

fn capture(runner_leader: Pid, known: Set(Atom)) -> Nil {
  let assert Ok(supervisor) = get_supervisor(atom.create("fluxer_gateway"))
  let assert Ok(app_leader) = leader_of(supervisor)
  let children =
    which_children(supervisor)
    |> list.filter_map(fn(child) {
      use id <- result.map(field(child, 0))
      let name =
        field(child, 1)
        |> result.try(as_pid)
        |> result.try(registered_name)
        |> option.from_result
      Child(id:, name:)
    })
  let snapshot =
    Snapshot(
      settings: settings.get(),
      rollout: get_rollout(FluxerGatewayRolloutConfig, unset()),
      supervisor:,
      children:,
      names: set.union(known, set.from_list(registered())),
      app_leader:,
      runner_leader:,
    )
  put_term(FluxerGatewayTestIsolation, Some(snapshot))
  Nil
}

pub fn reset() -> Nil {
  case get_term(FluxerGatewayTestIsolation, None) {
    None -> Nil
    Some(snapshot) -> restore(snapshot)
  }
}

pub fn keep_across_tests() -> Nil {
  case get_term(FluxerGatewayTestIsolation, None) {
    Some(snapshot) -> {
      set_group_leader(snapshot.runner_leader, process.self())
      Nil
    }
    None -> Nil
  }
}

pub fn pristine_settings() -> Settings {
  case get_term(FluxerGatewayTestIsolation, None) {
    Some(snapshot) -> snapshot.settings
    None -> settings.get()
  }
}

pub fn set_durations(change: fn(Durations) -> Durations) -> Nil {
  let current = settings.get()
  settings.store(Settings(..current, durations: change(current.durations)))
  Nil
}

fn restore(snapshot: Snapshot) -> Nil {
  case settings.get() == snapshot.settings {
    True -> Nil
    False -> {
      settings.store(snapshot.settings)
      Nil
    }
  }
  case get_rollout(FluxerGatewayRolloutConfig, unset()) == snapshot.rollout {
    True -> Nil
    False -> {
      put_rollout(FluxerGatewayRolloutConfig, snapshot.rollout)
      Nil
    }
  }
  case draining.is_draining() {
    True -> draining.clear_draining()
    False -> Nil
  }
  drain_report.reset()
  push_metrics.open_drain()
  stop_strays(snapshot)
  restart_children(snapshot)
  let _ = api_breaker.reset()
  throttle.reset()
}

fn stop_strays(snapshot: Snapshot) -> Nil {
  registered()
  |> list.filter(fn(name) { !set.contains(snapshot.names, name) })
  |> list.filter_map(fn(name) { as_pid(whereis(name)) })
  |> list.filter(fn(pid) {
    case leader_of(pid) {
      Ok(leader) ->
        leader != snapshot.app_leader && leader != snapshot.runner_leader
      Error(Nil) -> False
    }
  })
  |> list.each(stop)
}

fn restart_children(snapshot: Snapshot) -> Nil {
  let running =
    which_children(snapshot.supervisor)
    |> list.filter_map(fn(child) {
      use id <- result.try(field(child, 0))
      use pid <- result.map(field(child, 1) |> result.try(as_pid))
      #(id, pid)
    })
  list.each(snapshot.children, fn(child) {
    case list.key_find(running, child.id) {
      Ok(_) -> Nil
      Error(Nil) -> {
        case child.name {
          Some(name) ->
            case as_pid(whereis(name)) {
              Ok(holder) -> stop(holder)
              Error(Nil) -> Nil
            }
          None -> Nil
        }
        let _ = restart_child(snapshot.supervisor, child.id)
        Nil
      }
    }
  })
}

fn stop(pid: Pid) -> Nil {
  let monitor = process.monitor(pid)
  process.unlink(pid)
  process.kill(pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(5000)
  Nil
}

fn unset() -> Dynamic {
  coerce(None)
}

fn field(term: Dynamic, index: Int) -> Result(Dynamic, Nil) {
  decode.run(term, decode.at([index], decode.dynamic))
  |> result.replace_error(Nil)
}

fn as_pid(value: Dynamic) -> Result(Pid, Nil) {
  case is_pid(value) {
    True -> Ok(coerce(value))
    False -> Error(Nil)
  }
}

fn leader_of(pid: Pid) -> Result(Pid, Nil) {
  process_info(pid, GroupLeader) |> field(1) |> result.try(as_pid)
}

fn registered_name(pid: Pid) -> Result(Atom, Nil) {
  process_info(pid, RegisteredName)
  |> field(1)
  |> result.try(fn(value) {
    decode.run(value, atom.decoder()) |> result.replace_error(Nil)
  })
}
