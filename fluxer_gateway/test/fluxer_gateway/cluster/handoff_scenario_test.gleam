// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/app/drain
import fluxer_gateway/cluster/handoff
import fluxer_gateway/cluster/membership
import fluxer_gateway/config/draining
import fluxer_gateway/config/settings
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node, type Role}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import support/cluster_app

type Gate {
  Started(targets: List(Node), resume: Subject(Bool))
  Next(reply: Subject(Option(#(List(Node), Subject(Bool)))), within: Int)
}

type Asking {
  Asking(reply: Subject(Option(#(List(Node), Subject(Bool)))), until: Int)
}

type Waiting {
  Waiting(started: List(#(List(Node), Subject(Bool))), asking: Option(Asking))
}

fn start_gate() -> Subject(Gate) {
  let assert Ok(started) =
    actor.new(Waiting([], None))
    |> actor.on_message(fn(waiting: Waiting, message) {
      case message {
        Started(targets, resume) -> {
          let now = clock.monotonic_ms()
          case waiting.asking {
            Some(asking) if asking.until > now -> {
              process.send(asking.reply, Some(#(targets, resume)))
              actor.continue(Waiting(waiting.started, None))
            }
            _ ->
              actor.continue(Waiting(
                list.append(waiting.started, [#(targets, resume)]),
                None,
              ))
          }
        }
        Next(reply, within) ->
          case waiting.started {
            [first, ..rest] -> {
              process.send(reply, Some(first))
              actor.continue(Waiting(rest, None))
            }
            [] ->
              actor.continue(Waiting(
                [],
                Some(Asking(reply, clock.monotonic_ms() + within - 10)),
              ))
          }
      }
    })
    |> actor.start
  started.data
}

fn exporter(gate: Subject(Gate)) -> handoff.Exporter {
  fn(_role: Role, targets: List(Node)) {
    let resume = process.new_subject()
    process.send(gate, Started(targets, resume))
    let assert Ok(True) = process.receive(resume, 30_000)
    handoff.Outcome(guilds: 0, sessions: 0, calls: 0)
  }
}

fn next_worker(
  gate: Subject(Gate),
  within: Int,
) -> Option(#(List(Node), Subject(Bool))) {
  let reply = process.new_subject()
  process.send(gate, Next(reply, within))
  case process.receive(reply, within) {
    Ok(found) -> found
    Error(Nil) -> None
  }
}

fn settle(gate: Subject(Gate)) -> Nil {
  case next_worker(gate, 2600) {
    Some(#(_, resume)) -> {
      process.send(resume, True)
      settle(gate)
    }
    None -> Nil
  }
}

type Key {
  FluxerGatewayClusterTopology
}

@external(erlang, "persistent_term", "erase")
fn erase_topology(key: Key) -> Bool

@external(erlang, "erlang", "whereis")
fn whereis(name: Atom) -> process.Pid

fn start_membership() -> Pid {
  let configured = settings.Settings(..settings.get(), cluster_enabled: True)
  let spec = membership.supervised(configured)
  let assert Ok(member) = spec.start()
  process.unlink(member.pid)
  member.pid
}

fn with_cluster(run: fn(Subject(Gate)) -> Nil) -> Nil {
  let gate = start_gate()
  let configured = settings.Settings(..settings.get(), cluster_enabled: True)
  start_membership()
  let handoff_spec = handoff.supervised_with(configured, exporter(gate))
  let assert Ok(mover) = handoff_spec.start()
  run(gate)
  cluster_app.stop_linked(mover.pid)
  case cluster_app.registered("fluxer_gateway_cluster_membership") {
    Ok(pid) -> cluster_app.stop_linked(pid)
    Error(Nil) -> Nil
  }
  erase_topology(FluxerGatewayClusterTopology)
  draining.clear_draining()
}

pub fn second_worker_waits_for_the_first_and_undrain_is_refused_meanwhile_test() {
  use gate <- with_cluster
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  assert drain.undrain() == Error(drain.HandoffInFlight)
  assert draining.is_draining()
  drain.start()
  assert next_worker(gate, 2600) == None
  process.send(first, True)
  let assert Some(#(_, second)) = next_worker(gate, 4000)
  process.send(second, True)
  assert next_worker(gate, 2600) == None
  assert drain.undrain() == Ok(Nil)
  assert !draining.is_draining()
}

pub fn crashed_worker_is_retried_test() {
  use gate <- with_cluster
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  process.send(first, False)
  let assert Some(#(_, retry)) = next_worker(gate, 4000)
  process.send(retry, True)
  assert next_worker(gate, 2600) == None
  assert drain.undrain() == Ok(Nil)
}

pub fn membership_restart_while_draining_resends_the_drain_moves_test() {
  use gate <- with_cluster
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  process.send(first, True)
  settle(gate)
  let old = whereis(atom.create("fluxer_gateway_cluster_membership"))
  cluster_app.stop_linked(old)
  let restarted = start_membership()
  let assert Some(#(_, again)) = next_worker(gate, 5000)
  process.send(again, True)
  settle(gate)
  assert cluster_app.registered("fluxer_gateway_cluster_membership")
    == Ok(restarted)
  assert drain.undrain() == Ok(Nil)
}

pub fn one_drain_runs_the_handoff_once_test() {
  use gate <- with_cluster
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  process.sleep(300)
  process.send(first, True)
  assert next_worker(gate, 3000) == None
  assert drain.undrain() == Ok(Nil)
}

pub fn drain_without_membership_still_hands_off_once_test() {
  use gate <- with_cluster
  let old = whereis(atom.create("fluxer_gateway_cluster_membership"))
  cluster_app.stop_linked(old)
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 1000)
  process.send(first, True)
  assert next_worker(gate, 2600) == None
  assert drain.undrain() == Ok(Nil)
  assert !draining.is_draining()
}

pub fn timers_of_a_stopped_handoff_never_reach_its_successor_test() {
  let gate = start_gate()
  let configured = settings.Settings(..settings.get(), cluster_enabled: True)
  let assert Ok(orphan) =
    handoff.supervised_with(configured, exporter(gate)).start()
  process.sleep(700)
  cluster_app.stop_linked(orphan.pid)
  use gate <- with_cluster
  process.sleep(1200)
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  process.send(first, True)
  assert next_worker(gate, 2600) == None
  assert drain.undrain() == Ok(Nil)
}

pub fn undrain_right_after_a_drain_leaves_nothing_pending_test() {
  use gate <- with_cluster
  drain.start()
  let assert Some(#(_, first)) = next_worker(gate, 3000)
  process.send(first, True)
  assert next_worker(gate, 2600) == None
  assert drain.undrain() == Ok(Nil)
  assert next_worker(gate, 2600) == None
}
