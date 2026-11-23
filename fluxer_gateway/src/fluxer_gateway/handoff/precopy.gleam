// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/cluster/remote
import fluxer_gateway/config/env
import fluxer_gateway/core/node
import fluxer_gateway/handoff/adapter.{type Adapter, type Precopy}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/erlang/process.{type Monitor, type Name, type Pid, type Subject}
import gleam/list
import gleam/option
import gleam/otp/actor
import gleam/result

pub type Envelope {
  Bulk(move: MoveId, entity: Key, blob: BitArray, reply: Subject(Ack))
  Round(move: MoveId, round: Int, blob: BitArray, reply: Subject(Ack))
  Drop(move: MoveId)
}

pub type Ack {
  Staged(move: MoveId, round: Int)
  Failed(move: MoveId, round: Int, reason: String)
}

pub type Plan {
  Plan(
    max_rounds: Int,
    settled_changes: Int,
    bulk_timeout_ms: Int,
    round_timeout_ms: Int,
    budget_ms: Int,
  )
}

pub type Copied {
  Copied(rounds: Int, last_changes: Int, bulk_bytes: Int)
}

pub fn default_plan() -> Plan {
  Plan(
    max_rounds: 6,
    settled_changes: 256,
    bulk_timeout_ms: 30_000,
    round_timeout_ms: 5000,
    budget_ms: 30_000,
  )
}

const take_timeout_ms = 5000

const staged_ttl_ms = 180_000

pub fn receiver_name() -> Name(Envelope) {
  env.process_name("fluxer_gateway_handoff_precopy")
}

fn local(pid: Pid) -> Subject(Local) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_handoff_precopy_local")),
  )
}

pub fn run(
  hooks: Precopy,
  entity: Key,
  move: MoveId,
  version: Int,
  to: WorkerId,
  plan: Plan,
  now_ms: fn() -> Int,
) -> Result(Copied, String) {
  let started = now_ms()
  let target = node.from_atom(to.node)
  let reply = process.new_subject()
  let outcome = {
    use bulk <- result.try(
      hooks.snapshot(entity, move, version)
      |> result.map_error(adapter.describe),
    )
    use _ <- result.try(deliver(
      target,
      Bulk(move:, entity:, blob: bulk, reply:),
    ))
    use _ <- result.try(await(reply, move, 0, plan.bulk_timeout_ms))
    use #(rounds, changes) <- result.map(rounds(
      hooks,
      entity,
      move,
      version,
      target,
      reply,
      plan,
      started,
      now_ms,
      1,
    ))
    Copied(
      rounds:,
      last_changes: changes,
      bulk_bytes: bit_array.byte_size(bulk),
    )
  }
  case outcome {
    Ok(_) -> outcome
    Error(_) -> {
      hooks.cancel(entity, move)
      drop(to, move)
      outcome
    }
  }
}

fn rounds(
  hooks: Precopy,
  entity: Key,
  move: MoveId,
  version: Int,
  target: node.Node,
  reply: Subject(Ack),
  plan: Plan,
  started: Int,
  now_ms: fn() -> Int,
  round: Int,
) -> Result(#(Int, Int), String) {
  use #(blob, changes) <- result.try(
    hooks.changes(entity, move, version, round)
    |> result.map_error(adapter.describe),
  )
  use _ <- result.try(deliver(target, Round(move:, round:, blob:, reply:)))
  use _ <- result.try(await(reply, move, round, plan.round_timeout_ms))
  case
    changes <= plan.settled_changes
    || round >= plan.max_rounds
    || now_ms() - started >= plan.budget_ms
  {
    True -> Ok(#(round, changes))
    False ->
      rounds(
        hooks,
        entity,
        move,
        version,
        target,
        reply,
        plan,
        started,
        now_ms,
        round + 1,
      )
  }
}

fn deliver(target: node.Node, envelope: Envelope) -> Result(Nil, String) {
  remote.deliver_named(target, receiver_name(), envelope)
  |> result.replace_error("precopy target unreachable")
}

fn await(
  reply: Subject(Ack),
  move: MoveId,
  round: Int,
  timeout_ms: Int,
) -> Result(Nil, String) {
  case process.receive(reply, timeout_ms) {
    Ok(Staged(found, at)) if found == move && at == round -> Ok(Nil)
    Ok(Failed(found, at, reason)) if found == move && at == round ->
      Error(reason)
    Ok(_) -> await(reply, move, round, timeout_ms)
    Error(Nil) -> Error("precopy target did not answer")
  }
}

pub fn drop(to: WorkerId, move: MoveId) -> Nil {
  let _ =
    remote.send_named(node.from_atom(to.node), receiver_name(), Drop(move))
  Nil
}

pub fn take(move: MoveId) -> Result(Pid, Nil) {
  use pid <- result.try(process.named(receiver_name()))
  let reply = process.new_subject()
  process.send(local(pid), Take(move, reply))
  process.receive(reply, take_timeout_ms) |> result.flatten
}

type Local {
  Take(move: MoveId, reply: Subject(Result(Pid, Nil)))
  Built(move: MoveId, result: Result(Pid, String), reply: Subject(Ack))
  Gone(down: process.Down)
  Expire(move: MoveId, pid: Pid)
}

type Message {
  FromSource(Envelope)
  Locally(Local)
}

type Slot {
  Building(hooks: Precopy, dropped: Bool)
  Ready(pid: Pid, monitor: Monitor, hooks: Precopy)
}

type State {
  State(
    hooks: Dict(Int, Precopy),
    slots: Dict(MoveId, Slot),
    local: Subject(Local),
    ttl_ms: Int,
  )
}

pub fn start(
  adapters: List(Adapter),
) -> Result(actor.Started(Nil), actor.StartError) {
  start_expiring(adapters, staged_ttl_ms)
}

pub fn start_expiring(
  adapters: List(Adapter),
  ttl_ms: Int,
) -> Result(actor.Started(Nil), actor.StartError) {
  let receiver = receiver_name()
  let hooks =
    list.filter_map(adapters, fn(entry) {
      option.to_result(entry.precopy, Nil)
      |> result.map(fn(found) { #(entry.ns, found) })
    })
    |> dict.from_list
  actor.new_with_initialiser(1000, fn(_) {
    use _ <- result.map(
      process.register(process.self(), receiver)
      |> result.replace_error("handoff precopy name taken"),
    )
    let local = local(process.self())
    let selector =
      process.new_selector()
      |> process.select_map(process.named_subject(receiver), FromSource)
      |> process.select_map(local, Locally)
      |> process.select_monitors(fn(down) { Locally(Gone(down)) })
    State(hooks:, slots: dict.new(), local:, ttl_ms:)
    |> actor.initialised
    |> actor.selecting(selector)
    |> actor.returning(Nil)
  })
  |> actor.on_message(handle)
  |> actor.start
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    FromSource(Bulk(move:, entity:, blob:, reply:)) ->
      actor.continue(bulk(state, move, entity, blob, reply))
    FromSource(Round(move:, round:, blob:, reply:)) -> {
      round_in(state, move, round, blob, reply)
      actor.continue(state)
    }
    FromSource(Drop(move)) -> actor.continue(dropped(state, move))
    Locally(Take(move, reply)) -> {
      case dict.get(state.slots, move) {
        Ok(Ready(pid:, monitor:, ..)) -> {
          process.demonitor_process(monitor)
          process.send(reply, Ok(pid))
          actor.continue(State(..state, slots: dict.delete(state.slots, move)))
        }
        _ -> {
          process.send(reply, Error(Nil))
          actor.continue(state)
        }
      }
    }
    Locally(Built(move, result, reply)) ->
      actor.continue(built(state, move, result, reply))
    Locally(Gone(process.ProcessDown(pid:, ..))) ->
      actor.continue(
        State(
          ..state,
          slots: dict.filter(state.slots, fn(_, slot) {
            case slot {
              Ready(pid: found, ..) -> found != pid
              Building(..) -> True
            }
          }),
        ),
      )
    Locally(Gone(_)) -> actor.continue(state)
    Locally(Expire(move, pid)) ->
      case dict.get(state.slots, move) {
        Ok(Ready(pid: found, ..)) if found == pid ->
          actor.continue(dropped(state, move))
        _ -> actor.continue(state)
      }
  }
}

fn bulk(
  state: State,
  move: MoveId,
  entity: Key,
  blob: BitArray,
  reply: Subject(Ack),
) -> State {
  case dict.get(state.hooks, entity.ns), dict.has_key(state.slots, move) {
    _, True -> {
      process.send(reply, Failed(move, 0, "precopy already staged"))
      state
    }
    Error(Nil), False -> {
      process.send(reply, Failed(move, 0, "no precopy for this kind"))
      state
    }
    Ok(hooks), False -> {
      let local = state.local
      process.spawn_unlinked(fn() {
        let result =
          hooks.stage(entity, move, blob) |> result.map_error(adapter.describe)
        process.send(local, Built(move, result, reply))
      })
      State(
        ..state,
        slots: dict.insert(state.slots, move, Building(hooks:, dropped: False)),
      )
    }
  }
}

fn built(
  state: State,
  move: MoveId,
  result: Result(Pid, String),
  reply: Subject(Ack),
) -> State {
  case dict.get(state.slots, move), result {
    Ok(Building(hooks:, dropped: False)), Ok(pid) -> {
      process.send(reply, Staged(move, 0))
      process.send_after(state.local, state.ttl_ms, Expire(move, pid))
      State(
        ..state,
        slots: dict.insert(
          state.slots,
          move,
          Ready(pid:, monitor: process.monitor(pid), hooks:),
        ),
      )
    }
    Ok(Building(hooks:, dropped: True)), Ok(pid) -> {
      hooks.unstage(pid)
      process.send(reply, Failed(move, 0, "precopy dropped"))
      State(..state, slots: dict.delete(state.slots, move))
    }
    _, Ok(pid) -> {
      process.kill(pid)
      State(..state, slots: dict.delete(state.slots, move))
    }
    _, Error(reason) -> {
      process.send(reply, Failed(move, 0, reason))
      State(..state, slots: dict.delete(state.slots, move))
    }
  }
}

fn round_in(
  state: State,
  move: MoveId,
  round: Int,
  blob: BitArray,
  reply: Subject(Ack),
) -> Nil {
  case dict.get(state.slots, move) {
    Ok(Ready(pid:, hooks:, ..)) -> {
      process.spawn_unlinked(fn() {
        case hooks.restage(pid, round, blob) {
          Ok(Nil) -> process.send(reply, Staged(move, round))
          Error(error) ->
            process.send(reply, Failed(move, round, adapter.describe(error)))
        }
      })
      Nil
    }
    _ -> process.send(reply, Failed(move, round, "precopy not staged"))
  }
}

fn dropped(state: State, move: MoveId) -> State {
  case dict.get(state.slots, move) {
    Ok(Ready(pid:, monitor:, hooks:)) -> {
      process.demonitor_process(monitor)
      hooks.unstage(pid)
      State(..state, slots: dict.delete(state.slots, move))
    }
    Ok(Building(hooks:, ..)) ->
      State(
        ..state,
        slots: dict.insert(state.slots, move, Building(hooks:, dropped: True)),
      )
    Error(Nil) -> state
  }
}
