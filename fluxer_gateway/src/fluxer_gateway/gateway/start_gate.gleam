// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/durations
import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/gateway/abuse_protection
import fluxer_gateway/gateway/ip_connections
import fluxer_gateway/gateway/rate_limit
import fluxer_gateway/gateway/wait_queue.{type WaitQueue}
import fluxer_gateway/telemetry/metrics
import gleam/dict.{type Dict}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Monitor, type Name, type Pid, type Subject}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.gateway().start_gate_init_timeout_ms
}

pub opaque type StartPermit {
  SessionPermit(holder: Option(Pid))
  GuildPermit(holder: Option(Pid))
}

type Kind {
  SessionStart
  GuildStart
}

type AtomicsRef

type AtomicsOption {
  Signed(Bool)
}

type Gate {
  Gate(counters: AtomicsRef, name: Name(Message))
}

pub opaque type Message {
  Held(holder: Pid, kind: Kind)
  Released(holder: Pid, kind: Kind)
  Wait(waiter: Subject(StartPermit))
  CancelWait(waiter: Subject(StartPermit), done: Subject(Nil))
  RolloutChanged
  HolderDown(process.Down)
  PruneRates
  SweepOwners
}

type Holder {
  Holder(sessions: Int, guilds: Int, monitor: Monitor)
}

type State {
  State(
    gate: Gate,
    holders: Dict(Pid, Holder),
    waiters: WaitQueue(Pid, Subject(StartPermit)),
    waiter_monitors: Dict(Pid, Monitor),
    self: Subject(Message),
  )
}

fn cancel_ack_ms() -> Int {
  durations.gateway().start_gate_cancel_ack_ms
}

const session_slot = 1

const guild_slot = 2

const queue_slot = 3

@external(erlang, "atomics", "new")
fn atomics_new(arity: Int, options: List(AtomicsOption)) -> AtomicsRef

@external(erlang, "atomics", "get")
fn atomics_get(ref: AtomicsRef, index: Int) -> Int

@external(erlang, "atomics", "put")
fn atomics_put(ref: AtomicsRef, index: Int, value: Int) -> Atom

@external(erlang, "atomics", "sub_get")
fn atomics_sub_get(ref: AtomicsRef, index: Int, by: Int) -> Int

@external(erlang, "atomics", "compare_exchange")
fn atomics_compare_exchange(
  ref: AtomicsRef,
  index: Int,
  expected: Int,
  desired: Int,
) -> Exchanged

type Exchanged

@external(erlang, "erlang", "is_integer")
fn lost_race(exchanged: Exchanged) -> Bool

@external(erlang, "persistent_term", "get")
fn persistent_get(key: Atom, default: Option(Gate)) -> Option(Gate)

@external(erlang, "persistent_term", "put")
fn persistent_put(key: Atom, value: Option(Gate)) -> Atom

@external(erlang, "erlang", "process_flag")
fn process_flag(flag: Atom, value: Int) -> Int

fn gate_key() -> Atom {
  atom.create("fluxer_gateway_start_gate")
}

fn gate() -> Result(Gate, Nil) {
  persistent_get(gate_key(), None) |> option.to_result(Nil)
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  let name = process.new_name("fluxer_gateway_start_gate")
  supervision.worker(fn() { start(name) })
}

fn start(name: Name(Message)) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(named) {
    Ok(initialise(named, name))
  })
  |> actor.named(name)
  |> actor.on_message(handle)
  |> actor.start
}

fn initialise(
  named: Subject(Message),
  name: Name(Message),
) -> actor.Initialised(State, Message, Nil) {
  let self = process.new_subject()
  process_flag(atom.create("fullsweep_after"), 50)
  rate_limit.create_tables()
  abuse_protection.create_tables()
  ip_connections.create_tables()
  let gate = Gate(counters: atomics_new(3, [Signed(True)]), name:)
  persistent_put(gate_key(), Some(gate))
  metrics.register_collector("fluxer_gateway_start_gate", collect)
  let rollout_changes = process.new_subject()
  rollout.subscribe_changes(rollout_changes)
  process.send_after(self, rate_limit.shared_cleanup_interval_ms(), PruneRates)
  process.send_after(
    self,
    ip_connections.owner_sweep_interval_ms(),
    SweepOwners,
  )
  let selector =
    process.new_selector()
    |> process.select(named)
    |> process.select(self)
    |> process.select_map(rollout_changes, fn(_) { RolloutChanged })
    |> process.select_monitors(HolderDown)
  actor.initialised(State(
    gate:,
    holders: dict.new(),
    waiters: wait_queue.new(),
    waiter_monitors: dict.new(),
    self:,
  ))
  |> actor.selecting(selector)
}

fn handle(state: State, message: Message) -> actor.Next(State, Message) {
  case message {
    Held(holder, kind) -> actor.continue(hold(state, holder, kind))
    Released(holder, kind) ->
      actor.continue(settle(release(state, holder, kind, 1)))
    Wait(waiter) -> actor.continue(settle(enqueue(state, waiter)))
    CancelWait(waiter, done) -> {
      let state = publish_queue(cancel(state, waiter))
      process.send(done, Nil)
      actor.continue(state)
    }
    RolloutChanged -> actor.continue(settle(state))
    HolderDown(process.ProcessDown(pid:, ..)) ->
      actor.continue(settle(holder_down(state, pid)))
    HolderDown(process.PortDown(..)) -> actor.continue(state)
    PruneRates -> {
      rate_limit.prune()
      abuse_protection.prune()
      process.send_after(
        state.self,
        rate_limit.shared_cleanup_interval_ms(),
        PruneRates,
      )
      actor.continue(state)
    }
    SweepOwners -> {
      ip_connections.reclaim_dead_owners()
      process.send_after(
        state.self,
        ip_connections.owner_sweep_interval_ms(),
        SweepOwners,
      )
      actor.continue(state)
    }
  }
}

fn slot_of(kind: Kind) -> Int {
  case kind {
    SessionStart -> session_slot
    GuildStart -> guild_slot
  }
}

fn held(entry: Holder, kind: Kind) -> Int {
  case kind {
    SessionStart -> entry.sessions
    GuildStart -> entry.guilds
  }
}

fn with_held(entry: Holder, kind: Kind, count: Int) -> Holder {
  case kind {
    SessionStart -> Holder(..entry, sessions: count)
    GuildStart -> Holder(..entry, guilds: count)
  }
}

fn hold(state: State, holder: Pid, kind: Kind) -> State {
  let entry = case dict.get(state.holders, holder) {
    Ok(entry) -> entry
    Error(Nil) ->
      Holder(sessions: 0, guilds: 0, monitor: process.monitor(holder))
  }
  let entry = with_held(entry, kind, held(entry, kind) + 1)
  State(..state, holders: dict.insert(state.holders, holder, entry))
}

fn release(state: State, holder: Pid, kind: Kind, count: Int) -> State {
  case dict.get(state.holders, holder) {
    Error(Nil) -> state
    Ok(entry) -> {
      let released = int.min(held(entry, kind), count)
      decrement(state.gate.counters, slot_of(kind), released)
      let entry = with_held(entry, kind, held(entry, kind) - released)
      case entry.sessions + entry.guilds {
        0 -> {
          process.demonitor_process(entry.monitor)
          State(..state, holders: dict.delete(state.holders, holder))
        }
        _ -> State(..state, holders: dict.insert(state.holders, holder, entry))
      }
    }
  }
}

fn holder_down(state: State, pid: Pid) -> State {
  let state = case dict.get(state.holders, pid) {
    Ok(entry) ->
      state
      |> release(pid, SessionStart, entry.sessions)
      |> release(pid, GuildStart, entry.guilds)
    Error(Nil) -> state
  }
  State(
    ..state,
    waiters: wait_queue.drop_owner(state.waiters, pid),
    waiter_monitors: dict.delete(state.waiter_monitors, pid),
  )
}

fn enqueue(state: State, waiter: Subject(StartPermit)) -> State {
  case process.subject_owner(waiter) {
    Error(Nil) -> state
    Ok(owner) -> {
      let monitors = case dict.has_key(state.waiter_monitors, owner) {
        True -> state.waiter_monitors
        False ->
          dict.insert(state.waiter_monitors, owner, process.monitor(owner))
      }
      State(
        ..state,
        waiters: wait_queue.push(state.waiters, owner, waiter),
        waiter_monitors: monitors,
      )
    }
  }
}

fn cancel(state: State, waiter: Subject(StartPermit)) -> State {
  case process.subject_owner(waiter) {
    Error(Nil) -> state
    Ok(owner) ->
      State(..state, waiters: wait_queue.cancel(state.waiters, owner, waiter))
      |> forget_owner(owner)
  }
}

fn forget_owner(state: State, owner: Pid) -> State {
  case
    wait_queue.has_owner(state.waiters, owner),
    dict.get(state.waiter_monitors, owner)
  {
    False, Ok(monitor) -> {
      process.demonitor_process(monitor)
      State(..state, waiter_monitors: dict.delete(state.waiter_monitors, owner))
    }
    _, _ -> state
  }
}

fn settle(state: State) -> State {
  publish_queue(admit(state))
}

fn publish_queue(state: State) -> State {
  atomics_put(state.gate.counters, queue_slot, wait_queue.length(state.waiters))
  state
}

fn admit(state: State) -> State {
  case
    wait_queue.length(state.waiters) > 0
    && acquire(
      state.gate.counters,
      session_slot,
      rollout.max_concurrent_session_starts(),
    )
  {
    False -> state
    True ->
      case wait_queue.pop(state.waiters) {
        Error(Nil) -> {
          decrement(state.gate.counters, session_slot, 1)
          State(..state, waiters: wait_queue.new())
        }
        Ok(#(owner, waiter, waiters)) -> {
          process.send(waiter, SessionPermit(holder: Some(owner)))
          State(..state, waiters:)
          |> forget_owner(owner)
          |> hold(owner, SessionStart)
          |> admit
        }
      }
  }
}

fn acquire(counters: AtomicsRef, slot: Int, limit: Int) -> Bool {
  let current = atomics_get(counters, slot)
  case current >= limit {
    True -> False
    False -> {
      let exchanged =
        atomics_compare_exchange(counters, slot, current, current + 1)
      case lost_race(exchanged) {
        True -> acquire(counters, slot, limit)
        False -> True
      }
    }
  }
}

fn decrement(counters: AtomicsRef, slot: Int, by: Int) -> Nil {
  case by > 0 && atomics_sub_get(counters, slot, by) < 0 {
    True -> {
      atomics_put(counters, slot, 0)
      Nil
    }
    False -> Nil
  }
}

fn send_to_gate(gate: Gate, message: Message) -> Nil {
  case process.named(gate.name) {
    Ok(_) -> process.send(process.named_subject(gate.name), message)
    Error(Nil) -> Nil
  }
}

pub fn try_session_start() -> Result(StartPermit, Int) {
  case gate() {
    Error(Nil) -> Ok(SessionPermit(holder: None))
    Ok(gate) ->
      case
        acquire(
          gate.counters,
          session_slot,
          rollout.max_concurrent_session_starts(),
        )
      {
        True -> {
          let holder = process.self()
          send_to_gate(gate, Held(holder, SessionStart))
          Ok(SessionPermit(holder: Some(holder)))
        }
        False -> Error(int.max(0, atomics_get(gate.counters, queue_slot)))
      }
  }
}

pub fn wait_session_start(waiter: Subject(StartPermit)) -> Nil {
  case gate() {
    Ok(gate) -> send_to_gate(gate, Wait(waiter))
    Error(Nil) -> process.send(waiter, SessionPermit(holder: None))
  }
}

pub fn cancel_session_start(waiter: Subject(StartPermit)) -> Nil {
  case gate() {
    Ok(gate) ->
      case process.named(gate.name) {
        Ok(_) -> {
          let done = process.new_subject()
          process.send(
            process.named_subject(gate.name),
            CancelWait(waiter, done),
          )
          let _ = process.receive(done, cancel_ack_ms())
          Nil
        }
        Error(Nil) -> Nil
      }
    Error(Nil) -> Nil
  }
}

pub fn release_session_start(permit: StartPermit) -> Nil {
  case permit, gate() {
    SessionPermit(holder: Some(holder)), Ok(gate) ->
      case process.named(gate.name) {
        Ok(_) -> send_to_gate(gate, Released(holder, SessionStart))
        Error(Nil) -> decrement(gate.counters, session_slot, 1)
      }
    _, _ -> Nil
  }
}

pub fn try_guild_start() -> Result(StartPermit, Nil) {
  case gate() {
    Error(Nil) -> Ok(GuildPermit(holder: None))
    Ok(gate) ->
      case
        acquire(
          gate.counters,
          guild_slot,
          rollout.max_concurrent_guild_starts(),
        )
      {
        True -> {
          let holder = process.self()
          send_to_gate(gate, Held(holder, GuildStart))
          Ok(GuildPermit(holder: Some(holder)))
        }
        False -> Error(Nil)
      }
  }
}

pub fn release_guild_start(permit: StartPermit) -> Nil {
  case permit, gate() {
    GuildPermit(holder: Some(holder)), Ok(gate) ->
      case process.named(gate.name) {
        Ok(_) -> send_to_gate(gate, Released(holder, GuildStart))
        Error(Nil) -> decrement(gate.counters, guild_slot, 1)
      }
    _, _ -> Nil
  }
}

fn session_start_count() -> Int {
  count(session_slot)
}

@internal
pub fn guild_start_count() -> Int {
  count(guild_slot)
}

fn session_queue_length() -> Int {
  count(queue_slot)
}

fn count(slot: Int) -> Int {
  gate()
  |> result.map(fn(gate) { int.max(0, atomics_get(gate.counters, slot)) })
  |> result.unwrap(0)
}

fn collect() -> List(metrics.Sample) {
  [
    metrics.Sample(
      "fluxer_gateway_concurrent_session_starts",
      [],
      session_start_count(),
    ),
    metrics.Sample(
      "fluxer_gateway_queued_session_starts",
      [],
      session_queue_length(),
    ),
    metrics.Sample(
      "fluxer_gateway_concurrent_guild_starts",
      [],
      guild_start_count(),
    ),
  ]
}
