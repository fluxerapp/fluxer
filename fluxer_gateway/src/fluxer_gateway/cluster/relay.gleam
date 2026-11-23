// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/remote
import fluxer_gateway/config/durations
import fluxer_gateway/config/env
import fluxer_gateway/config/rollout
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node.{type Node}
import fluxer_gateway/core/session.{type SessionInbox, type SessionRef}
import gleam/dict
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result

fn init_timeout_ms() -> Int {
  durations.cluster().relay_init_timeout_ms
}

const kill_threshold = 50_000

fn claim_deadline_ms() -> Int {
  durations.cluster().relay_claim_deadline_ms
}

fn claim_backoff_ms() -> Int {
  durations.cluster().relay_claim_backoff_ms
}

type Inbox =
  Subject(SessionInbox)

pub opaque type WorkerMessage {
  Deliver(inboxes: List(Inbox), messages: List(SessionInbox))
}

pub opaque type RemoteMessage {
  RemoteDeliver(inboxes: List(Inbox), messages: List(SessionInbox))
}

type NameTable

type Atomics

type AtomicsOption {
  Signed(Bool)
}

type Shards {
  Shards(count: Int, names: NameTable, credits: Atomics)
}

type Key {
  FluxerGatewayRelayShards
}

type Stored

type ProcessFlag {
  MessageQueueData
  FullsweepAfter
}

type QueueMode {
  OffHeap
}

@external(erlang, "erlang", "list_to_tuple")
fn names_table(names: List(Name(WorkerMessage))) -> NameTable

@external(erlang, "erlang", "element")
fn name_at(index: Int, table: NameTable) -> Name(WorkerMessage)

@external(erlang, "atomics", "new")
fn atomics_new(size: Int, options: List(AtomicsOption)) -> Atomics

@external(erlang, "atomics", "add_get")
fn atomics_add_get(cells: Atomics, index: Int, by: Int) -> Int

@external(erlang, "atomics", "sub")
fn atomics_sub(cells: Atomics, index: Int, by: Int) -> Stored

@external(erlang, "atomics", "add")
fn atomics_add(cells: Atomics, index: Int, by: Int) -> Stored

@external(erlang, "atomics", "get")
fn atomics_get(cells: Atomics, index: Int) -> Int

@external(erlang, "atomics", "put")
fn atomics_put(cells: Atomics, index: Int, value: Int) -> Stored

@external(erlang, "persistent_term", "get")
fn get_shards(key: Key, default: Option(Shards)) -> Option(Shards)

@external(erlang, "persistent_term", "put")
fn put_shards(key: Key, value: Option(Shards)) -> Stored

@external(erlang, "erlang", "phash2")
fn phash2(term: a, range: Int) -> Int

@external(erlang, "erlang", "node")
fn pid_node(pid: Pid) -> Atom

@external(erlang, "erlang", "node")
fn self_node() -> Atom

@external(erlang, "erlang", "process_flag")
fn queue_flag(flag: ProcessFlag, mode: QueueMode) -> QueueMode

@external(erlang, "erlang", "process_flag")
fn sweep_flag(flag: ProcessFlag, value: Int) -> Int

fn remote_name() -> Name(RemoteMessage) {
  env.process_name("fluxer_gateway_dispatch_relay")
}

fn worker_name(index: Int) -> Name(WorkerMessage) {
  env.process_name("fluxer_gateway_dispatch_relay_" <> int.to_string(index))
}

pub fn supervised(_settings: Settings) -> ChildSpecification(Nil) {
  supervision.supervisor(start)
}

fn start() -> Result(actor.Started(Nil), actor.StartError) {
  let count = rollout.dispatch_relay_shards()
  let names = list.map(indices(0, count - 1, []), worker_name)
  let shards =
    Shards(
      count:,
      names: names_table(names),
      credits: atomics_new(count, [Signed(True)]),
    )
  let builder =
    static_supervisor.new(static_supervisor.OneForOne)
    |> static_supervisor.restart_tolerance(intensity: 10, period: 10)
    |> static_supervisor.add(supervision.worker(fn() { start_remote(shards) }))
  list.index_fold(names, builder, fn(builder, worker, index) {
    static_supervisor.add(
      builder,
      supervision.worker(fn() { start_worker(worker, index + 1, shards) }),
    )
  })
  |> static_supervisor.start
  |> result.map(fn(started) { actor.Started(pid: started.pid, data: Nil) })
}

fn start_remote(
  shards: Shards,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let _ = queue_flag(MessageQueueData, OffHeap)
    let _ = sweep_flag(FullsweepAfter, 50)
    put_shards(FluxerGatewayRelayShards, Some(shards))
    let selector = process.new_selector() |> process.select(self)
    Nil
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(remote_name())
  |> actor.on_message(fn(state, message) {
    let RemoteDeliver(inboxes, messages) = message
    deliver(inboxes, messages)
    actor.continue(state)
  })
  |> actor.start
}

fn start_worker(
  worker: Name(WorkerMessage),
  slot: Int,
  shards: Shards,
) -> Result(actor.Started(Nil), actor.StartError) {
  actor.new_with_initialiser(init_timeout_ms(), fn(self) {
    let _ = queue_flag(MessageQueueData, OffHeap)
    let _ = sweep_flag(FullsweepAfter, 50)
    atomics_put(shards.credits, slot, 0)
    let selector = process.new_selector() |> process.select(self)
    Nil
    |> actor.initialised
    |> actor.selecting(selector)
    |> Ok
  })
  |> actor.named(worker)
  |> actor.on_message(fn(state, message) {
    let Deliver(inboxes, messages) = message
    atomics_sub(shards.credits, slot, 1)
    deliver(inboxes, messages)
    actor.continue(state)
  })
  |> actor.start
}

pub fn settle(timeout_ms: Int) -> Nil {
  case get_shards(FluxerGatewayRelayShards, None) {
    None -> Nil
    Some(shards) -> settle_until(shards, clock.monotonic_ms() + timeout_ms)
  }
}

fn settle_until(shards: Shards, deadline: Int) -> Nil {
  let busy =
    indices(1, shards.count, [])
    |> list.any(fn(slot) { atomics_get(shards.credits, slot) > 0 })
  case busy && clock.monotonic_ms() < deadline {
    True -> {
      process.sleep(1)
      settle_until(shards, deadline)
    }
    False -> Nil
  }
}

pub fn send(sessions: List(SessionRef), message: SessionInbox) -> Nil {
  send_all(sessions, [message])
}

pub fn send_all(
  sessions: List(SessionRef),
  messages: List(SessionInbox),
) -> Nil {
  let inboxes = list.map(sessions, fn(session) { session.inbox })
  case inboxes, messages, get_shards(FluxerGatewayRelayShards, None) {
    [], _, _ | _, [], _ -> Nil
    _, _, None -> deliver(inboxes, messages)
    _, _, Some(shards) -> relay(shards, inboxes, messages)
  }
}

fn relay(
  shards: Shards,
  inboxes: List(Inbox),
  messages: List(SessionInbox),
) -> Nil {
  let buckets =
    list.fold(inboxes, dict.new(), fn(buckets, inbox) {
      let slot = case process.subject_owner(inbox) {
        Ok(pid) -> phash2(pid, shards.count) + 1
        Error(Nil) -> 1
      }
      dict.upsert(buckets, slot, fn(existing) {
        [inbox, ..option.unwrap(existing, [])]
      })
    })
  let max_queue = max_queue()
  let deferred =
    dict.fold(buckets, [], fn(deferred, slot, bucket) {
      case claim(shards, slot, max_queue) {
        True -> {
          enqueue(shards, slot, Deliver(list.reverse(bucket), messages))
          deferred
        }
        False -> [#(slot, list.reverse(bucket)), ..deferred]
      }
    })
  let deadline = clock.monotonic_ms() + claim_deadline_ms()
  list.each(deferred, fn(entry) {
    let #(slot, bucket) = entry
    enqueue_blocking(
      shards,
      slot,
      Deliver(bucket, messages),
      max_queue,
      deadline,
    )
  })
}

fn enqueue_blocking(
  shards: Shards,
  slot: Int,
  message: WorkerMessage,
  max_queue: Int,
  deadline: Int,
) -> Nil {
  case claim(shards, slot, max_queue) {
    True -> enqueue(shards, slot, message)
    False ->
      case clock.monotonic_ms() >= deadline {
        True -> {
          atomics_add(shards.credits, slot, 1)
          enqueue(shards, slot, message)
        }
        False -> {
          process.sleep(claim_backoff_ms())
          enqueue_blocking(shards, slot, message, max_queue, deadline)
        }
      }
  }
}

fn claim(shards: Shards, slot: Int, max_queue: Int) -> Bool {
  case atomics_add_get(shards.credits, slot, 1) <= max_queue {
    True -> True
    False -> {
      atomics_sub(shards.credits, slot, 1)
      False
    }
  }
}

fn enqueue(shards: Shards, slot: Int, message: WorkerMessage) -> Nil {
  case remote.send_local(name_at(slot, shards.names), message) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      atomics_sub(shards.credits, slot, 1)
      let Deliver(inboxes, messages) = message
      deliver(inboxes, messages)
    }
  }
}

fn max_queue() -> Int {
  case rollout.dispatch_relay_max_queue() {
    configured if configured > 0 -> int.min(configured, kill_threshold)
    _ -> kill_threshold
  }
}

fn deliver(inboxes: List(Inbox), messages: List(SessionInbox)) -> Nil {
  let local = self_node()
  let remote_groups =
    list.fold(inboxes, dict.new(), fn(groups, inbox) {
      case process.subject_owner(inbox) {
        Ok(pid) ->
          case pid_node(pid) == local {
            True -> {
              list.each(messages, process.send(inbox, _))
              groups
            }
            False ->
              dict.upsert(groups, pid_node(pid), fn(existing) {
                [inbox, ..option.unwrap(existing, [])]
              })
          }
        Error(Nil) -> groups
      }
    })
  dict.each(remote_groups, fn(peer, group) {
    send_remote(node.from_atom(peer), list.reverse(group), messages)
  })
}

fn send_remote(
  peer: Node,
  inboxes: List(Inbox),
  messages: List(SessionInbox),
) -> Nil {
  let _ =
    remote.deliver_named(peer, remote_name(), RemoteDeliver(inboxes, messages))
  Nil
}

fn indices(from: Int, to: Int, acc: List(Int)) -> List(Int) {
  case to < from {
    True -> acc
    False -> indices(from, to - 1, [to, ..acc])
  }
}
