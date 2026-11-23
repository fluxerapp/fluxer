// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId}
import bouncer_proto/lease.{Held, Moving} as proto_lease
import bouncer_proto/message.{type Refusal, BucketEmptied, CloseBucket}
import bouncer_proto/worker.{type Entry, type WorkerId, WorkerId}
import fluxer_gateway/app/drain_report
import fluxer_gateway/cluster/bouncer_client
import fluxer_gateway/cluster/buckets
import fluxer_gateway/cluster/lease
import fluxer_gateway/cluster/links
import fluxer_gateway/cluster/metrics as cluster_metrics
import fluxer_gateway/cluster/wire
import fluxer_gateway/config/draining
import fluxer_gateway/config/durations
import fluxer_gateway/config/settings.{type Settings}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/manager/registry
import fluxer_gateway/handoff/adapter.{type Adapter}
import fluxer_gateway/handoff/inbound
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports.{type Ports, Ports}
import fluxer_gateway/handoff/precopy
import fluxer_gateway/handoff/source
import fluxer_gateway/presence/handoff/adapter as presence_adapter
import fluxer_gateway/presence/pids
import fluxer_gateway/presence/route
import fluxer_gateway/push/drain as push_drain
import fluxer_gateway/push/handoff/adapter as push_adapter
import fluxer_gateway/push/metrics as push_metrics
import fluxer_gateway/rpc/server as rpc_server
import fluxer_gateway/voice/call/handoff/adapter as call_adapter
import fluxer_gateway/voice/call/manager as call_manager
import gleam/bool
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/order
import gleam/otp/actor
import gleam/otp/static_supervisor
import gleam/otp/supervision.{type ChildSpecification}
import gleam/result
import gleam/string
import logging

const close_wait_ms = 5000

const close_poll_ms = 20

const drain_round_ms = 1000

pub const limits =
  move.Limits(
    entities_in_flight: 8,
    buckets_in_flight: 2,
    retry_budget_ms: 20_000,
  )

pub fn adapters(settings: Settings) -> List(Adapter) {
  list.flatten([
    case node.runs(settings.role, node.Guilds) {
      True -> [guild_adapter.adapter()]
      False -> []
    },
    case node.runs(settings.role, node.Presence) {
      True -> [presence_adapter.adapter()]
      False -> []
    },
    case node.runs(settings.role, node.Calls) {
      True -> [call_adapter.adapter()]
      False -> []
    },
    case node.runs(settings.role, node.Push) {
      True -> [push_adapter.adapter()]
      False -> []
    },
  ])
}

pub fn movable(settings: Settings) -> Bool {
  list.any([node.Guilds, node.Presence, node.Calls, node.Push], fn(role) {
    node.runs(settings.role, role)
  })
}

type Loadable

@external(erlang, "code", "all_available")
fn all_available() -> List(#(Charlist, Loadable, Bool))

type Charlist

@external(erlang, "erlang", "list_to_atom")
fn charlist_atom(name: Charlist) -> Atom

@external(erlang, "code", "ensure_modules_loaded")
fn ensure_modules_loaded(modules: List(Atom)) -> Dynamic

pub fn load_wire_atoms() -> Nil {
  let _ =
    all_available()
    |> list.map(fn(found) { charlist_atom(found.0) })
    |> list.filter(fn(module) {
      let name = atom.to_string(module)
      list.any(["fluxer_gateway@", "gleam@", "bouncer_proto@"], fn(prefix) {
        string.starts_with(name, prefix)
      })
    })
    |> ensure_modules_loaded
  Nil
}

pub fn supervised(settings: Settings) -> ChildSpecification(Nil) {
  let movable = adapters(settings)
  supervision.supervisor(fn() {
    load_wire_atoms()
    let ports = live_ports(movable)
    static_supervisor.new(static_supervisor.OneForAll)
    |> static_supervisor.add(
      supervision.worker(fn() { start_engine(movable, ports) }),
    )
    |> static_supervisor.add(
      supervision.worker(fn() { precopy.start(movable) }),
    )
    |> static_supervisor.add(
      supervision.worker(fn() {
        inbound.start_registered(movable, ports)
        |> result.map(fn(started) { actor.Started(started.pid, Nil) })
      }),
    )
    |> static_supervisor.start
    |> result.map(fn(started) { actor.Started(started.pid, Nil) })
  })
}

fn start_engine(
  movable: List(Adapter),
  ports: Ports,
) -> Result(actor.Started(Nil), actor.StartError) {
  move.start_with(movable, limits, ports)
  |> result.replace_error(actor.InitFailed("handoff engine did not start"))
  |> result.try(fn(engine) {
    process.subject_owner(engine.inbox)
    |> result.map(fn(pid) { actor.Started(pid, Nil) })
    |> result.replace_error(actor.InitFailed("handoff engine has no owner"))
  })
}

fn me() -> WorkerId {
  WorkerId(node.to_atom(node.self()), lease.incarnation())
}

pub fn live_ports(movable: List(Adapter)) -> Ports {
  let namespaces = list.map(movable, fn(entry) { entry.ns })
  Ports(
    ..ports.unwired(),
    me: me(),
    to_home: bouncer_client.to_home,
    holding: fn(bucket) { lease.valid(bucket) |> result.replace_error(Nil) },
    held: fn() { held(namespaces) },
    place:,
    target_codecs: fn(target: WorkerId) {
      case links.entry(node.from_atom(target.node)) {
        Ok(found) -> found.profile.codecs
        Error(Nil) -> []
      }
    },
    close_bucket: fn(bucket, at, to, ids) {
      close_bucket(movable, bucket, at, to, ids)
    },
    bucket_emptied: fn(bucket, at) {
      let _ = bouncer_client.to_home(bucket, BucketEmptied(me(), bucket, at))
      Nil
    },
    now_ms: clock.monotonic_ms,
    timings: source.default_timings(),
  )
}

fn held(namespaces: List(Int)) -> List(BucketId) {
  lease.held()
  |> list.map(fn(claim) { claim.bucket })
  |> list.filter(fn(bucket) { list.contains(namespaces, bucket.ns) })
}

fn own_entry() -> Result(Entry, Nil) {
  wire.own(links.all())
}

pub fn session_targets() -> List(node.Node) {
  case own_entry() {
    Error(Nil) -> []
    Ok(mine) -> {
      let ready =
        links.all()
        |> list.filter(fn(found) {
          found.status == worker.Ready
          && found.worker.node != mine.worker.node
          && worker.same_build_line(found.profile, mine.profile)
        })
      let fresh =
        list.filter(ready, fn(found) {
          found.profile.template_hash != mine.profile.template_hash
        })
      case fresh {
        [] -> ready
        _ -> fresh
      }
      |> list.map(fn(found) { node.from_atom(found.worker.node) })
    }
  }
}

pub fn place(bucket: BucketId) -> Result(WorkerId, Nil) {
  use mine <- result.try(own_entry())
  let loads =
    buckets.records()
    |> list.fold(dict.new(), fn(acc, record) {
      case record.holder {
        Some(holder) ->
          dict.upsert(acc, holder.node, fn(found) {
            option.unwrap(found, 0) + 1
          })
        _ -> acc
      }
    })
  let load = fn(found: Entry) {
    dict.get(loads, found.worker.node) |> result.unwrap(0)
  }
  links.all()
  |> list.filter(fn(found) {
    found.status == worker.Ready
    && found.worker.node != mine.worker.node
    && worker.same_build_line(found.profile, mine.profile)
    && list.any(found.profile.codecs, fn(range) { range.ns == bucket.ns })
  })
  |> list.sort(fn(a, b) {
    let fresh_a = a.profile.template_hash != mine.profile.template_hash
    let fresh_b = b.profile.template_hash != mine.profile.template_hash
    case fresh_a, fresh_b {
      True, False -> order.Lt
      False, True -> order.Gt
      _, _ -> int.compare(load(a), load(b))
    }
  })
  |> list.first
  |> result.map(fn(found) { found.worker })
}

fn close_bucket(
  movable: List(Adapter),
  bucket: BucketId,
  at: Epoch,
  to: WorkerId,
  ids: List(Int),
) -> Result(Epoch, Refusal) {
  let _ = lease.close(bucket)
  let ids =
    movable
    |> list.filter(fn(entry) { entry.ns == bucket.ns })
    |> list.flat_map(fn(entry) { entry.entities_in(bucket.bucket) })
    |> list.append(ids)
    |> list.unique
  let watcher = process.new_subject()
  let _ = bouncer_client.subscribe(watcher)
  let started = clock.monotonic_ms()
  let #(label, outcome) = case
    bouncer_client.to_home(bucket, CloseBucket(me(), bucket, at, to, ids))
  {
    Error(Nil) -> #(
      "unreachable",
      Error(message.RetryAfter(close_poll_ms * 10)),
    )
    Ok(Nil) ->
      case await_moving(bucket, at, to, watcher, started + close_wait_ms) {
        Ok(found) -> #("closed", Ok(found))
        Error(reason) -> #("refused", Error(reason))
      }
  }
  cluster_metrics.record_close(label, clock.monotonic_ms() - started)
  case outcome {
    Ok(_) -> Nil
    Error(reason) -> {
      let _ = lease.reopen(bucket)
      logging.log(
        logging.Warning,
        "handoff close of bucket "
          <> int.to_string(bucket.ns)
          <> "/"
          <> int.to_string(bucket.bucket)
          <> " refused: "
          <> string.inspect(reason)
          <> " record "
          <> string.inspect(
          buckets.record(bucket) |> result.map(fn(found) { found.state }),
        ),
      )
    }
  }
  outcome
}

fn await_moving(
  bucket: BucketId,
  at: Epoch,
  to: WorkerId,
  watcher: Subject(message.ToWorker),
  deadline: Int,
) -> Result(Epoch, Refusal) {
  case moved_to(bucket, at, to) {
    Ok(to_epoch) -> Ok(to_epoch)
    Error(Nil) ->
      case clock.monotonic_ms() >= deadline {
        True -> Error(message.RetryAfter(close_poll_ms * 10))
        False ->
          case process.receive(watcher, close_poll_ms) {
            Ok(message.CloseRefused(bucket: refused, reason:))
              if refused == bucket
            -> Error(reason)
            _ -> await_moving(bucket, at, to, watcher, deadline)
          }
      }
  }
}

fn moved_to(bucket: BucketId, at: Epoch, to: WorkerId) -> Result(Epoch, Nil) {
  case buckets.record(bucket) {
    Ok(proto_lease.BucketRecord(state: Moving(to: target, to_epoch:, ..), ..))
      if target == to
    -> Ok(to_epoch)
    Ok(proto_lease.BucketRecord(
      holder: Some(holder),
      epoch: held_at,
      state: Held,
      ..,
    ))
      if holder == to
    ->
      case epoch.compare(held_at, at) {
        order.Gt -> Ok(held_at)
        _ -> Error(Nil)
      }
    _ -> Error(Nil)
  }
}

pub fn drain() -> Nil {
  case draining.is_draining() {
    True -> Nil
    False -> {
      draining.set_draining()
      push_metrics.open_drain()
      let _ = bouncer_client.set_draining(True)
      let serial = drain_report.serial()
      process.spawn_unlinked(fn() {
        rpc_server.stop_listening()
        drain_rounds(
          clock.monotonic_ms() + durations.app().drain_deadline_ms,
          serial,
        )
      })
      Nil
    }
  }
}

fn drain_rounds(deadline: Int, serial: Int) -> Nil {
  rounds(deadline, drain_round_ms, fn() { same_drain(serial) }, drain_round)
}

pub fn same_drain(serial: Int) -> Bool {
  draining.is_draining() && drain_report.serial() == serial
}

pub fn rounds(
  deadline: Int,
  round_ms: Int,
  active: fn() -> Bool,
  round: fn(Int) -> Bool,
) -> Nil {
  use <- bool.guard(!active(), Nil)
  use <- bool.guard(!round(deadline), Nil)
  process.sleep(round_ms)
  rounds(deadline, round_ms, active, round)
}

fn drain_round(deadline: Int) -> Bool {
  let movable = adapters(settings.get())
  let left = held(list.map(movable, fn(entry) { entry.ns }))
  hand_push()
  use <- bool.lazy_guard(clock.monotonic_ms() >= deadline, fn() {
    close_push()
    False
  })
  use <- bool.lazy_guard(left == [], fn() {
    await_push(deadline)
    False
  })
  case move.drain_local(deadline - clock.monotonic_ms()) {
    Ok(_) -> Nil
    Error(_) ->
      logging.log(logging.Warning, "bouncer drain round left buckets behind")
  }
  True
}

fn runs_outbox(settings: Settings) -> Bool {
  node.runs(settings.role, node.Push) || node.runs(settings.role, node.Calls)
}

fn pushes_jobs() -> Bool {
  runs_outbox(settings.get()) && draining.is_draining()
}

fn push_owner() -> Result(push_drain.Peer, Nil) {
  push_peer()
  |> result.map(push_drain.Peer(_, push_adapter.site()))
}

fn hand_push() -> Nil {
  case pushes_jobs(), push_owner() {
    True, Ok(peer) -> {
      let _ = push_drain.hand(push_adapter.site(), peer)
      Nil
    }
    _, _ -> Nil
  }
}

fn close_push() -> Nil {
  case pushes_jobs() {
    True -> {
      let _ =
        push_drain.close(
          push_adapter.site(),
          push_owner,
          draining.is_draining,
          drain_round_ms,
        )
      Nil
    }
    False -> Nil
  }
}

fn await_push(deadline: Int) -> Nil {
  use <- bool.guard(!pushes_jobs(), Nil)
  let _ =
    push_drain.run(
      push_adapter.site(),
      push_owner,
      draining.is_draining,
      deadline,
      drain_round_ms,
    )
  Nil
}

pub fn push_peer() -> Result(node.Node, Nil) {
  use mine <- result.try(own_entry())
  links.all()
  |> list.filter(fn(found) {
    found.status == worker.Ready
    && found.worker.node != mine.worker.node
    && worker.same_build_line(found.profile, mine.profile)
    && accepts_push_jobs(mine, found)
  })
  |> list.sort(fn(a, b) {
    let fresh_a = a.profile.template_hash != mine.profile.template_hash
    let fresh_b = b.profile.template_hash != mine.profile.template_hash
    case fresh_a, fresh_b {
      True, False -> order.Lt
      False, True -> order.Gt
      _, _ -> order.Eq
    }
  })
  |> list.first
  |> result.map(fn(found) { node.from_atom(found.worker.node) })
}

pub fn accepts_push_jobs(mine: Entry, found: Entry) -> Bool {
  found.worker.node != mine.worker.node
  && wire.speaks(node.from_atom(found.worker.node), wire.move_forms_epoch)
  && list.any(found.profile.codecs, fn(range) { range.ns == key.push })
}

fn outbox_drained(settings: Settings) -> Bool {
  !runs_outbox(settings) || push_drain.is_idle(push_adapter.site())
}

pub fn calls_left() -> Int {
  case node.runs(settings.get().role, node.Calls) {
    True -> list.length(call_manager.local_calls())
    False -> 0
  }
}

pub fn push_jobs_left() -> Int {
  case runs_outbox(settings.get()) {
    False -> 0
    True -> push_drain.left(push_adapter.site())
  }
}

pub fn buckets_held() -> Int {
  adapters(settings.get())
  |> list.map(fn(entry) { entry.ns })
  |> held
  |> list.length
}

pub fn drained() -> Bool {
  let settings = settings.get()
  let movable = adapters(settings)
  draining.is_draining()
  && held(list.map(movable, fn(entry) { entry.ns })) == []
  && { !node.runs(settings.role, node.Guilds) || registry.count() == 0 }
  && {
    !node.runs(settings.role, node.Presence)
    || list.all(route.shards(), fn(shard) { pids.shard_entries(shard) == [] })
  }
  && {
    !node.runs(settings.role, node.Calls) || call_manager.local_calls() == []
  }
  && outbox_drained(settings)
}

type Latch {
  FluxerGatewayWireAccepted
}

@external(erlang, "persistent_term", "get")
fn latched(key: Latch, default: Bool) -> Bool

@external(erlang, "persistent_term", "put")
fn latch(key: Latch, value: Bool) -> Nil

pub fn peek_wire_readiness() -> wire.Assessment {
  use <- bool.guard(latched(FluxerGatewayWireAccepted, False), wire.Accepted)
  wire.current()
}

pub fn wire_readiness() -> wire.Assessment {
  use <- bool.guard(latched(FluxerGatewayWireAccepted, False), wire.Accepted)
  let found = wire.current()
  case found {
    wire.Accepted -> latch(FluxerGatewayWireAccepted, True)
    wire.Pending | wire.Mismatch(..) -> Nil
  }
  found
}
