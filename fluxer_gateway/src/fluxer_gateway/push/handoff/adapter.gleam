// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId}
import bouncer_proto/worker.{type WorkerId}
import fluxer_gateway/core/clock
import fluxer_gateway/core/node
import fluxer_gateway/handoff/adapter.{
  type Adapter, type AdapterError, type Arrival, type Frozen, Adapter, Arrival,
  Frozen,
}
import fluxer_gateway/handoff/codec as handoff_codec
import fluxer_gateway/handoff/move as handoff_move
import fluxer_gateway/push/handoff/codec.{type Tail, Tail}
import fluxer_gateway/push/outbox
import fluxer_gateway/push/outbox/state
import fluxer_gateway/push/server
import fluxer_gateway/push/throttle
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/erlang/process.{type Name, type Pid, type Subject}
import gleam/list
import gleam/option.{None}
import gleam/result

const reply_timeout_ms = 5000

const late_reply_margin_ms = 1000

pub type Site {
  Site(outbox: Name(outbox.Message), server: Name(server.Message))
}

pub fn site() -> Site {
  Site(outbox: outbox.name(), server: server.name())
}

type Signal {
  Commit(to: state.Target, reply: Subject(Bool))
  Thaw(reply: Subject(Bool))
  Release
  Seal(reply: Subject(Bool))
  Abort
}

type Entities {
  FluxerGatewayPushEntities
}

@external(erlang, "persistent_term", "get")
fn stored(key: Entities, default: Dict(Int, Int)) -> Dict(Int, Int)

@external(erlang, "persistent_term", "put")
fn store(key: Entities, value: Dict(Int, Int)) -> Nil

pub fn adapter() -> Adapter {
  adapter_at(site())
}

pub fn adapter_at(at: Site) -> Adapter {
  Adapter(
    ns: key.push,
    codecs: codec.range(),
    entities_in: entities_in(at, _),
    freeze: fn(entity, move, version, epoch) {
      freeze(at, entity, move, version, epoch)
    },
    install: fn(entity, source, epoch, tail) {
      install(at, entity, source, epoch, tail)
    },
    release:,
    seal:,
    abort_arrival:,
    committed:,
    thaw:,
    abandon:,
    precopy: None,
  )
}

pub fn entity_of(bucket: Int) -> Result(Int, Nil) {
  let found = stored(FluxerGatewayPushEntities, dict.new())
  let table = case dict.size(found) == key.bucket_count {
    True -> found
    False -> {
      let built = canonical(0, dict.new())
      store(FluxerGatewayPushEntities, built)
      built
    }
  }
  dict.get(table, bucket)
}

fn canonical(next: Int, acc: Dict(Int, Int)) -> Dict(Int, Int) {
  case dict.size(acc) == key.bucket_count {
    True -> acc
    False -> {
      let bucket = key.bucket(next)
      case dict.has_key(acc, bucket) {
        True -> canonical(next + 1, acc)
        False -> canonical(next + 1, dict.insert(acc, bucket, next))
      }
    }
  }
}

pub fn entities_in(at: Site, bucket: Int) -> List(Int) {
  let busy_in = fn(found: Result(List(Int), Nil)) {
    case found {
      Ok(buckets) -> list.contains(buckets, bucket)
      Error(Nil) -> True
    }
  }
  let busy =
    busy_in(server.busy_buckets(at.server))
    || busy_in(outbox.busy_buckets(at.outbox))
  case busy, entity_of(bucket) {
    True, Ok(entity) -> [entity]
    _, _ -> []
  }
}

fn bucket_of(entity: Key) -> Result(Int, AdapterError) {
  let bucket = key.bucket(entity.id)
  case entity_of(bucket) == Ok(entity.id) {
    True -> Ok(bucket)
    False -> Error(adapter.EntityMissing(entity))
  }
}

fn signals(pid: Pid) -> Subject(Signal) {
  process.unsafely_create_subject(
    pid,
    atom.to_dynamic(atom.create("fluxer_gateway_push_handoff")),
  )
}

fn ask(pid: Pid, make: fn(Subject(a)) -> Signal) -> Result(a, Nil) {
  let reply = process.new_subject()
  let monitor = process.monitor(pid)
  process.send(signals(pid), make(reply))
  let answer =
    process.new_selector()
    |> process.select_map(reply, Ok)
    |> process.select_specific_monitor(monitor, fn(_) { Error(Nil) })
    |> process.selector_receive(reply_timeout_ms)
  process.demonitor_process(monitor)
  case answer {
    Ok(found) -> found
    Error(Nil) -> Error(Nil)
  }
}

pub fn freeze(
  at_site: Site,
  entity: Key,
  move: MoveId,
  version: Int,
  at: Epoch,
) -> Result(Frozen, AdapterError) {
  use bucket <- result.try(bucket_of(entity))
  let ready = process.new_subject()
  let before_ms = clock.monotonic_ms() + reply_timeout_ms
  let stub =
    process.spawn_unlinked(fn() {
      case outbox.export_bucket(at_site.outbox, bucket) {
        Error(Nil) -> process.send(ready, Error(adapter.EntityBusy(entity)))
        Ok(#(carried, marks)) ->
          case server.export_pending(at_site.server, bucket) {
            Error(Nil) -> {
              outbox.reclaim_bucket(at_site.outbox, bucket, carried)
              process.send(ready, Error(adapter.EntityBusy(entity)))
            }
            Ok(waiting) ->
              stub_start(
                at_site,
                entity,
                move,
                version,
                at,
                before_ms,
                ready,
                Tail(
                  bucket:,
                  waiting:,
                  carried:,
                  marks:,
                  throttle: throttle.snapshot(),
                ),
              )
          }
      }
    })
  case process.receive(ready, reply_timeout_ms + late_reply_margin_ms) {
    Ok(Ok(bytes)) -> Ok(Frozen(tail: bytes, stub:))
    Ok(Error(error)) -> Error(error)
    Error(Nil) -> {
      process.kill(stub)
      Error(adapter.EntityBusy(entity))
    }
  }
}

fn stub_start(
  at_site: Site,
  entity: Key,
  move: MoveId,
  version: Int,
  at: Epoch,
  before_ms: Int,
  ready: Subject(Result(BitArray, AdapterError)),
  tail: Tail,
) -> Nil {
  case
    handoff_codec.encode(codec.tail(), tail, version, epoch.to_int(at)),
    clock.monotonic_ms() > before_ms
  {
    Error(_), _ -> {
      give_back(at_site, tail)
      process.send(ready, Error(adapter.EncodeFailed("push tail")))
    }
    Ok(_), True -> {
      give_back(at_site, tail)
      process.send(ready, Error(adapter.EntityBusy(entity)))
    }
    Ok(bytes), False -> {
      process.send(ready, Ok(bytes))
      stub_loop(at_site, move, tail)
    }
  }
}

fn give_back(at: Site, tail: Tail) -> Nil {
  outbox.reclaim_bucket(at.outbox, tail.bucket, tail.carried)
  server.requeue_local(at.server, tail.waiting)
}

fn stub_loop(at: Site, move: MoveId, tail: Tail) -> Nil {
  case process.receive_forever(signals(process.self())) {
    Commit(to:, reply:) -> {
      outbox.hand_bucket(at.outbox, tail.bucket, to)
      process.send(reply, True)
      handoff_move.repointed(move)
    }
    Thaw(reply:) -> {
      give_back(at, tail)
      process.send(reply, True)
    }
    Abort -> give_back(at, tail)
    Seal(reply:) -> {
      process.send(reply, False)
      stub_loop(at, move, tail)
    }
    Release -> stub_loop(at, move, tail)
  }
}

pub fn install(
  at_site: Site,
  entity: Key,
  source: WorkerId,
  at: Epoch,
  tail: BitArray,
) -> Result(Arrival, AdapterError) {
  let _ = source
  let _ = at
  use bucket <- result.try(bucket_of(entity))
  use #(_, decoded) <- result.try(
    handoff_codec.decode(codec.tail(), tail)
    |> result.replace_error(adapter.DecodeFailed("push tail")),
  )
  use Nil <- result.map(case decoded.bucket == bucket {
    True -> Ok(Nil)
    False -> Error(adapter.DecodeFailed("push tail names another bucket"))
  })
  let holder =
    process.spawn_unlinked(fn() { arrival_loop(at_site, decoded, False) })
  Arrival(
    entity: holder,
    target_ref: codec.encode_target(outbox.target_at(
      node.self(),
      at_site.outbox,
    )),
  )
}

fn arrival_loop(at: Site, tail: Tail, imported: Bool) -> Nil {
  case process.receive_forever(signals(process.self())) {
    Release -> arrival_loop(at, tail, take_over(at, tail, imported))
    Seal(reply:) -> {
      let _ = take_over(at, tail, imported)
      process.send(reply, True)
    }
    Abort if !imported -> Nil
    Abort -> arrival_loop(at, tail, imported)
    Commit(reply:, ..) | Thaw(reply:) -> {
      process.send(reply, False)
      arrival_loop(at, tail, imported)
    }
  }
}

fn take_over(at: Site, tail: Tail, imported: Bool) -> Bool {
  case imported {
    True -> True
    False -> {
      list.each(tail.throttle, fn(found) {
        throttle.absorb(found.0, found.1, found.2)
      })
      outbox.arrive_bucket(at.outbox, tail.bucket, tail.carried, tail.marks)
      server.requeue_local(at.server, tail.waiting)
      True
    }
  }
}

pub fn release(
  entity: Pid,
  seq: Int,
  bytes: BitArray,
) -> Result(Nil, AdapterError) {
  let _ = seq
  let _ = bytes
  process.send(signals(entity), Release)
  Ok(Nil)
}

pub fn seal(entity: Pid, last_seq: Int) -> Result(Nil, AdapterError) {
  let _ = last_seq
  case ask(entity, Seal) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) -> Error(adapter.InstallFailed("push did not seal"))
  }
}

pub fn abort_arrival(entity: Pid) -> Nil {
  process.send(signals(entity), Abort)
}

pub fn committed(
  stub: Pid,
  target_ref: BitArray,
  at: Epoch,
) -> Result(Nil, AdapterError) {
  let _ = at
  use to <- result.try(
    codec.decode_target(target_ref)
    |> result.replace_error(adapter.DecodeFailed("push target")),
  )
  case ask(stub, Commit(to, _)) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("push stub was not frozen"))
  }
}

pub fn thaw(
  stub: Pid,
  returned: List(#(Int, BitArray)),
) -> Result(Nil, AdapterError) {
  let _ = returned
  case ask(stub, Thaw) {
    Ok(True) -> Ok(Nil)
    Ok(False) | Error(Nil) ->
      Error(adapter.InstallFailed("push stub was not frozen"))
  }
}

pub fn abandon(entity: Key) -> Nil {
  let _ = entity
  Nil
}
