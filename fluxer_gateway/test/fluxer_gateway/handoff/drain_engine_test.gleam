// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type BucketId, BucketId, Key}
import bouncer_proto/message.{type ToBouncer}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/handoff/adapter.{type Adapter, Adapter, Frozen}
import fluxer_gateway/handoff/move
import fluxer_gateway/handoff/ports
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/list
import gleam/option.{None}
import gleam/otp/actor
import gleam/result
import support/handoff_toy
import support/wait

type Status {
  Here
  Busy
  Gone
}

type FleetMsg {
  SetStatus(id: Int, status: Status)
  Lookup(id: Int, reply: Subject(Result(Status, Nil)))
  Freeze(id: Int, reply: Subject(Status))
  Attempts(id: Int, reply: Subject(Int))
  Present(bucket: Int, reply: Subject(List(Int)))
}

type Fleet {
  Fleet(statuses: Dict(Int, Status), attempts: Dict(Int, Int))
}

fn start_fleet(ids: List(Int)) -> Subject(FleetMsg) {
  let statuses = list.map(ids, fn(id) { #(id, Here) }) |> dict.from_list
  let assert Ok(started) =
    actor.new(Fleet(statuses:, attempts: dict.new()))
    |> actor.on_message(fn(fleet: Fleet, msg: FleetMsg) {
      case msg {
        SetStatus(id, status) ->
          actor.continue(
            Fleet(..fleet, statuses: dict.insert(fleet.statuses, id, status)),
          )
        Lookup(id, reply) -> {
          process.send(reply, dict.get(fleet.statuses, id))
          actor.continue(fleet)
        }
        Freeze(id, reply) -> {
          let status = dict.get(fleet.statuses, id) |> result.unwrap(Gone)
          process.send(reply, status)
          let attempts =
            dict.upsert(fleet.attempts, id, fn(found) {
              option.unwrap(found, 0) + 1
            })
          actor.continue(Fleet(..fleet, attempts:))
        }
        Attempts(id, reply) -> {
          process.send(reply, dict.get(fleet.attempts, id) |> result.unwrap(0))
          actor.continue(fleet)
        }
        Present(bucket, reply) -> {
          fleet.statuses
          |> dict.to_list
          |> list.filter_map(fn(entry) {
            case entry.1 != Gone && key.bucket(entry.0) == bucket {
              True -> Ok(entry.0)
              False -> Error(Nil)
            }
          })
          |> list.sort(int.compare)
          |> process.send(reply, _)
          actor.continue(fleet)
        }
      }
    })
    |> actor.start
  started.data
}

fn fleet_adapter(fleet: Subject(FleetMsg)) -> Adapter {
  let missing = adapter.EntityMissing(Key(ns: handoff_toy.ns, id: 0))
  Adapter(
    ns: handoff_toy.ns,
    codecs: handoff_toy.range(),
    entities_in: fn(bucket) { process.call(fleet, 1000, Present(bucket, _)) },
    freeze: fn(entity, _, _, _) {
      case process.call(fleet, 1000, Freeze(entity.id, _)) {
        Here -> {
          let stub = process.spawn_unlinked(fn() { process.sleep_forever() })
          Ok(Frozen(tail: <<entity.id:64>>, stub:))
        }
        Busy -> Error(adapter.EntityBusy(entity))
        Gone -> Error(adapter.EntityMissing(entity))
      }
    },
    install: fn(_, _, _, _) { Error(adapter.InstallFailed("not a target")) },
    release: fn(_, _, _) { Error(missing) },
    seal: fn(_, _) { Error(missing) },
    abort_arrival: fn(_) { Nil },
    committed: fn(stub, target_ref, _) {
      case target_ref {
        <<id:64>> -> process.send(fleet, SetStatus(id, Gone))
        _ -> Nil
      }
      process.kill(stub)
      Ok(Nil)
    },
    thaw: fn(stub, _) {
      process.kill(stub)
      Ok(Nil)
    },
    abandon: fn(_) { Nil },
    precopy: None,
  )
}

type HomeMsg {
  Engine(engine: move.Engine)
  Worker(message: ToBouncer)
}

type Begun =
  Dict(message.MoveId, #(Epoch, Dict(Int, BitArray)))

fn start_home() -> Subject(HomeMsg) {
  let assert Ok(started) =
    actor.new(#(None, dict.new()))
    |> actor.on_message(fn(state, msg: HomeMsg) {
      let #(engine, begun): #(option.Option(move.Engine), Begun) = state
      case msg, engine {
        Engine(found), _ -> actor.continue(#(option.Some(found), begun))
        Worker(found), option.Some(engine) ->
          actor.continue(#(option.Some(engine), home_step(engine, begun, found)))
        Worker(_), None -> actor.continue(state)
      }
    })
    |> actor.start
  started.data
}

fn home_step(engine: move.Engine, begun: Begun, found: ToBouncer) -> Begun {
  case found {
    message.BeginMove(move:, epoch:, ..) -> {
      move.from_home(engine, message.MoveAccepted(move))
      dict.insert(begun, move, #(epoch, dict.new()))
    }
    message.BlobChunk(move:, part:, index:, count:, bytes:) -> {
      move.from_home(engine, message.ChunkAck(move, part, index))
      case dict.get(begun, move), part {
        Ok(#(at, chunks)), blob.Tail -> {
          let chunks = dict.insert(chunks, index, bytes)
          case dict.size(chunks) == count {
            False -> dict.insert(begun, move, #(at, chunks))
            True -> {
              let tail =
                chunks
                |> dict.to_list
                |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
                |> list.map(fn(entry) { entry.1 })
                |> bit_array.concat
              move.from_home(
                engine,
                message.Committed(move, tail, epoch.next(at)),
              )
              dict.delete(begun, move)
            }
          }
        }
        _, _ -> begun
      }
    }
    message.SealSource(move:, last_seq:) -> {
      move.from_home(engine, message.AppliedThrough(move, last_seq))
      begun
    }
    _ -> begun
  }
}

fn at() -> Epoch {
  let assert Ok(found) = epoch.from_int(5)
  found
}

fn target() -> WorkerId {
  WorkerId(atom.create("drain_engine_target@test"), 1)
}

fn production_limits() -> move.Limits {
  move.Limits(
    entities_in_flight: 8,
    buckets_in_flight: 2,
    retry_budget_ms: 20_000,
  )
}

fn buckets_of(ids: List(Int)) -> List(BucketId) {
  ids
  |> list.map(key.bucket)
  |> list.unique
  |> list.map(fn(bucket) { BucketId(ns: handoff_toy.ns, bucket:) })
}

fn start_engine(
  fleet: Subject(FleetMsg),
  held: List(BucketId),
  refused: List(BucketId),
) -> move.Engine {
  let home = start_home()
  let source_ports =
    ports.Ports(
      ..ports.unwired(),
      me: WorkerId(atom.create("drain_engine_source@test"), 1),
      to_home: fn(_, found) { Ok(process.send(home, Worker(found))) },
      holding: fn(_) { Ok(at()) },
      held: fn() { held },
      place: fn(_) { Ok(target()) },
      target_codecs: fn(_) { [handoff_toy.range()] },
      close_bucket: fn(bucket, held_at, _, _) {
        case list.contains(refused, bucket) {
          True -> Error(message.RetryAfter(200))
          False -> Ok(epoch.next(held_at))
        }
      },
    )
  let engine = claim_engine(fleet, source_ports)
  process.send(home, Engine(engine))
  engine
}

fn claim_engine(
  fleet: Subject(FleetMsg),
  source_ports: ports.Ports,
) -> move.Engine {
  case
    move.start_with([fleet_adapter(fleet)], production_limits(), source_ports)
  {
    Ok(engine) -> engine
    Error(_) -> {
      process.sleep(5)
      claim_engine(fleet, source_ports)
    }
  }
}

fn stop_engine(engine: move.Engine) -> Nil {
  case process.subject_owner(engine.inbox) {
    Ok(pid) -> {
      process.unlink(pid)
      process.kill(pid)
    }
    Error(Nil) -> Nil
  }
}

fn status(fleet: Subject(FleetMsg), id: Int) -> Result(Status, Nil) {
  process.call(fleet, 1000, Lookup(id, _))
}

fn all_gone(fleet: Subject(FleetMsg), ids: List(Int)) -> Bool {
  list.all(ids, fn(id) { status(fleet, id) == Ok(Gone) })
}

fn attempts(fleet: Subject(FleetMsg), id: Int) -> Int {
  process.call(fleet, 1000, Attempts(id, _))
}

fn spawn_drain(
  engine: move.Engine,
  within_ms: Int,
) -> Subject(Result(List(move.BucketOutcome), move.MoveError)) {
  let reply = process.new_subject()
  process.spawn_unlinked(fn() {
    process.send(reply, move.drain(engine, within_ms))
  })
  reply
}

fn ids_after_the_first_bucket(ids: List(Int)) -> #(Int, List(Int)) {
  let assert [first, ..] = ids
  let bucket = key.bucket(first)
  #(first, list.filter(ids, fn(id) { key.bucket(id) != bucket }))
}

pub fn a_busy_entity_does_not_hold_back_the_rest_of_the_drain_test() {
  let ids = int.range(from: 1001, to: 1121, with: [], run: list.prepend)
  let ids = list.reverse(ids)
  let #(busy, others) = ids_after_the_first_bucket(ids)
  let fleet = start_fleet(ids)
  process.send(fleet, SetStatus(busy, Busy))
  let engine = start_engine(fleet, buckets_of(ids), [])
  let started = now_ms()
  let _ = spawn_drain(engine, 30_000)
  assert wait.until(3000, fn() { all_gone(fleet, others) })
  assert now_ms() - started < 3000
  assert status(fleet, busy) == Ok(Busy)
  process.send(fleet, SetStatus(busy, Here))
  assert wait.until(10_000, fn() { status(fleet, busy) == Ok(Gone) })
  stop_engine(engine)
}

pub fn a_drain_stops_retrying_at_its_deadline_test() {
  let busy = 2001
  let fleet = start_fleet([busy])
  process.send(fleet, SetStatus(busy, Busy))
  let engine = start_engine(fleet, buckets_of([busy]), [])
  let started = now_ms()
  let _ = spawn_drain(engine, 1000)
  process.sleep(int.max(0, started + 2500 - now_ms()))
  let at_deadline = attempts(fleet, busy)
  process.sleep(int.max(0, started + 4600 - now_ms()))
  assert at_deadline >= 1
  assert attempts(fleet, busy) == at_deadline
  stop_engine(engine)
}

pub fn a_refused_bucket_does_not_stop_the_other_buckets_test() {
  let ids = int.range(from: 3001, to: 3061, with: [], run: list.prepend)
  let ids = list.reverse(ids)
  let #(stuck, _) = ids_after_the_first_bucket(ids)
  let held = buckets_of(ids)
  let assert [refused, ..] = held
  let others = list.filter(ids, fn(id) { key.bucket(id) != refused.bucket })
  let fleet = start_fleet(ids)
  let engine = start_engine(fleet, held, [refused])
  let reply = spawn_drain(engine, 10_000)
  assert wait.until(3000, fn() { all_gone(fleet, others) })
  assert status(fleet, stuck) == Ok(Here)
  let assert Ok(Error(move.Refused(message.RetryAfter(_)))) =
    process.receive(reply, 5000)
  stop_engine(engine)
}

@external(erlang, "erlang", "monotonic_time")
fn monotonic(unit: atom.Atom) -> Int

fn now_ms() -> Int {
  monotonic(atom.create("millisecond"))
}
