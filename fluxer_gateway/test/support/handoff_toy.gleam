// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId, type ToBouncer, type ToMover}
import fluxer_gateway/handoff/adapter.{type Adapter, Adapter, Arrival, Frozen}
import fluxer_gateway/handoff/move
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result

pub const ns = 9

pub fn range() -> CodecRange {
  CodecRange(ns:, oldest_read: 1, max_read: 1, max_write: 1)
}

pub type EntityMsg {
  Op(n: Int)
  Freeze(move: MoveId, reply: Subject(BitArray))
  Committed(target: Subject(EntityMsg))
  Repoint(peer_ack: Subject(Nil))
  Thaw(entries: List(#(Int, BitArray)))
  Release(bytes: BitArray)
  Seal(reply: Subject(Nil))
  Ops(reply: Subject(List(Int)))
}

pub type PeerMsg {
  PeerMoved(target: Subject(EntityMsg), ack: Subject(Nil))
}

type Mode {
  Serving
  Stub(move: MoveId, peer: Subject(PeerMsg), held: List(Int))
  Direct(target: Subject(EntityMsg))
  ArrivingMode(stash: List(Int))
}

type Entity {
  Entity(self: Subject(EntityMsg), ops: List(Int), mode: Mode)
}

pub type Directory =
  Subject(DirMsg)

pub type DirMsg {
  Put(pid: Pid, subject: Subject(EntityMsg))
  Get(pid: Pid, reply: Subject(Result(Subject(EntityMsg), Nil)))
}

pub fn directory() -> Directory {
  let assert Ok(started) =
    actor.new(dict.new())
    |> actor.on_message(fn(state: Dict(Pid, Subject(EntityMsg)), msg) {
      case msg {
        Put(pid, subject) -> actor.continue(dict.insert(state, pid, subject))
        Get(pid, reply) -> {
          process.send(reply, dict.get(state, pid))
          actor.continue(state)
        }
      }
    })
    |> actor.start
  started.data
}

fn lookup(dir: Directory, pid: Pid) -> Result(Subject(EntityMsg), Nil) {
  process.call(dir, 1000, Get(pid, _))
}

pub fn spawn_entity(
  dir: Directory,
  ops: List(Int),
  arriving: Bool,
  peer: Subject(PeerMsg),
) -> Subject(EntityMsg) {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      let self = process.new_subject()
      process.send(ready, self)
      let mode = case arriving {
        True -> ArrivingMode([])
        False -> Serving
      }
      entity_loop(Entity(self:, ops: list.reverse(ops), mode:), peer)
    })
  let assert Ok(subject) = process.receive(ready, 1000)
  process.send(dir, Put(pid, subject))
  subject
}

fn entity_loop(entity: Entity, peer: Subject(PeerMsg)) -> Nil {
  let msg = process.receive_forever(entity.self)
  case msg, entity.mode {
    Op(n), Serving ->
      entity_loop(Entity(..entity, ops: [n, ..entity.ops]), peer)
    Op(n), Stub(move, to_peer, held) -> {
      move.forward(move, <<n:32>>)
      entity_loop(
        Entity(..entity, mode: Stub(move, to_peer, [n, ..held])),
        peer,
      )
    }
    Op(n), Direct(target) -> {
      process.send(target, Op(n))
      entity_loop(entity, peer)
    }
    Op(n), ArrivingMode(stash) ->
      entity_loop(Entity(..entity, mode: ArrivingMode([n, ..stash])), peer)
    Freeze(move, reply), Serving -> {
      process.send(reply, encode(list.reverse(entity.ops)))
      entity_loop(Entity(..entity, mode: Stub(move, peer, [])), peer)
    }
    Committed(target), Stub(_, to_peer, _) -> {
      let ack = process.new_subject()
      process.send(to_peer, PeerMoved(target, ack))
      let _ = process.receive(ack, 2000)
      process.send(entity.self, Repoint(ack))
      entity_loop(entity, peer)
    }
    Repoint(_), Stub(move, _, _) -> {
      move.repointed(move)
      entity_loop(entity, peer)
    }
    Thaw(_), Stub(held:, ..) ->
      entity_loop(
        Entity(..entity, mode: Serving, ops: list.append(held, entity.ops)),
        peer,
      )
    Release(bytes), ArrivingMode(_) ->
      entity_loop(Entity(..entity, ops: [decode_op(bytes), ..entity.ops]), peer)
    Seal(reply), ArrivingMode(stash) -> {
      process.send(reply, Nil)
      entity_loop(
        Entity(..entity, mode: Serving, ops: list.append(stash, entity.ops)),
        peer,
      )
    }
    Ops(reply), _ -> {
      process.send(reply, list.reverse(entity.ops))
      entity_loop(entity, peer)
    }
    _, _ -> entity_loop(entity, peer)
  }
}

pub fn ops(entity: Subject(EntityMsg)) -> List(Int) {
  process.call(entity, 1000, Ops)
}

@external(erlang, "erlang", "term_to_binary")
fn to_bytes(value: a) -> BitArray

@external(erlang, "erlang", "binary_to_term")
fn from_bytes(bytes: BitArray) -> a

pub fn adapter(
  dir: Directory,
  source: Subject(EntityMsg),
  installed: Subject(Subject(EntityMsg)),
  peer: Subject(PeerMsg),
) -> Adapter {
  Adapter(
    ns:,
    codecs: range(),
    entities_in: fn(_) { [1] },
    freeze: fn(_key, move, _version, _epoch) {
      let reply = process.new_subject()
      process.send(source, Freeze(move, reply))
      case process.receive(reply, 1000) {
        Ok(tail) ->
          case process.subject_owner(source) {
            Ok(pid) -> Ok(Frozen(tail:, stub: pid))
            Error(Nil) -> Error(adapter.EntityMissing(key.Key(ns:, id: 1)))
          }
        Error(Nil) -> Error(adapter.EntityBusy(key.Key(ns:, id: 1)))
      }
    },
    install: fn(_key, _source, _epoch, tail) {
      let subject = spawn_entity(dir, decode(tail), True, peer)
      process.send(installed, subject)
      case process.subject_owner(subject) {
        Ok(pid) -> Ok(Arrival(entity: pid, target_ref: to_bytes(subject)))
        Error(Nil) -> Error(adapter.InstallFailed("no owner"))
      }
    },
    release: fn(pid, _seq, bytes) {
      lookup(dir, pid)
      |> result.map(fn(subject) { process.send(subject, Release(bytes)) })
      |> result.replace_error(adapter.EntityMissing(key.Key(ns:, id: 1)))
    },
    seal: fn(pid, _last) {
      lookup(dir, pid)
      |> result.map(fn(subject) {
        let _ = process.call(subject, 1000, Seal)
        Nil
      })
      |> result.replace_error(adapter.EntityMissing(key.Key(ns:, id: 1)))
    },
    abort_arrival: fn(pid) { process.kill(pid) },
    committed: fn(pid, target_ref, _epoch) {
      lookup(dir, pid)
      |> result.map(fn(subject) {
        process.send(subject, Committed(from_bytes(target_ref)))
      })
      |> result.replace_error(adapter.EntityMissing(key.Key(ns:, id: 1)))
    },
    thaw: fn(pid, entries) {
      lookup(dir, pid)
      |> result.map(fn(subject) { process.send(subject, Thaw(entries)) })
      |> result.replace_error(adapter.EntityMissing(key.Key(ns:, id: 1)))
    },
    abandon: fn(_) { Nil },
    precopy: None,
  )
}

type Countdown {
  Take(reply: Subject(Bool))
}

pub fn refusing(toy: Adapter, refusals: Int) -> Adapter {
  let assert Ok(started) =
    actor.new(refusals)
    |> actor.on_message(fn(left, msg: Countdown) {
      let Take(reply) = msg
      process.send(reply, left > 0)
      actor.continue(left - 1)
    })
    |> actor.start
  let countdown = started.data
  Adapter(..toy, install: fn(entity, source, at, tail) {
    case process.call(countdown, 1000, Take) {
      True -> Error(adapter.InstallFailed("refused by the test"))
      False -> toy.install(entity, source, at, tail)
    }
  })
}

fn encode(ops: List(Int)) -> BitArray {
  list.fold(ops, <<list.length(ops):32>>, fn(acc, op) {
    bit_array.append(acc, <<op:32>>)
  })
}

fn decode(bytes: BitArray) -> List(Int) {
  case bytes {
    <<_count:32, rest:bits>> -> decode_loop(rest, [])
    _ -> []
  }
}

fn decode_loop(bytes: BitArray, acc: List(Int)) -> List(Int) {
  case bytes {
    <<op:32, rest:bits>> -> decode_loop(rest, [op, ..acc])
    _ -> list.reverse(acc)
  }
}

fn decode_op(bytes: BitArray) -> Int {
  case bytes {
    <<op:32>> -> op
    _ -> -1
  }
}

pub fn start_peer(
  total: Int,
  done: Subject(Int),
) -> #(Subject(PeerMsg), Subject(Subject(EntityMsg))) {
  let ready = process.new_subject()
  process.spawn(fn() {
    let inbox = process.new_subject()
    let go = process.new_subject()
    process.send(ready, #(inbox, go))
    let first = process.receive_forever(go)
    peer_loop(first, inbox, 1, total, done)
  })
  let assert Ok(pair) = process.receive(ready, 1000)
  pair
}

fn peer_loop(
  target: Subject(EntityMsg),
  inbox: Subject(PeerMsg),
  next: Int,
  total: Int,
  done: Subject(Int),
) -> Nil {
  case next > total {
    True -> process.send(done, total)
    False -> {
      let target = case process.receive(inbox, 0) {
        Ok(PeerMoved(new_target, ack)) -> {
          process.send(ack, Nil)
          new_target
        }
        Error(Nil) -> target
      }
      process.send(target, Op(next))
      process.sleep(1)
      peer_loop(target, inbox, next + 1, total, done)
    }
  }
}

pub type Home {
  Home(
    move: Option(MoveId),
    entity: Option(Key),
    to_epoch: Epoch,
    tail: Dict(Int, BitArray),
    tail_count: Int,
    forwards: List(#(Int, BitArray)),
    committed: Bool,
    released: Int,
    received: Int,
    sealed: Option(Int),
  )
}

pub type HomeMsg {
  FromWorker(message: ToBouncer)
  Routes(to_source: fn(ToMover) -> Nil, to_target: fn(ToMover) -> Nil)
}

pub fn home() -> Subject(HomeMsg) {
  let initial =
    Home(
      move: None,
      entity: None,
      to_epoch: epoch.zero(),
      tail: dict.new(),
      tail_count: -1,
      forwards: [],
      committed: False,
      released: 0,
      received: 0,
      sealed: None,
    )
  let unrouted = fn(_) { Nil }
  let assert Ok(started) =
    actor.new(#(initial, unrouted, unrouted))
    |> actor.on_message(fn(state, msg: HomeMsg) {
      let #(home, to_source, to_target) = state
      case msg {
        Routes(to_source, to_target) ->
          actor.continue(#(home, to_source, to_target))
        FromWorker(message) ->
          actor.continue(#(
            home_step(home, message, to_source, to_target),
            to_source,
            to_target,
          ))
      }
    })
    |> actor.start
  started.data
}

pub fn homes() -> Subject(HomeMsg) {
  let unrouted = fn(_) { Nil }
  let assert Ok(started) =
    actor.new(#(dict.new(), unrouted, unrouted))
    |> actor.on_message(fn(state, msg: HomeMsg) {
      let #(homes, to_source, to_target) = state
      case msg {
        Routes(to_source, to_target) ->
          actor.continue(#(homes, to_source, to_target))
        FromWorker(message) ->
          case move_of(message) {
            Error(Nil) -> actor.continue(state)
            Ok(move) -> {
              let home = dict.get(homes, move) |> result.unwrap(blank_home())
              let home = home_step(home, message, to_source, to_target)
              actor.continue(#(
                dict.insert(homes, move, home),
                to_source,
                to_target,
              ))
            }
          }
      }
    })
    |> actor.start
  started.data
}

fn blank_home() -> Home {
  Home(
    move: None,
    entity: None,
    to_epoch: epoch.zero(),
    tail: dict.new(),
    tail_count: -1,
    forwards: [],
    committed: False,
    released: 0,
    received: 0,
    sealed: None,
  )
}

fn move_of(found: ToBouncer) -> Result(MoveId, Nil) {
  case found {
    message.BeginMove(move:, ..)
    | message.InstallRefused(move:, ..)
    | message.BlobChunk(move:, ..)
    | message.Forward(move:, ..)
    | message.Installed(move:, ..)
    | message.SealSource(move:, ..)
    | message.Applied(move:, ..) -> Ok(move)
    _ -> Error(Nil)
  }
}

fn home_step(
  state: Home,
  msg: ToBouncer,
  to_source: fn(ToMover) -> Nil,
  to_target: fn(ToMover) -> Nil,
) -> Home {
  case msg {
    message.BeginMove(move, entity, at, _, _, _) -> {
      to_source(message.MoveAccepted(move))
      Home(
        move: Some(move),
        entity: Some(entity),
        to_epoch: epoch.next(at),
        tail: dict.new(),
        tail_count: -1,
        forwards: [],
        committed: False,
        released: 0,
        received: 0,
        sealed: None,
      )
    }
    message.InstallRefused(move, _) -> {
      to_source(message.Aborted(move))
      to_target(message.Abort(move))
      state
    }
    message.BlobChunk(move, part, index, count, bytes) -> {
      to_source(message.ChunkAck(move, part, index))
      let state =
        Home(
          ..state,
          tail: dict.insert(state.tail, index, bytes),
          tail_count: count,
        )
      case dict.size(state.tail) == count, state.entity {
        True, Some(entity) -> {
          list.each(dict.to_list(state.tail), fn(entry) {
            to_target(message.Install(
              move,
              entity,
              state.to_epoch,
              1,
              blob.Tail,
              entry.0,
              count,
              entry.1,
            ))
          })
          state
        }
        _, _ -> state
      }
    }
    message.Forward(move, seq, bytes) ->
      case seq == state.received + 1 {
        False -> state
        True ->
          Home(
            ..state,
            received: seq,
            forwards: list.append(state.forwards, [#(seq, bytes)]),
          )
          |> release(move, to_target)
      }
    message.Installed(move, target_ref) -> {
      to_source(message.Committed(move, target_ref, state.to_epoch))
      release(Home(..state, committed: True), move, to_target)
    }
    message.SealSource(move, last) ->
      release(Home(..state, sealed: Some(last)), move, to_target)
    message.Applied(move, last) -> {
      case state.sealed == Some(last) {
        True -> to_source(message.AppliedThrough(move, last))
        False -> Nil
      }
      state
    }
    _ -> state
  }
}

fn release(state: Home, move: MoveId, to_target: fn(ToMover) -> Nil) -> Home {
  case state.committed {
    False -> state
    True -> {
      list.each(state.forwards, fn(entry) {
        to_target(message.Release(move, entry.0, entry.1))
      })
      let state = Home(..state, forwards: [], released: state.received)
      case state.sealed {
        Some(last) if state.released >= last -> {
          to_target(message.Seal(move, last))
          Home(..state, sealed: Some(last))
        }
        _ -> state
      }
    }
  }
}
