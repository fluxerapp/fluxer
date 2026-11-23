// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{type Part}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key, Key}
import bouncer_proto/message.{type MoveId, type Refusal, type ToBouncer, MoveId}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/handoff/arrival.{type Arrival}
import fluxer_gateway/handoff/source.{type Source}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/erlang/atom
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub type Point {
  OnBegin
  OnTailChunk
  AfterTailComplete
  OnInstalled
  AfterCommitWrite
  OnSeal
  OnApplied
  AtMs(ms: Int)
}

pub type Fault {
  BouncerCrash(ordinal: Int, at: Point)
  SourceCrash(at: Point)
  TargetCrash(at: Point)
  Refuse(reason: Refusal)
  InstallFails
  InstallDelay(ms: Int)
}

pub type Config {
  Config(
    ops: Int,
    op_every_ms: Int,
    tail_padding: Int,
    faults: List(Fault),
    until_ms: Int,
    commit_quorum_one: Bool,
    precopy: Option(Result(Int, Int)),
  )
}

pub fn config() -> Config {
  Config(
    ops: 40,
    op_every_ms: 1,
    tail_padding: 0,
    faults: [],
    until_ms: 20_000,
    commit_quorum_one: False,
    precopy: None,
  )
}

pub type SourceMode {
  Serving
  Forwarding
  Direct
  SourceDead
}

pub type TargetMode {
  Absent
  Arriving
  Open
  Discarded
  TargetDead
}

type Delivery {
  SourceEvent(event: source.Event)
  ArrivalEvent(event: arrival.Event)
  AtBouncer(ordinal: Int, message: ToBouncer)
  BouncerTimer(ordinal: Int)
  PeerTick
  ToSourceEntity(op: Int)
  ToTargetEntity(op: Int)
  PeerMoved
  StubAck
  Trip(fault: Fault)
}

type HomeMove {
  HomeMove(
    key: Key,
    to_epoch: Epoch,
    version: Int,
    chunks: Dict(#(Int, Int), BitArray),
    counts: Dict(Int, Int),
    tail_sent: Bool,
    forwards: Dict(Int, BitArray),
    received: Int,
    released: Int,
    committed: Bool,
    target_ref: BitArray,
    sealed: Option(Int),
    seal_sent: Bool,
    resend_pending: Bool,
    source_down: Bool,
    applied: Int,
  )
}

type Bouncer {
  Bouncer(alive: Bool, move: Option(HomeMove))
}

type Store {
  Store(commit: Option(BitArray), holders: List(Int), aborted: Bool)
}

pub opaque type Sim {
  Sim(
    config: Config,
    now: Int,
    serial: Int,
    queue: List(#(Int, Int, Delivery)),
    faults: List(Fault),
    source: Source,
    source_mode: SourceMode,
    source_ops: List(Int),
    arrival: Option(Arrival),
    target_mode: TargetMode,
    target_ops: List(Int),
    stash: List(Int),
    bouncers: Dict(Int, Bouncer),
    store: Store,
    peer_to_target: Bool,
    peer_next: Int,
    lost: List(Int),
    outcome: Option(source.Outcome),
    arrival_outcome: Option(arrival.Outcome),
    lease_lapse_scheduled: Bool,
  )
}

pub fn source_worker() -> WorkerId {
  WorkerId(atom.create("sim_source@host"), 1)
}

pub fn target_worker() -> WorkerId {
  WorkerId(atom.create("sim_target@host"), 1)
}

pub fn move_id() -> MoveId {
  MoveId(source: source_worker(), serial: 1)
}

pub fn entity() -> Key {
  Key(ns: key.guild, id: 42)
}

fn epoch_of(value: Int) -> Epoch {
  case epoch.from_int(value) {
    Ok(stamp) -> stamp
    Error(Nil) -> epoch.zero()
  }
}

pub fn run(config: Config) -> Sim {
  let source =
    source.new(
      move: move_id(),
      key: entity(),
      epoch: epoch_of(7),
      to: target_worker(),
      version: 1,
      estimate: config.tail_padding + 1024,
      timings: source.default_timings(),
      precopy: option.is_some(config.precopy),
    )
  let sim =
    Sim(
      config:,
      now: 0,
      serial: 0,
      queue: [],
      faults: config.faults,
      source:,
      source_mode: Serving,
      source_ops: [],
      arrival: None,
      target_mode: Absent,
      target_ops: [],
      stash: [],
      bouncers: dict.from_list([
        #(0, Bouncer(True, None)),
        #(1, Bouncer(True, None)),
        #(2, Bouncer(True, None)),
      ]),
      store: Store(commit: None, holders: [], aborted: False),
      peer_to_target: False,
      peer_next: 1,
      lost: [],
      outcome: None,
      arrival_outcome: None,
      lease_lapse_scheduled: False,
    )
  let sim =
    list.fold(config.faults, sim, fn(acc, fault) {
      case fault_point(fault) {
        Some(AtMs(ms)) -> schedule(acc, ms, Trip(fault))
        _ -> acc
      }
    })
  sim
  |> schedule(0, PeerTick)
  |> schedule(5, SourceEvent(source.Begin))
  |> loop
}

pub fn outcome(sim: Sim) -> Option(source.Outcome) {
  sim.outcome
}

pub fn arrival_outcome(sim: Sim) -> Option(arrival.Outcome) {
  sim.arrival_outcome
}

pub fn source_mode(sim: Sim) -> SourceMode {
  sim.source_mode
}

pub fn target_mode(sim: Sim) -> TargetMode {
  sim.target_mode
}

pub fn lost(sim: Sim) -> List(Int) {
  list.reverse(sim.lost)
}

pub fn committed(sim: Sim) -> Bool {
  option.is_some(sim.store.commit)
}

pub fn aborted(sim: Sim) -> Bool {
  sim.store.aborted
}

pub fn serving_owners(sim: Sim) -> Int {
  let source = case sim.source_mode {
    Serving -> 1
    _ -> 0
  }
  let target = case sim.target_mode {
    Open -> 1
    _ -> 0
  }
  source + target
}

pub fn final_ops(sim: Sim) -> List(Int) {
  case sim.target_mode, sim.source_mode {
    Open, _ -> list.reverse(sim.target_ops)
    _, Serving -> list.reverse(sim.source_ops)
    _, _ -> []
  }
}

pub fn all_ops(sim: Sim) -> List(Int) {
  indices(sim.config.ops) |> list.map(fn(index) { index + 1 })
}

fn loop(sim: Sim) -> Sim {
  case sim.queue {
    [] -> sim
    [#(at, _, _), ..] if at > sim.config.until_ms -> sim
    [#(at, _, delivery), ..rest] ->
      deliver(Sim(..sim, now: at, queue: rest), delivery)
      |> loop
  }
}

fn schedule(sim: Sim, at: Int, delivery: Delivery) -> Sim {
  let entry = #(at, sim.serial, delivery)
  Sim(..sim, serial: sim.serial + 1, queue: insert(sim.queue, entry))
}

fn insert(
  queue: List(#(Int, Int, Delivery)),
  entry: #(Int, Int, Delivery),
) -> List(#(Int, Int, Delivery)) {
  case queue {
    [] -> [entry]
    [head, ..rest] ->
      case head.0 > entry.0 || { head.0 == entry.0 && head.1 > entry.1 } {
        True -> [entry, head, ..rest]
        False -> [head, ..insert(rest, entry)]
      }
  }
}

fn soon(sim: Sim, delivery: Delivery) -> Sim {
  schedule(sim, sim.now + 1, delivery)
}

fn fault_point(fault: Fault) -> Option(Point) {
  case fault {
    BouncerCrash(_, at) | SourceCrash(at) | TargetCrash(at) -> Some(at)
    _ -> None
  }
}

fn trip(sim: Sim, point: Point) -> Sim {
  let #(hits, rest) =
    list.partition(sim.faults, fn(fault) { fault_point(fault) == Some(point) })
  list.fold(hits, Sim(..sim, faults: rest), apply_fault)
}

fn apply_fault(sim: Sim, fault: Fault) -> Sim {
  case fault {
    BouncerCrash(ordinal, _) -> crash_bouncer(sim, ordinal)
    SourceCrash(_) -> crash_source(sim)
    TargetCrash(_) -> crash_target(sim)
    _ -> sim
  }
}

fn has_fault(sim: Sim, wanted: fn(Fault) -> Bool) -> Bool {
  list.any(sim.config.faults, wanted)
}

fn deliver(sim: Sim, delivery: Delivery) -> Sim {
  case delivery {
    Trip(fault) ->
      case list.contains(sim.faults, fault) {
        True ->
          apply_fault(
            Sim(..sim, faults: list.filter(sim.faults, fn(f) { f != fault })),
            fault,
          )
        False -> sim
      }
    PeerTick -> peer_tick(sim)
    ToSourceEntity(op) -> source_entity(sim, op)
    ToTargetEntity(op) -> target_entity(sim, op)
    PeerMoved -> soon(Sim(..sim, peer_to_target: True), StubAck)
    StubAck ->
      case sim.source_mode {
        SourceDead -> sim
        _ -> step_source(Sim(..sim, source_mode: Direct), source.Repointed)
      }
    SourceEvent(event) ->
      case sim.source_mode {
        SourceDead -> sim
        _ -> step_source(sim, event)
      }
    ArrivalEvent(event) -> step_arrival(sim, event)
    AtBouncer(ordinal, message) -> at_bouncer(sim, ordinal, message)
    BouncerTimer(ordinal) -> freeze_bound(sim, ordinal)
  }
}

fn peer_tick(sim: Sim) -> Sim {
  case sim.peer_next > sim.config.ops {
    True -> sim
    False -> {
      let op = sim.peer_next
      let delivery = case sim.peer_to_target {
        True -> ToTargetEntity(op)
        False -> ToSourceEntity(op)
      }
      Sim(..sim, peer_next: op + 1)
      |> soon(delivery)
      |> schedule(sim.now + sim.config.op_every_ms, PeerTick)
    }
  }
}

fn source_entity(sim: Sim, op: Int) -> Sim {
  case sim.source_mode {
    Serving -> Sim(..sim, source_ops: [op, ..sim.source_ops])
    Forwarding -> step_source(sim, source.Forwarded(<<op:32>>))
    Direct -> soon(sim, ToTargetEntity(op))
    SourceDead -> Sim(..sim, lost: [op, ..sim.lost])
  }
}

fn target_entity(sim: Sim, op: Int) -> Sim {
  case sim.target_mode {
    Arriving -> Sim(..sim, stash: [op, ..sim.stash])
    Open -> Sim(..sim, target_ops: [op, ..sim.target_ops])
    _ -> Sim(..sim, lost: [op, ..sim.lost])
  }
}

fn step_source(sim: Sim, event: source.Event) -> Sim {
  let #(next, effects) = source.step(sim.source, event, sim.now)
  list.fold(effects, Sim(..sim, source: next), source_effect)
}

fn source_effect(sim: Sim, effect: source.Effect) -> Sim {
  case effect {
    source.ToHome(message) -> to_home(sim, message)
    source.RunPrecopy ->
      case sim.config.precopy {
        Some(Ok(ms)) ->
          schedule(sim, sim.now + ms, SourceEvent(source.PrecopyDone(Ok(1))))
        Some(Error(ms)) ->
          schedule(
            sim,
            sim.now + ms,
            SourceEvent(
              source.PrecopyDone(Error("precopy target did not answer")),
            ),
          )
        None -> sim
      }
    source.RunFreeze -> {
      let tail = encode_ops(list.reverse(sim.source_ops), sim.config)
      Sim(..sim, source_mode: Forwarding)
      |> soon(SourceEvent(source.FreezeReady(Ok(tail))))
    }
    source.Commit(_, _) -> soon(sim, PeerMoved)
    source.Thaw(entries) ->
      Sim(
        ..sim,
        source_mode: Serving,
        source_ops: list.fold(entries, sim.source_ops, fn(acc, entry) {
          [decode_op(entry.1), ..acc]
        }),
      )
    source.Abandon -> Sim(..sim, source_mode: Serving)
    source.Fence -> Sim(..sim, source_mode: SourceDead)
    source.After(timer, ms) ->
      schedule(sim, sim.now + ms, SourceEvent(source.Fired(timer)))
    source.Done(outcome) -> Sim(..sim, outcome: Some(outcome))
  }
}

fn step_arrival(sim: Sim, event: arrival.Event) -> Sim {
  case sim.target_mode, sim.arrival, event {
    TargetDead, _, _ -> sim
    _, None, arrival.FromHome(message.Install(move, key, stamp, version, ..)) -> {
      let fresh =
        arrival.new(
          move:,
          key:,
          epoch: stamp,
          version:,
          me: target_worker(),
          status_after_ms: 2500,
        )
      step_arrival(Sim(..sim, arrival: Some(fresh)), event)
    }
    _, None, _ -> sim
    _, Some(current), _ -> {
      let #(next, effects) = arrival.step(current, event)
      list.fold(effects, Sim(..sim, arrival: Some(next)), arrival_effect)
    }
  }
}

fn arrival_effect(sim: Sim, effect: arrival.Effect) -> Sim {
  case effect {
    arrival.ToHome(message) -> to_home(sim, message)
    arrival.RunInstall(tail) -> {
      let failing = has_fault(sim, fn(f) { f == InstallFails })
      let delay =
        list.fold(sim.config.faults, 0, fn(acc, fault) {
          case fault {
            InstallDelay(ms) -> ms
            _ -> acc
          }
        })
      case failing {
        True ->
          soon(sim, ArrivalEvent(arrival.InstallReady(Error("decode failed"))))
        False ->
          Sim(
            ..sim,
            target_mode: Arriving,
            target_ops: list.reverse(decode_ops(tail)),
          )
          |> schedule(
            sim.now + 1 + delay,
            ArrivalEvent(arrival.InstallReady(Ok(<<"target-ref">>))),
          )
      }
    }
    arrival.Deliver(_, bytes) ->
      Sim(..sim, target_ops: [decode_op(bytes), ..sim.target_ops])
    arrival.Close(_) ->
      Sim(
        ..sim,
        target_mode: Open,
        target_ops: list.append(sim.stash, sim.target_ops),
        stash: [],
      )
    arrival.Discard -> Sim(..sim, target_mode: Discarded, stash: [])
    arrival.After(timer, ms) ->
      schedule(sim, sim.now + ms, ArrivalEvent(arrival.Fired(timer)))
    arrival.Done(outcome) -> Sim(..sim, arrival_outcome: Some(outcome))
  }
}

fn home(sim: Sim) -> Option(Int) {
  [0, 1, 2]
  |> list.find(fn(ordinal) { bouncer(sim, ordinal).alive })
  |> option.from_result
}

fn successor(sim: Sim, ordinal: Int) -> Option(Int) {
  [0, 1, 2]
  |> list.find(fn(other) { other > ordinal && bouncer(sim, other).alive })
  |> option.from_result
}

fn bouncer(sim: Sim, ordinal: Int) -> Bouncer {
  case dict.get(sim.bouncers, ordinal) {
    Ok(found) -> found
    Error(Nil) -> Bouncer(False, None)
  }
}

fn put_bouncer(sim: Sim, ordinal: Int, value: Bouncer) -> Sim {
  Sim(..sim, bouncers: dict.insert(sim.bouncers, ordinal, value))
}

fn put_move(sim: Sim, ordinal: Int, move: Option(HomeMove)) -> Sim {
  put_bouncer(sim, ordinal, Bouncer(..bouncer(sim, ordinal), move:))
}

fn to_home(sim: Sim, message: ToBouncer) -> Sim {
  case home(sim) {
    Some(ordinal) -> soon(sim, AtBouncer(ordinal, message))
    None -> sim
  }
}

fn to_source(sim: Sim, message: message.ToMover) -> Sim {
  soon(sim, SourceEvent(source.FromHome(message)))
}

fn to_target(sim: Sim, message: message.ToMover) -> Sim {
  soon(sim, ArrivalEvent(arrival.FromHome(message)))
}

fn at_bouncer(sim: Sim, ordinal: Int, message: ToBouncer) -> Sim {
  case bouncer(sim, ordinal) {
    Bouncer(alive: False, ..) -> sim
    Bouncer(alive: True, move: move) -> handle_home(sim, ordinal, move, message)
  }
}

fn handle_home(
  sim: Sim,
  ordinal: Int,
  move: Option(HomeMove),
  message: ToBouncer,
) -> Sim {
  case message, move {
    message.BeginMove(..), _ -> begin(trip(sim, OnBegin), ordinal, message)
    message.BlobChunk(_, part, index, count, bytes), Some(current) ->
      chunk(sim, ordinal, current, part, index, count, bytes)
    message.Forward(_, seq, bytes), Some(current) ->
      forward(sim, ordinal, current, seq, bytes)
    message.Installed(_, target_ref), Some(current) ->
      installed(sim, ordinal, current, target_ref)
    message.InstallRefused(_, _), Some(_) -> abort(sim, ordinal)
    message.SealSource(_, last_seq), Some(current) -> {
      let sim = trip(sim, OnSeal)
      case bouncer(sim, ordinal).alive {
        False -> sim
        True ->
          try_seal(sim, ordinal, HomeMove(..current, sealed: Some(last_seq)))
      }
    }
    message.Applied(_, last_seq), Some(current) -> {
      let sim = trip(sim, OnApplied)
      case bouncer(sim, ordinal).alive {
        False -> sim
        True -> applied(sim, ordinal, current, last_seq)
      }
    }
    message.AskMoveStatus(_, reply_to), _ ->
      status(sim, ordinal, move, reply_to)
    _, _ -> sim
  }
}

fn begin(sim: Sim, ordinal: Int, message: ToBouncer) -> Sim {
  let refusal =
    list.find_map(sim.config.faults, fn(fault) {
      case fault {
        Refuse(reason) -> Ok(reason)
        _ -> Error(Nil)
      }
    })
  case bouncer(sim, ordinal).alive, refusal, message {
    False, _, _ -> sim
    True, Ok(reason), _ ->
      to_source(sim, message.MoveRefused(move_id(), reason))
    True, Error(Nil), message.BeginMove(_, key, stamp, _, version, _) -> {
      let record = case bouncer(sim, ordinal).move {
        Some(existing) -> existing
        None ->
          HomeMove(
            key:,
            to_epoch: epoch.next(stamp),
            version:,
            chunks: dict.new(),
            counts: dict.new(),
            tail_sent: False,
            forwards: dict.new(),
            received: 0,
            released: 0,
            committed: False,
            target_ref: <<>>,
            sealed: None,
            seal_sent: False,
            resend_pending: False,
            source_down: False,
            applied: 0,
          )
      }
      sim
      |> put_move(ordinal, Some(record))
      |> mirror(ordinal, record)
      |> to_source(message.MoveAccepted(move_id()))
    }
    True, Error(Nil), _ -> sim
  }
}

fn mirror(sim: Sim, ordinal: Int, record: HomeMove) -> Sim {
  case successor(sim, ordinal) {
    Some(next) ->
      put_move(
        sim,
        next,
        Some(
          HomeMove(
            ..record,
            forwards: dict.new(),
            received: 0,
            released: 0,
            committed: False,
          ),
        ),
      )
    None -> sim
  }
}

fn chunk(
  sim: Sim,
  ordinal: Int,
  current: HomeMove,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
) -> Sim {
  let sim = case part {
    blob.Tail -> trip(sim, OnTailChunk)
    _ -> sim
  }
  case bouncer(sim, ordinal).alive {
    False -> sim
    True -> {
      let code = blob.part_code(part)
      let current =
        HomeMove(
          ..current,
          chunks: dict.insert(current.chunks, #(code, index), bytes),
          counts: dict.insert(current.counts, code, count),
        )
      let sim =
        sim
        |> put_move(ordinal, Some(current))
        |> mirror(ordinal, current)
        |> to_source(message.ChunkAck(move_id(), part, index))
      case tail_complete(current) && !current.tail_sent {
        False -> sim
        True -> {
          let current = HomeMove(..current, tail_sent: True)
          let sim =
            relay_tail(put_move(sim, ordinal, Some(current)), current)
            |> mirror(ordinal, current)
            |> schedule(sim.now + 2000, BouncerTimer(ordinal))
          trip(sim, AfterTailComplete)
        }
      }
    }
  }
}

fn tail_complete(current: HomeMove) -> Bool {
  let code = blob.part_code(blob.Tail)
  case dict.get(current.counts, code) {
    Error(Nil) -> False
    Ok(count) ->
      list.all(indices(count), fn(index) {
        dict.has_key(current.chunks, #(code, index))
      })
  }
}

fn install_chunk(
  sim: Sim,
  current: HomeMove,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
) -> Sim {
  to_target(
    sim,
    message.Install(
      move_id(),
      current.key,
      current.to_epoch,
      current.version,
      part,
      index,
      count,
      bytes,
    ),
  )
}

fn relay_tail(sim: Sim, current: HomeMove) -> Sim {
  let code = blob.part_code(blob.Tail)
  case dict.get(current.counts, code) {
    Error(Nil) -> sim
    Ok(count) ->
      list.fold(indices(count), sim, fn(acc, index) {
        case dict.get(current.chunks, #(code, index)) {
          Ok(bytes) ->
            install_chunk(acc, current, blob.Tail, index, count, bytes)
          Error(Nil) -> acc
        }
      })
  }
}

fn forward(
  sim: Sim,
  ordinal: Int,
  current: HomeMove,
  seq: Int,
  bytes: BitArray,
) -> Sim {
  case seq == current.received + 1 {
    False -> sim
    True -> {
      let current =
        HomeMove(
          ..current,
          forwards: dict.insert(current.forwards, seq, bytes),
          received: seq,
        )
      case current.committed {
        True -> release_through(sim, ordinal, current)
        False -> put_move(sim, ordinal, Some(current))
      }
    }
  }
}

fn release_through(sim: Sim, ordinal: Int, current: HomeMove) -> Sim {
  case current.released < current.received {
    False -> try_seal(sim, ordinal, current)
    True -> {
      let seq = current.released + 1
      let bytes = case dict.get(current.forwards, seq) {
        Ok(found) -> found
        Error(Nil) -> <<>>
      }
      let sim = to_target(sim, message.Release(move_id(), seq, bytes))
      release_through(
        sim,
        ordinal,
        HomeMove(
          ..current,
          released: seq,
          forwards: dict.delete(current.forwards, seq),
        ),
      )
    }
  }
}

fn try_seal(sim: Sim, ordinal: Int, current: HomeMove) -> Sim {
  let current = case current.source_down, current.sealed, current.committed {
    True, None, True -> HomeMove(..current, sealed: Some(current.received))
    _, _, _ -> current
  }
  case current.committed, current.sealed, current.seal_sent {
    True, Some(last_seq), False if current.released >= last_seq ->
      sim
      |> to_target(message.Seal(move_id(), last_seq))
      |> check_done(ordinal, HomeMove(..current, seal_sent: True))
    _, _, _ -> check_done(sim, ordinal, current)
  }
}

fn installed(
  sim: Sim,
  ordinal: Int,
  current: HomeMove,
  target_ref: BitArray,
) -> Sim {
  let sim = trip(sim, OnInstalled)
  case bouncer(sim, ordinal).alive, sim.store.aborted {
    False, _ | _, True -> sim
    True, False -> {
      let sim =
        Sim(
          ..sim,
          store: Store(
            ..sim.store,
            commit: Some(target_ref),
            holders: commit_holders(sim, ordinal),
          ),
        )
        |> trip(AfterCommitWrite)
      case bouncer(sim, ordinal).alive {
        False -> sim
        True -> {
          let current = HomeMove(..current, committed: True, target_ref:)
          sim
          |> to_source(message.Committed(
            move_id(),
            target_ref,
            current.to_epoch,
          ))
          |> release_through(ordinal, current)
        }
      }
    }
  }
}

fn applied(sim: Sim, ordinal: Int, current: HomeMove, last_seq: Int) -> Sim {
  let current = HomeMove(..current, applied: int.max(current.applied, last_seq))
  case current.resend_pending {
    True ->
      sim
      |> put_move(
        ordinal,
        Some(
          HomeMove(
            ..current,
            resend_pending: False,
            received: last_seq,
            released: last_seq,
          ),
        ),
      )
      |> to_source(message.ResendFrom(move_id(), last_seq + 1))
    False -> check_done(sim, ordinal, current)
  }
}

fn check_done(sim: Sim, ordinal: Int, current: HomeMove) -> Sim {
  case current.sealed {
    Some(last_seq) if current.applied >= last_seq ->
      sim
      |> put_move(ordinal, None)
      |> to_source(message.AppliedThrough(move_id(), last_seq))
    _ -> put_move(sim, ordinal, Some(current))
  }
}

fn status(
  sim: Sim,
  ordinal: Int,
  move: Option(HomeMove),
  reply_to: WorkerId,
) -> Sim {
  let reply = fn(sim, verdict) {
    case reply_to == target_worker() {
      True -> to_target(sim, message.MoveStatus(move_id(), verdict))
      False -> to_source(sim, message.MoveStatus(move_id(), verdict))
    }
  }
  case sim.store.commit, sim.store.aborted, move {
    Some(target_ref), _, _ ->
      reply(sim, message.VerdictCommitted(target_ref, epoch_of(8)))
    None, True, _ -> reply(sim, message.VerdictAborted)
    None, False, None -> {
      let sim = abort(sim, ordinal)
      reply(sim, message.VerdictAborted)
    }
    None, False, Some(_) -> sim
  }
}

fn freeze_bound(sim: Sim, ordinal: Int) -> Sim {
  case bouncer(sim, ordinal) {
    Bouncer(alive: True, move: Some(current)) if !current.committed ->
      abort(sim, ordinal)
    _ -> sim
  }
}

fn abort(sim: Sim, ordinal: Int) -> Sim {
  case sim.store.commit {
    Some(_) -> sim
    None ->
      Sim(..sim, store: Store(..sim.store, aborted: True))
      |> put_move(ordinal, None)
      |> to_target(message.Abort(move_id()))
      |> to_source(message.Aborted(move_id()))
  }
}

fn crash_bouncer(sim: Sim, ordinal: Int) -> Sim {
  let was_home = home(sim) == Some(ordinal)
  let sim = put_bouncer(sim, ordinal, Bouncer(alive: False, move: None))
  let holders = list.filter(sim.store.holders, fn(other) { other != ordinal })
  let sim =
    Sim(
      ..sim,
      store: Store(..sim.store, holders:, commit: case holders {
        [] -> None
        _ -> sim.store.commit
      }),
    )
  let sim = case was_home, home(sim) {
    True, Some(next) -> take_over(sim, next)
    _, _ -> sim
  }
  let sim = case was_home {
    True ->
      sim
      |> soon(SourceEvent(source.HomeChanged))
      |> soon(ArrivalEvent(arrival.HomeChanged))
    False -> sim
  }
  let alive = list.count([0, 1, 2], fn(other) { bouncer(sim, other).alive })
  case alive < 2 && !sim.lease_lapse_scheduled {
    True ->
      Sim(..sim, lease_lapse_scheduled: True)
      |> schedule(sim.now + 5000, SourceEvent(source.LeaseLapsed))
    False -> sim
  }
}

fn commit_holders(sim: Sim, ordinal: Int) -> List(Int) {
  case sim.config.commit_quorum_one {
    True -> [ordinal]
    False -> list.filter([0, 1, 2], fn(other) { bouncer(sim, other).alive })
  }
}

fn take_over(sim: Sim, ordinal: Int) -> Sim {
  let mirrored = bouncer(sim, ordinal).move
  case sim.store.commit, mirrored {
    Some(target_ref), Some(current) ->
      put_move(
        sim,
        ordinal,
        Some(
          HomeMove(
            ..current,
            committed: True,
            target_ref:,
            resend_pending: True,
          ),
        ),
      )
    Some(target_ref), None ->
      put_move(
        sim,
        ordinal,
        Some(HomeMove(
          key: entity(),
          to_epoch: epoch_of(8),
          version: 1,
          chunks: dict.new(),
          counts: dict.new(),
          tail_sent: True,
          forwards: dict.new(),
          received: 0,
          released: 0,
          committed: True,
          target_ref:,
          sealed: None,
          seal_sent: False,
          resend_pending: True,
          source_down: False,
          applied: 0,
        )),
      )
    None, Some(current) ->
      case current.tail_sent {
        True -> abort(sim, ordinal)
        False -> sim
      }
    None, None -> sim
  }
}

fn crash_source(sim: Sim) -> Sim {
  let sim = Sim(..sim, source_mode: SourceDead)
  case home(sim) {
    None -> sim
    Some(ordinal) ->
      case bouncer(sim, ordinal).move {
        None -> sim
        Some(current) ->
          case current.tail_sent || current.committed {
            False -> abort(sim, ordinal)
            True ->
              try_seal(sim, ordinal, HomeMove(..current, source_down: True))
          }
      }
  }
}

fn crash_target(sim: Sim) -> Sim {
  let sim = Sim(..sim, target_mode: TargetDead)
  case sim.store.commit, home(sim) {
    None, Some(ordinal) ->
      case bouncer(sim, ordinal).move {
        Some(_) -> abort(sim, ordinal)
        None -> sim
      }
    _, _ -> sim
  }
}

fn padding(size: Int) -> BitArray {
  case size <= 0 {
    True -> <<>>
    False -> <<0:size(size)-unit(8)>>
  }
}

fn encode_ops(ops: List(Int), config: Config) -> BitArray {
  let body =
    list.fold(ops, <<>>, fn(acc, op) { bit_array.append(acc, <<op:32>>) })
  bit_array.concat([<<list.length(ops):32>>, body, padding(config.tail_padding)])
}

fn decode_ops(bytes: BitArray) -> List(Int) {
  case bytes {
    <<count:32, rest:bits>> -> take_ops(rest, count, [])
    _ -> []
  }
}

fn take_ops(bytes: BitArray, count: Int, acc: List(Int)) -> List(Int) {
  case count, bytes {
    0, _ -> list.reverse(acc)
    _, <<op:32, rest:bits>> -> take_ops(rest, count - 1, [op, ..acc])
    _, _ -> list.reverse(acc)
  }
}

fn decode_op(bytes: BitArray) -> Int {
  case bytes {
    <<op:32>> -> op
    _ -> -1
  }
}

pub fn debug(sim: Sim) -> String {
  string.inspect(#(
    source.phase(sim.source),
    option.map(sim.arrival, arrival.phase),
    sim.bouncers,
    sim.store,
  ))
}

pub fn describe(sim: Sim) -> String {
  "now="
  <> int.to_string(sim.now)
  <> " lost="
  <> int.to_string(list.length(sim.lost))
}

fn indices(count: Int) -> List(Int) {
  int.range(from: count - 1, to: -1, with: [], run: list.prepend)
}
