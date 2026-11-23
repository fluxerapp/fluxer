// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{type Part}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{
  type MoveId, type Refusal, type ToBouncer, type ToMover,
}
import bouncer_proto/worker.{type WorkerId}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}

pub type Timings {
  Timings(
    request_timeout_ms: Int,
    admission_attempts: Int,
    freeze_bound_ms: Int,
    status_poll_ms: Int,
    applied_bound_ms: Int,
  )
}

const applied_resends = 10

fn applied_every(source: Source) -> Int {
  int.max(source.timings.applied_bound_ms / applied_resends, 1)
}

pub fn default_timings() -> Timings {
  Timings(
    request_timeout_ms: 500,
    admission_attempts: 4,
    freeze_bound_ms: 2000,
    status_poll_ms: 250,
    applied_bound_ms: 30_000,
  )
}

pub type Reason {
  Refused(reason: Refusal)
  AdmissionTimeout
  FreezeFailed(reason: String)
  AbortedEarly
  LeaseLost
}

pub type Outcome {
  Moved(target_ref: BitArray, epoch: Epoch, freeze_ms: Int)
  Thawed(epoch: Epoch)
  NotMoved(reason: Reason)
  Fenced
}

pub type Timer {
  AdmissionTimer(attempt: Int)
  FreezeTimer
  PollTimer(generation: Int)
  LeaseTimer
  AppliedTimer(resends_left: Int)
}

pub type Event {
  Begin
  FromHome(message: ToMover)
  PrecopyDone(result: Result(Int, String))
  FreezeReady(result: Result(BitArray, String))
  Forwarded(bytes: BitArray)
  Repointed
  Fired(timer: Timer)
  HomeChanged
  LeaseLapsed
}

pub type Effect {
  ToHome(message: ToBouncer)
  RunPrecopy
  RunFreeze
  Commit(target_ref: BitArray, epoch: Epoch)
  Thaw(entries: List(#(Int, BitArray)))
  Abandon
  Fence
  After(timer: Timer, ms: Int)
  Done(outcome: Outcome)
}

pub type Phase {
  Admitting(attempt: Int)
  Copying
  Freezing(aborted: Bool)
  Streaming
  InDoubt(generation: Int)
  Releasing(target_ref: BitArray, epoch: Epoch, freeze_ms: Int)
  Sealed(target_ref: BitArray, epoch: Epoch, freeze_ms: Int, last_seq: Int)
  Finished(outcome: Outcome)
}

type Stream {
  Stream(
    part: Part,
    chunks: Dict(Int, BitArray),
    count: Int,
    next: Int,
    unacked: Set(Int),
  )
}

pub opaque type Source {
  Source(
    move: MoveId,
    key: Key,
    epoch: Epoch,
    to: WorkerId,
    version: Int,
    estimate: Int,
    timings: Timings,
    precopy: Bool,
    phase: Phase,
    stream: Option(Stream),
    hold: Dict(Int, BitArray),
    next_seq: Int,
    frozen_at: Int,
  )
}

pub fn new(
  move move: MoveId,
  key key: Key,
  epoch epoch: Epoch,
  to to: WorkerId,
  version version: Int,
  estimate estimate: Int,
  timings timings: Timings,
  precopy precopy: Bool,
) -> Source {
  Source(
    move:,
    key:,
    epoch:,
    to:,
    version:,
    estimate:,
    timings:,
    precopy:,
    phase: Admitting(1),
    stream: None,
    hold: dict.new(),
    next_seq: 1,
    frozen_at: 0,
  )
}

pub fn phase(source: Source) -> Phase {
  source.phase
}

pub fn move_id(source: Source) -> MoveId {
  source.move
}

pub fn key(source: Source) -> Key {
  source.key
}

pub fn target(source: Source) -> WorkerId {
  source.to
}

pub fn held(source: Source) -> List(#(Int, BitArray)) {
  entries_from(source, 1)
}

pub fn is_finished(source: Source) -> Bool {
  case source.phase {
    Finished(_) -> True
    _ -> False
  }
}

pub fn step(source: Source, event: Event, now: Int) -> #(Source, List(Effect)) {
  case source.phase, event {
    Finished(_), _ -> #(source, [])
    Admitting(_), Begin -> #(source, admission(source, 1))
    Admitting(attempt), Fired(AdmissionTimer(fired)) if fired == attempt ->
      case attempt >= source.timings.admission_attempts {
        True -> finish(source, NotMoved(AdmissionTimeout), [])
        False -> {
          let source = Source(..source, phase: Admitting(attempt + 1))
          #(source, admission(source, attempt + 1))
        }
      }
    Admitting(_), HomeChanged -> #(source, [ToHome(begin_move(source))])
    Admitting(_), FromHome(message.MoveAccepted(_)) ->
      case source.precopy {
        True -> #(Source(..source, phase: Copying), [
          RunPrecopy,
          After(LeaseTimer, source.timings.status_poll_ms),
        ])
        False -> #(Source(..source, phase: Freezing(False), frozen_at: now), [
          RunFreeze,
        ])
      }
    Copying, PrecopyDone(_) -> #(
      Source(..source, phase: Freezing(False), frozen_at: now),
      [RunFreeze],
    )
    Copying, FromHome(message.Aborted(_))
    | Copying, FromHome(message.MoveStatus(_, message.VerdictAborted))
    -> finish(source, NotMoved(AbortedEarly), [Abandon])
    Copying, LeaseLapsed -> finish(source, NotMoved(LeaseLost), [Abandon])
    Copying, Fired(LeaseTimer) -> #(source, [
      After(LeaseTimer, source.timings.status_poll_ms),
    ])
    Admitting(_), FromHome(message.MoveRefused(_, reason)) ->
      finish(source, NotMoved(Refused(reason)), [])
    Admitting(_), LeaseLapsed -> finish(source, NotMoved(LeaseLost), [])
    Freezing(_), Forwarded(bytes) -> #(hold_entry(source, bytes).0, [])
    Freezing(True), FreezeReady(Ok(_)) ->
      finish(source, Thawed(source.epoch), [Thaw(held(source))])
    Freezing(False), FreezeReady(Ok(tail)) -> {
      let #(source, chunk_effects) = stream_part(source, blob.Tail, tail)
      let forwards =
        held(source)
        |> list.map(fn(entry) {
          ToHome(message.Forward(source.move, entry.0, entry.1))
        })
      let #(source, phase_effects) = after_tail_progress(source)
      #(
        source,
        list.flatten([
          chunk_effects,
          forwards,
          phase_effects,
          [After(FreezeTimer, source.timings.freeze_bound_ms)],
        ]),
      )
    }
    Freezing(_), FreezeReady(Error(reason)) ->
      finish(source, NotMoved(FreezeFailed(reason)), [Abandon])
    Freezing(_), LeaseLapsed -> finish(source, Fenced, [Fence])
    Streaming, FromHome(message.ChunkAck(_, blob.Tail, index))
    | InDoubt(_), FromHome(message.ChunkAck(_, blob.Tail, index))
    -> {
      let #(source, sent) = ack(source, index)
      let #(source, phase_effects) = after_tail_progress(source)
      #(source, list.append(sent, phase_effects))
    }
    Streaming, Forwarded(bytes)
    | InDoubt(_), Forwarded(bytes)
    | Releasing(..), Forwarded(bytes)
    -> {
      let #(source, seq) = hold_entry(source, bytes)
      #(source, [ToHome(message.Forward(source.move, seq, bytes))])
    }
    Streaming, Fired(FreezeTimer) ->
      finish(source, Thawed(source.epoch), [Thaw(held(source))])
    InDoubt(generation), Fired(FreezeTimer) -> poll(source, generation + 1)
    InDoubt(generation), Fired(PollTimer(fired)) if fired == generation ->
      poll(source, generation + 1)
    Streaming, HomeChanged -> #(
      source,
      list.append(resend_unacked(source), resend_forwards(source, 1)),
    )
    InDoubt(generation), HomeChanged -> {
      let #(source, polled) = poll(source, generation + 1)
      #(
        source,
        list.flatten([
          resend_unacked(source),
          resend_forwards(source, 1),
          polled,
        ]),
      )
    }
    Streaming, LeaseLapsed | InDoubt(_), LeaseLapsed ->
      finish(source, Fenced, [Fence])
    Freezing(_), FromHome(message.Aborted(_))
    | Freezing(_), FromHome(message.MoveStatus(_, message.VerdictAborted))
    -> #(Source(..source, phase: Freezing(True)), [])
    Streaming, FromHome(message.Committed(_, target_ref, to_epoch))
    | InDoubt(_), FromHome(message.Committed(_, target_ref, to_epoch))
    | Streaming,
      FromHome(message.MoveStatus(
        _,
        message.VerdictCommitted(target_ref, to_epoch),
      ))
    | InDoubt(_),
      FromHome(message.MoveStatus(
        _,
        message.VerdictCommitted(target_ref, to_epoch),
      ))
    -> {
      let freeze_ms = int.max(now - source.frozen_at, 0)
      #(Source(..source, phase: Releasing(target_ref, to_epoch, freeze_ms)), [
        Commit(target_ref, to_epoch),
      ])
    }
    Streaming, FromHome(message.Aborted(_))
    | InDoubt(_), FromHome(message.Aborted(_))
    | Streaming, FromHome(message.MoveStatus(_, message.VerdictAborted))
    | InDoubt(_), FromHome(message.MoveStatus(_, message.VerdictAborted))
    -> finish(source, Thawed(source.epoch), [Thaw(held(source))])
    Admitting(_), FromHome(message.Aborted(_)) ->
      finish(source, NotMoved(AbortedEarly), [Abandon])
    Releasing(target_ref, to_epoch, freeze_ms), Repointed -> {
      let last_seq = source.next_seq - 1
      #(
        Source(
          ..source,
          phase: Sealed(target_ref, to_epoch, freeze_ms, last_seq),
        ),
        [
          ToHome(message.SealSource(source.move, last_seq)),
          After(AppliedTimer(applied_resends), applied_every(source)),
        ],
      )
    }
    Sealed(_, _, _, last_seq), HomeChanged -> #(
      source,
      list.append(resend_forwards(source, 1), [
        ToHome(message.SealSource(source.move, last_seq)),
      ]),
    )
    Sealed(_, _, _, last_seq), Fired(AppliedTimer(left)) if left > 1 -> #(
      source,
      [
        ToHome(message.SealSource(source.move, last_seq)),
        After(AppliedTimer(left - 1), applied_every(source)),
      ],
    )
    Sealed(target_ref, to_epoch, freeze_ms, _), Fired(AppliedTimer(_)) ->
      finish(source, Moved(target_ref, to_epoch, freeze_ms), [])
    Releasing(..), FromHome(message.AppliedThrough(_, upto)) -> #(
      drop_through(source, upto),
      [],
    )
    Sealed(target_ref, to_epoch, freeze_ms, last_seq),
      FromHome(message.AppliedThrough(_, upto))
    ->
      case upto >= last_seq {
        True ->
          finish(
            drop_through(source, upto),
            Moved(target_ref, to_epoch, freeze_ms),
            [],
          )
        False -> #(drop_through(source, upto), [])
      }
    Releasing(..), FromHome(message.ResendFrom(_, seq)) -> #(
      source,
      resend_forwards(source, seq),
    )
    Sealed(_, _, _, last_seq), FromHome(message.ResendFrom(_, seq)) -> #(
      source,
      list.append(resend_forwards(source, seq), [
        ToHome(message.SealSource(source.move, last_seq)),
      ]),
    )
    _, _ -> #(source, [])
  }
}

fn admission(source: Source, attempt: Int) -> List(Effect) {
  [
    ToHome(begin_move(source)),
    After(AdmissionTimer(attempt), source.timings.request_timeout_ms),
  ]
}

fn begin_move(source: Source) -> ToBouncer {
  message.BeginMove(
    move: source.move,
    key: source.key,
    epoch: source.epoch,
    to: source.to,
    codec_version: source.version,
    bytes_estimate: source.estimate,
  )
}

fn finish(
  source: Source,
  outcome: Outcome,
  effects: List(Effect),
) -> #(Source, List(Effect)) {
  #(
    Source(..source, phase: Finished(outcome)),
    list.append(effects, [Done(outcome)]),
  )
}

fn poll(source: Source, generation: Int) -> #(Source, List(Effect)) {
  #(Source(..source, phase: InDoubt(generation)), [
    ToHome(message.AskMoveStatus(source.move, source.move.source)),
    After(PollTimer(generation), source.timings.status_poll_ms),
  ])
}

fn hold_entry(source: Source, bytes: BitArray) -> #(Source, Int) {
  let seq = source.next_seq
  #(
    Source(
      ..source,
      hold: dict.insert(source.hold, seq, bytes),
      next_seq: seq + 1,
    ),
    seq,
  )
}

fn entries_from(source: Source, seq: Int) -> List(#(Int, BitArray)) {
  source.hold
  |> dict.to_list
  |> list.filter(fn(entry) { entry.0 >= seq })
  |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
}

fn drop_through(source: Source, upto: Int) -> Source {
  Source(..source, hold: dict.filter(source.hold, fn(seq, _) { seq > upto }))
}

fn resend_forwards(source: Source, seq: Int) -> List(Effect) {
  entries_from(source, seq)
  |> list.map(fn(entry) {
    ToHome(message.Forward(source.move, entry.0, entry.1))
  })
}

fn stream_part(
  source: Source,
  part: Part,
  bytes: BitArray,
) -> #(Source, List(Effect)) {
  let chunks = blob.split(bytes)
  let stream =
    Stream(
      part:,
      chunks: chunks
        |> list.index_map(fn(chunk, index) { #(index, chunk) })
        |> dict.from_list,
      count: list.length(chunks),
      next: 0,
      unacked: set.new(),
    )
  pump(Source(..source, stream: Some(stream)))
}

fn pump(source: Source) -> #(Source, List(Effect)) {
  case source.stream {
    None -> #(source, [])
    Some(stream) -> {
      let #(stream, effects) = pump_loop(source.move, stream, [])
      #(Source(..source, stream: Some(stream)), effects)
    }
  }
}

fn pump_loop(
  move: MoveId,
  stream: Stream,
  acc: List(Effect),
) -> #(Stream, List(Effect)) {
  case
    stream.next < stream.count && set.size(stream.unacked) < blob.chunk_window
  {
    False -> #(stream, list.reverse(acc))
    True -> {
      let index = stream.next
      let effect = chunk_effect(move, stream, index)
      pump_loop(
        move,
        Stream(
          ..stream,
          next: index + 1,
          unacked: set.insert(stream.unacked, index),
        ),
        [effect, ..acc],
      )
    }
  }
}

fn chunk_effect(move: MoveId, stream: Stream, index: Int) -> Effect {
  let bytes = case dict.get(stream.chunks, index) {
    Ok(bytes) -> bytes
    Error(Nil) -> <<>>
  }
  ToHome(message.BlobChunk(move, stream.part, index, stream.count, bytes))
}

fn ack(source: Source, index: Int) -> #(Source, List(Effect)) {
  case source.stream {
    None -> #(source, [])
    Some(stream) ->
      pump(
        Source(
          ..source,
          stream: Some(
            Stream(..stream, unacked: set.delete(stream.unacked, index)),
          ),
        ),
      )
  }
}

fn resend_unacked(source: Source) -> List(Effect) {
  case source.stream {
    None -> []
    Some(stream) ->
      stream.unacked
      |> set.to_list
      |> list.sort(int.compare)
      |> list.map(chunk_effect(source.move, stream, _))
  }
}

fn stream_sent(source: Source) -> Bool {
  case source.stream {
    None -> True
    Some(stream) -> stream.next >= stream.count
  }
}

fn after_tail_progress(source: Source) -> #(Source, List(Effect)) {
  case source.phase, stream_sent(source) {
    InDoubt(_), _ -> #(source, [])
    _, False -> #(Source(..source, phase: Streaming), [])
    _, True -> #(Source(..source, phase: InDoubt(0)), [])
  }
}
