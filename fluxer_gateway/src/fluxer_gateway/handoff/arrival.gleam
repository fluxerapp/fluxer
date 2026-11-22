// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{type Part}
import bouncer_proto/epoch.{type Epoch}
import bouncer_proto/key.{type Key}
import bouncer_proto/message.{type MoveId, type ToBouncer, type ToMover}
import bouncer_proto/worker.{type WorkerId}
import gleam/dict.{type Dict}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

const unknown_answers_to_discard = 3

pub type Outcome {
  Arrived(last_seq: Int)
  Discarded
  Refused(reason: String)
}

pub type Timer {
  StatusTimer(generation: Int)
}

pub type Event {
  FromHome(message: ToMover)
  InstallReady(result: Result(BitArray, String))
  Fired(timer: Timer)
  HomeChanged
}

pub type Effect {
  ToHome(message: ToBouncer)
  RunInstall(tail: BitArray)
  Deliver(seq: Int, bytes: BitArray)
  Close(last_seq: Int)
  Discard
  After(timer: Timer, ms: Int)
  Done(outcome: Outcome)
}

pub type Phase {
  Receiving
  Installing(aborted: Bool)
  Waiting(committed: Bool, generation: Int)
  Finished(outcome: Outcome)
}

pub opaque type Arrival {
  Arrival(
    move: MoveId,
    key: Key,
    epoch: Epoch,
    version: Int,
    me: WorkerId,
    status_after_ms: Int,
    phase: Phase,
    chunks: Dict(#(Int, Int), BitArray),
    counts: Dict(Int, Int),
    next_seq: Int,
    early: Dict(Int, BitArray),
    seal: Option(Int),
    unknown_answers: Int,
  )
}

pub fn new(
  move move: MoveId,
  key key: Key,
  epoch epoch: Epoch,
  version version: Int,
  me me: WorkerId,
  status_after_ms status_after_ms: Int,
) -> Arrival {
  Arrival(
    move:,
    key:,
    epoch:,
    version:,
    me:,
    status_after_ms:,
    phase: Receiving,
    chunks: dict.new(),
    counts: dict.new(),
    next_seq: 1,
    early: dict.new(),
    seal: None,
    unknown_answers: 0,
  )
}

pub fn phase(arrival: Arrival) -> Phase {
  arrival.phase
}

pub fn move_id(arrival: Arrival) -> MoveId {
  arrival.move
}

pub fn key(arrival: Arrival) -> Key {
  arrival.key
}

pub fn epoch(arrival: Arrival) -> Epoch {
  arrival.epoch
}

pub fn version(arrival: Arrival) -> Int {
  arrival.version
}

pub fn applied_seq(arrival: Arrival) -> Int {
  arrival.next_seq - 1
}

pub fn is_finished(arrival: Arrival) -> Bool {
  case arrival.phase {
    Finished(_) -> True
    _ -> False
  }
}

pub fn step(arrival: Arrival, event: Event) -> #(Arrival, List(Effect)) {
  case arrival.phase, event {
    Finished(Arrived(last_seq)), HomeChanged -> #(arrival, [
      ToHome(message.Applied(arrival.move, last_seq)),
    ])
    Finished(_), _ -> #(arrival, [])
    Receiving, FromHome(message.Install(_, _, _, _, part, index, count, bytes))
    -> receive_chunk(arrival, part, index, count, bytes)
    Receiving, FromHome(message.Abort(_))
    | Receiving, FromHome(message.MoveStatus(_, message.VerdictAborted))
    -> finish(arrival, Discarded, [])
    Receiving, FromHome(message.MoveStatus(_, message.VerdictUnknown)) ->
      case arrival.unknown_answers + 1 >= unknown_answers_to_discard {
        True -> finish(arrival, Discarded, [])
        False -> #(
          Arrival(..arrival, unknown_answers: arrival.unknown_answers + 1),
          [],
        )
      }
    Receiving, Fired(StatusTimer(0)) -> #(arrival, [
      ToHome(message.AskMoveStatus(arrival.move, arrival.me)),
      After(StatusTimer(0), arrival.status_after_ms),
    ])
    Installing(_), FromHome(message.Abort(_)) -> #(
      Arrival(..arrival, phase: Installing(True)),
      [],
    )
    Installing(True), InstallReady(Ok(_)) ->
      finish(arrival, Discarded, [Discard])
    Installing(True), InstallReady(Error(_)) -> finish(arrival, Discarded, [])
    Installing(False), InstallReady(Ok(target_ref)) -> #(
      Arrival(..arrival, phase: Waiting(False, 1)),
      [
        ToHome(message.Installed(arrival.move, target_ref)),
        After(StatusTimer(1), arrival.status_after_ms),
      ],
    )
    Installing(False), InstallReady(Error(reason)) ->
      finish(arrival, Refused(reason), [
        ToHome(message.InstallRefused(arrival.move, message.DecodeFailed)),
      ])
    Waiting(False, _), FromHome(message.Abort(_))
    | Waiting(False, _), FromHome(message.MoveStatus(_, message.VerdictAborted))
    -> finish(arrival, Discarded, [Discard])
    Waiting(_, generation), FromHome(message.Release(_, seq, bytes)) ->
      release(Arrival(..arrival, phase: Waiting(True, generation)), seq, bytes)
    Waiting(_, generation), FromHome(message.Seal(_, last_seq)) ->
      close_when_ready(
        Arrival(
          ..arrival,
          phase: Waiting(True, generation),
          seal: Some(last_seq),
        ),
        [],
      )
    Waiting(_, generation),
      FromHome(message.MoveStatus(_, message.VerdictCommitted(..)))
    -> #(Arrival(..arrival, phase: Waiting(True, generation)), [])
    Waiting(committed, generation), Fired(StatusTimer(fired))
      if fired == generation
    -> #(Arrival(..arrival, phase: Waiting(committed, generation + 1)), [
      ToHome(message.AskMoveStatus(arrival.move, arrival.me)),
      After(StatusTimer(generation + 1), arrival.status_after_ms),
    ])
    Waiting(_, _), HomeChanged -> #(arrival, [
      ToHome(message.Applied(arrival.move, applied_seq(arrival))),
    ])
    _, _ -> #(arrival, [])
  }
}

fn finish(
  arrival: Arrival,
  outcome: Outcome,
  effects: List(Effect),
) -> #(Arrival, List(Effect)) {
  #(
    Arrival(..arrival, phase: Finished(outcome)),
    list.append(effects, [Done(outcome)]),
  )
}

fn receive_chunk(
  arrival: Arrival,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
) -> #(Arrival, List(Effect)) {
  let code = blob.part_code(part)
  let first = dict.is_empty(arrival.counts)
  let arrival =
    Arrival(
      ..arrival,
      chunks: dict.insert(arrival.chunks, #(code, index), bytes),
      counts: dict.insert(arrival.counts, code, count),
      unknown_answers: 0,
    )
  case assemble(arrival, blob.Tail) {
    Error(Nil) if first -> #(arrival, [
      After(StatusTimer(0), arrival.status_after_ms),
    ])
    Error(Nil) -> #(arrival, [])
    Ok(tail) -> #(
      Arrival(..arrival, phase: Installing(False), chunks: dict.new()),
      [RunInstall(tail)],
    )
  }
}

fn assemble(arrival: Arrival, part: Part) -> Result(BitArray, Nil) {
  let code = blob.part_code(part)
  use count <- result.try(dict.get(arrival.counts, code))
  arrival.chunks
  |> dict.to_list
  |> list.filter_map(fn(entry) {
    let #(#(chunk_part, index), bytes) = entry
    case chunk_part == code {
      True -> Ok(#(index, bytes))
      False -> Error(Nil)
    }
  })
  |> blob.join(count)
  |> result.replace_error(Nil)
}

fn release(
  arrival: Arrival,
  seq: Int,
  bytes: BitArray,
) -> #(Arrival, List(Effect)) {
  case seq < arrival.next_seq {
    True -> #(arrival, [])
    False ->
      Arrival(..arrival, early: dict.insert(arrival.early, seq, bytes))
      |> close_when_ready([])
  }
}

fn close_when_ready(
  arrival: Arrival,
  acc: List(Effect),
) -> #(Arrival, List(Effect)) {
  case dict.get(arrival.early, arrival.next_seq) {
    Ok(bytes) ->
      close_when_ready(
        Arrival(
          ..arrival,
          early: dict.delete(arrival.early, arrival.next_seq),
          next_seq: arrival.next_seq + 1,
        ),
        [Deliver(arrival.next_seq, bytes), ..acc],
      )
    Error(Nil) ->
      case arrival.seal {
        Some(last_seq) if last_seq <= arrival.next_seq - 1 -> {
          let #(arrival, done) =
            finish(arrival, Arrived(last_seq), [
              Close(last_seq),
              ToHome(message.Applied(arrival.move, last_seq)),
            ])
          #(arrival, list.append(list.reverse(acc), done))
        }
        _ -> #(arrival, list.reverse(acc))
      }
  }
}
