// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/handoff/arrival.{type Arrival, type Effect}
import gleam/erlang/atom
import gleam/list

fn receiving() -> #(Arrival, List(Effect)) {
  let assert Ok(at) = epoch.from_int(6)
  let move =
    MoveId(source: WorkerId(atom.create("arrival_test@test"), 1), serial: 1)
  let me = WorkerId(atom.create("arrival_test_target@test"), 1)
  let entity = Key(ns: 0, id: 1)
  arrival.new(
    move:,
    key: entity,
    epoch: at,
    version: 1,
    me:,
    status_after_ms: 4000,
  )
  |> arrival.step(
    arrival.FromHome(
      message.Install(move, entity, at, 1, blob.Tail, 0, 2, <<1>>),
    ),
  )
}

fn fire_timers(
  machine: Arrival,
  effects: List(Effect),
) -> #(Arrival, List(Effect)) {
  effects
  |> list.filter_map(fn(effect) {
    case effect {
      arrival.After(timer, _) -> Ok(arrival.Fired(timer))
      _ -> Error(Nil)
    }
  })
  |> list.fold(#(machine, []), fn(acc, event) {
    let #(machine, more) = arrival.step(acc.0, event)
    #(machine, list.append(acc.1, more))
  })
}

pub fn a_partial_arrival_whose_abort_was_lost_asks_and_discards_test() {
  let #(machine, effects) = receiving()
  let #(machine, asked) = fire_timers(machine, effects)
  assert list.any(asked, fn(effect) {
    case effect {
      arrival.ToHome(message.AskMoveStatus(..)) -> True
      _ -> False
    }
  })
  let #(machine, _) =
    arrival.step(
      machine,
      arrival.FromHome(message.MoveStatus(
        arrival.move_id(machine),
        message.VerdictAborted,
      )),
    )
  assert arrival.phase(machine) == arrival.Finished(arrival.Discarded)
}

fn unknown(machine: Arrival) -> Arrival {
  arrival.step(
    machine,
    arrival.FromHome(message.MoveStatus(
      arrival.move_id(machine),
      message.VerdictUnknown,
    )),
  ).0
}

pub fn a_partial_arrival_the_home_no_longer_knows_is_discarded_test() {
  let #(machine, _) = receiving()
  let machine = unknown(unknown(machine))
  assert arrival.phase(machine) == arrival.Receiving
  let machine = unknown(machine)
  assert arrival.phase(machine) == arrival.Finished(arrival.Discarded)
}

pub fn a_chunk_between_unknown_answers_keeps_the_arrival_test() {
  let #(machine, _) = receiving()
  let machine = unknown(unknown(machine))
  let assert Ok(at) = epoch.from_int(6)
  let #(machine, _) =
    arrival.step(
      machine,
      arrival.FromHome(
        message.Install(
          arrival.move_id(machine),
          arrival.key(machine),
          at,
          1,
          blob.Tail,
          1,
          3,
          <<2>>,
        ),
      ),
    )
  let machine = unknown(unknown(machine))
  assert arrival.phase(machine) == arrival.Receiving
}
