// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/handoff/source.{type Effect, type Source}
import gleam/erlang/atom
import gleam/list

fn sealed() -> #(Source, List(Effect)) {
  let assert Ok(at) = epoch.from_int(5)
  let move =
    MoveId(source: WorkerId(atom.create("source_test@test"), 1), serial: 1)
  let machine =
    source.new(
      move:,
      key: Key(ns: 0, id: 1),
      epoch: at,
      to: WorkerId(atom.create("source_test_target@test"), 1),
      version: 1,
      estimate: 0,
      timings: source.default_timings(),
      precopy: False,
    )
  [
    source.Begin,
    source.FromHome(message.MoveAccepted(move)),
    source.FreezeReady(Ok(<<1, 2, 3>>)),
    source.Forwarded(<<"a">>),
    source.FromHome(message.ChunkAck(move, blob.Tail, 0)),
    source.FromHome(message.Committed(move, <<"ref">>, epoch.next(at))),
    source.Repointed,
  ]
  |> list.fold(#(machine, []), fn(acc, event) { source.step(acc.0, event, 0) })
}

fn seals(effects: List(Effect)) -> List(Int) {
  list.filter_map(effects, fn(effect) {
    case effect {
      source.ToHome(message.SealSource(last_seq:, ..)) -> Ok(last_seq)
      _ -> Error(Nil)
    }
  })
}

pub fn a_sealed_source_seals_again_at_a_new_home_test() {
  let #(machine, _) = sealed()
  let assert source.Sealed(..) = source.phase(machine)
  let #(_, effects) = source.step(machine, source.HomeChanged, 10)
  assert seals(effects) == [1]
  assert list.contains(
    effects,
    source.ToHome(message.Forward(source.move_id(machine), 1, <<"a">>)),
  )
}

fn timers(effects: List(Effect)) -> List(source.Event) {
  list.filter_map(effects, fn(effect) {
    case effect {
      source.After(timer, _) -> Ok(source.Fired(timer))
      _ -> Error(Nil)
    }
  })
}

fn fire_until_quiet(
  machine: Source,
  effects: List(Effect),
  left: Int,
) -> Source {
  case timers(effects), left > 0 {
    [], _ | _, False -> machine
    events, True -> {
      let #(machine, more) =
        list.fold(events, #(machine, []), fn(acc, event) {
          let #(machine, more) = source.step(acc.0, event, 100_000)
          #(machine, list.append(acc.1, more))
        })
      fire_until_quiet(machine, more, left - 1)
    }
  }
}

pub fn a_sealed_source_that_never_hears_applied_finishes_moved_test() {
  let #(machine, effects) = sealed()
  assert seals(effects) == [1]
  let machine = fire_until_quiet(machine, effects, 100)
  let assert source.Finished(source.Moved(..)) = source.phase(machine)
}

pub fn a_sealed_source_seals_again_while_it_waits_for_applied_test() {
  let #(machine, effects) = sealed()
  let #(machine, again) =
    list.fold(timers(effects), #(machine, []), fn(acc, event) {
      let #(machine, more) = source.step(acc.0, event, 100)
      #(machine, list.append(acc.1, more))
    })
  let assert source.Sealed(..) = source.phase(machine)
  assert seals(again) == [1]
}
