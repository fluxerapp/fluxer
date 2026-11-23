// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/message.{type MoveId, MoveId}
import bouncer_proto/worker.{type WorkerId, WorkerId}
import fluxer_gateway/handoff/move
import gleam/erlang/atom
import gleam/erlang/process.{type Pid, type Subject}
import gleam/list

pub type Sink {
  Sink(pid: Pid, seen: Subject(move.Command))
}

pub fn start() -> Sink {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      claim()
      process.send(ready, Nil)
      relay(process.named_subject(move.engine_name()), seen)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  Sink(pid:, seen:)
}

fn claim() -> Nil {
  case process.register(process.self(), move.engine_name()) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.sleep(5)
      claim()
    }
  }
}

fn relay(inbox: Subject(move.Command), seen: Subject(move.Command)) -> Nil {
  process.send(seen, process.receive_forever(inbox))
  relay(inbox, seen)
}

pub fn stop(sink: Sink) -> Nil {
  let monitor = process.monitor(sink.pid)
  process.kill(sink.pid)
  let _ =
    process.new_selector()
    |> process.select_specific_monitor(monitor, fn(_) { Nil })
    |> process.selector_receive(1000)
  Nil
}

pub fn source() -> WorkerId {
  WorkerId(node: atom.create("handoff_move_source"), incarnation: 1)
}

pub fn move_id(serial: Int) -> MoveId {
  MoveId(source: source(), serial:)
}

pub fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

pub fn forwarded_until_sealed(sink: Sink) -> List(#(Int, BitArray)) {
  collect(sink, [])
}

fn collect(sink: Sink, acc: List(#(Int, BitArray))) -> List(#(Int, BitArray)) {
  let assert Ok(found) = process.receive(sink.seen, 3000)
  case found {
    move.Forwarded(bytes:, ..) ->
      collect(sink, [#(list.length(acc) + 1, bytes), ..acc])
    move.Repointed(..) -> list.reverse(acc)
    _ -> collect(sink, acc)
  }
}

pub fn next_forwarded(sink: Sink) -> BitArray {
  let assert Ok(found) = process.receive(sink.seen, 3000)
  case found {
    move.Forwarded(bytes:, ..) -> bytes
    _ -> next_forwarded(sink)
  }
}
