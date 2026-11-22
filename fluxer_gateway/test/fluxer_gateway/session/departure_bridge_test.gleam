// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionInbox, Dispatch}
import fluxer_gateway/session/control.{type Control}
import fluxer_gateway/session/departure
import fluxer_gateway/session/state.{Detached, PresenceDown}
import gleam/erlang/process.{type Subject}
import gleam/json
import gleam/list
import support/session_harness as h

fn message(content: String) -> raw_json.RawJson {
  raw_json.from_json(json.object([#("content", json.string(content))]))
}

fn api() -> h.Responder {
  h.session_api(h.session_data(h.unique_user(), False, []))
}

type Seen {
  Seen(inbox: List(SessionInbox), controls: List(Control))
}

fn depart(speaks_moves: Bool) -> Seen {
  depart_with(speaks_moves, fn(_) { Nil })
}

fn depart_with(
  speaks_moves: Bool,
  extra: fn(session.SessionRef) -> Nil,
) -> Seen {
  use <- h.with_api(api())
  let #(ref, _, _) = h.ready_session("tok-bridge")
  let live = h.state_of(ref)
  let target_inbox: Subject(SessionInbox) = process.new_subject()
  let seen = process.new_subject()
  let mover =
    process.spawn(fn() {
      let me = session.SessionRef(..live.me, inbox: process.new_subject())
      let target = session.SessionRef(..live.me, inbox: target_inbox)
      let moving =
        state.State(..live, me:, socket: Detached(0), presence: PresenceDown)
      session.send(me, Dispatch("MESSAGE_CREATE", message("late")))
      control.send(me, control.PresenceRejoinCheck)
      extra(me)
      departure.run(moving, target, speaks_moves)
      process.send(seen, Nil)
    })
  let assert Ok(Nil) = process.receive(seen, 15_000)
  h.terminate(ref)
  let _ = mover
  Seen(
    inbox: drain(target_inbox, []),
    controls: drain(control.subject(process.self()), []),
  )
}

fn drain(inbox: Subject(a), found: List(a)) -> List(a) {
  case process.receive(inbox, 100) {
    Ok(message) -> drain(inbox, [message, ..found])
    Error(Nil) -> list.reverse(found)
  }
}

pub fn a_move_to_an_epoch_four_target_forwards_raw_messages_test() {
  let seen = depart(False)
  assert seen.inbox == [Dispatch("MESSAGE_CREATE", message("late"))]
  assert seen.controls == [control.PresenceRejoinCheck]
}

pub fn a_move_to_a_current_target_relays_and_reports_done_test() {
  let seen = depart(True)
  assert seen.inbox == []
  assert seen.controls
    == [
      control.Relayed(Dispatch("MESSAGE_CREATE", message("late"))),
      control.RelayedControl(control.PresenceRejoinCheck),
      control.MoveDone(guilds: [], presence: True),
    ]
}

pub fn a_resume_during_departure_reaches_the_target_test() {
  let reply = process.new_subject()
  let socket =
    session.SocketRef(pid: process.self(), outbox: process.new_subject())
  let resume = control.Resume("tok-bridge", 1, socket, reply)
  let seen = depart_with(True, fn(me) { control.send(me, resume) })
  assert list.contains(seen.controls, control.RelayedControl(resume))
  let seen = depart_with(False, fn(me) { control.send(me, resume) })
  assert list.contains(seen.controls, resume)
}

pub fn a_socket_reattaching_to_a_session_held_by_another_socket_is_told_to_reconnect_test() {
  use <- h.with_api(api())
  let #(ref, _socket, seq) = h.ready_session("tok-late-reattach")
  let outbox = process.new_subject()
  let late = session.SocketRef(pid: process.self(), outbox:)
  control.send(ref, control.Reattach(late, seq))
  let assert Ok(session.Send(frames: frames, ..)) =
    process.receive(outbox, 2000)
  assert frames == [session.ReconnectFrame]
  h.terminate(ref)
}
