// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{Durations, SessionDurations}
import fluxer_gateway/core/node
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, Dispatch}
import fluxer_gateway/session/control
import fluxer_gateway/session/server
import fluxer_gateway/session/state
import gleam/erlang/process
import gleam/json
import gleam/option.{None}
import support/isolation
import support/session_harness as h
import support/session_transfer_tap as tap
import support/wait

fn short_arrival() -> Nil {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        server_move_relay_ms: 200,
        server_rebind_ack_wait_ms: 100,
      ),
    )
  })
}

fn adopt(found) -> SessionRef {
  let reply = process.new_subject()
  process.spawn_unlinked(fn() {
    let assert Ok(started) =
      server.start(server.Start(seed: found, socket: None, reply: None))
    process.send(reply, started.data)
  })
  let assert Ok(ref) = process.receive(reply, 5000)
  ref
}

fn late() -> session.SessionInbox {
  Dispatch(
    "MESSAGE_CREATE",
    raw_json.from_json(json.object([#("content", json.string("late"))])),
  )
}

pub fn a_relay_that_arrives_after_the_arrival_expired_is_not_applied_test() {
  short_arrival()
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, _, _) = h.ready_session("tok-expiry")
  let transfer = tap.start()
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  let assert Ok(#(exported, _commit)) = tap.committing(transfer, 5000)
  let target = adopt(exported)
  assert h.state_of(target).arrival != state.Settled
  assert wait.until(3000, fn() {
    case h.state_of(target).arrival {
      state.Arriving(..) -> False
      _ -> True
    }
  })
  let before = h.state_of(target).seq
  control.send(target, control.Relayed(late()))
  process.sleep(200)
  assert h.state_of(target).seq == before
  tap.release(transfer)
  h.terminate(target)
  h.terminate(ref)
}
