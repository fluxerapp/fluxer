// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/epoch
import bouncer_proto/key.{Key}
import bouncer_proto/message.{MoveId}
import bouncer_proto/worker.{WorkerId}
import fluxer_gateway/config/settings.{Durations}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/handoff/move
import fluxer_gateway/presence/client
import fluxer_gateway/presence/handoff/adapter as presence_adapter
import fluxer_gateway/presence/protocol
import fluxer_gateway/session/state
import gleam/erlang/atom
import gleam/erlang/process.{type Pid}
import gleam/json
import support/isolation
import support/presence_scenario as presence
import support/session_harness as h

fn linked_to(ref: SessionRef) -> Result(Pid, Nil) {
  case h.state_of(ref).presence {
    state.PresenceUp(link, _) -> client.link_pid(link)
    _ -> Error(Nil)
  }
}

fn at(value: Int) -> epoch.Epoch {
  let assert Ok(found) = epoch.from_int(value)
  found
}

fn sink(seen: process.Subject(move.Command)) -> Pid {
  let ready = process.new_subject()
  let pid =
    process.spawn_unlinked(fn() {
      claim_engine_name()
      process.send(ready, Nil)
      relay(process.named_subject(move.engine_name()), seen)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  pid
}

fn claim_engine_name() -> Nil {
  case process.register(process.self(), move.engine_name()) {
    Ok(Nil) -> Nil
    Error(Nil) -> {
      process.sleep(5)
      claim_engine_name()
    }
  }
}

fn relay(
  inbox: process.Subject(move.Command),
  seen: process.Subject(move.Command),
) -> Nil {
  process.send(seen, process.receive_forever(inbox))
  relay(inbox, seen)
}

pub fn a_moved_presence_rebinds_its_session_link_without_reconnecting_test() {
  isolation.set_durations(fn(d) {
    Durations(
      ..d,
      presence: settings.PresenceDurations(
        ..d.presence,
        server_seal_wait_ms: 60_000,
        server_forward_grace_ms: 300,
      ),
    )
  })
  let user = h.unique_user()
  use <- h.with_api(
    h.session_api(
      h.session_data_with(user, False, [], [
        #("auth_session_id_hash", json.string("aGFzaA")),
      ]),
    ),
  )
  let #(ref, _socket, _) = h.ready_session("tok-pr1")
  assert h.wait_until(fn() { linked_to(ref) != Error(Nil) }, 3000)
  let assert Ok(source) = linked_to(ref)
  let seen = process.new_subject()
  let sink_pid = sink(seen)
  let entity = Key(key.presence, user)
  let move_id =
    MoveId(
      source: WorkerId(
        node: atom.create("presence_rebind_source"),
        incarnation: 1,
      ),
      serial: 1,
    )
  let assert Ok(frozen) = presence_adapter.freeze(entity, move_id, 1, at(3))
  assert frozen.stub == source
  let assert Ok(arrival) =
    presence_adapter.install(entity, move_id.source, at(4), frozen.tail)
  let assert Ok(Nil) =
    presence_adapter.committed(frozen.stub, arrival.target_ref, at(4))
  let assert Ok(move.Repointed(..)) = process.receive(seen, 3000)
  let assert Ok(Nil) = presence_adapter.seal(arrival.entity, 0)
  assert linked_to(ref) == Ok(arrival.entity)
  assert presence.wait_down(source, 2000)
  process.sleep(100)
  assert linked_to(ref) == Ok(arrival.entity)
  let assert Ok(inbox) = presence.presence_inbox(user)
  let reply = process.new_subject()
  process.send(inbox, protocol.TerminateSessions(["aGFzaA"], reply))
  assert process.receive(reply, 3000) == Ok(protocol.TerminateOk)
  assert h.await_exit(h.session_pid(ref), 3000)
  process.kill(sink_pid)
}
