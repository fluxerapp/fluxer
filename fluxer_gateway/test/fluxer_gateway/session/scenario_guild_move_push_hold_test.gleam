// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/key.{Key}
import fluxer_gateway/core/id
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/guild/handoff/adapter as guild_adapter
import fluxer_gateway/guild/manager/registry as guild_registry
import fluxer_gateway/session/manager
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import support/guild_engagement
import support/guild_fixture as fixture
import support/move_sink as sink
import support/session_harness as h
import support/session_move as move
import support/wait

fn start_invisible(sock: move.Sock, token: String) -> SessionRef {
  let reply = process.new_subject()
  manager.start(
    session.Identify(
      ..h.identify(token),
      presence: Some(
        fixture.to_dynamic(json.object([#("status", json.string("invisible"))])),
      ),
    ),
    session.Peer(ip: "127.0.0.1", api_version: 1),
    sock.ref,
    reply,
  )
  let assert Ok(Ok(ref)) = process.receive(reply, 15_000)
  ref
}

pub fn a_moved_guild_keeps_the_push_hold_of_an_invisible_desktop_session_test() {
  let user = h.unique_user()
  let found = move.guild(user)
  use <- h.with_api(move.responder(user, [found]))
  let sock = move.socket()
  let ref = start_invisible(sock, "tok-hold-move")
  let assert Ok(guild) = id.guild_id(found.id)
  assert wait.until(8000, fn() {
    move.ready_seq(move.seen(sock)) != option.None
  })
  let assert Ok(source) = guild_registry.lookup(guild)
  assert wait.until(3000, fn() { guild_engagement.engaged(source.pid, user) })
  let moves = sink.start()
  let assert Ok(frozen) =
    guild_adapter.freeze(
      Key(key.guild, found.id),
      sink.move_id(31),
      2,
      sink.at(5),
    )
  let assert Ok(arrival) =
    guild_adapter.install(
      Key(key.guild, found.id),
      sink.source(),
      sink.at(6),
      frozen.tail,
    )
  let assert Ok(Nil) =
    guild_adapter.committed(frozen.stub, arrival.target_ref, sink.at(6))
  let entries = sink.forwarded_until_sealed(moves)
  list.each(entries, fn(entry) {
    let assert Ok(Nil) = guild_adapter.release(arrival.entity, entry.0, entry.1)
  })
  let assert Ok(Nil) = guild_adapter.seal(arrival.entity, list.length(entries))
  assert wait.until(3000, fn() {
    guild_engagement.engaged(arrival.entity, user)
  })
  sink.stop(moves)
  h.terminate(ref)
  fixture.stop(found)
}
