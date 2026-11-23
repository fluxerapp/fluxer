// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, SessionDurations,
}
import fluxer_gateway/core/guild
import fluxer_gateway/core/node
import fluxer_gateway/core/session.{type SessionRef, ReconnectFrame}
import fluxer_gateway/presence/protocol
import fluxer_gateway/session/control
import fluxer_gateway/session/links
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import fluxer_gateway/session/seed.{type Seed}
import fluxer_gateway/session/state
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{Some}
import gleeunit/should
import support/isolation
import support/presence_scenario as presence
import support/session_fake_guild as fake_guild
import support/session_harness.{Other} as h
import support/session_transfer_tap as tap

fn fast_joins() -> Nil {
  isolation.set_durations(fn(d: Durations) {
    Durations(
      ..d,
      session: SessionDurations(
        ..d.session,
        server_ready_deadline_ms: 300,
        guilds_first_join_floor_ms: 300,
        guilds_join_grace_ms: 200,
        guilds_after_ready_join_ms: 300,
        guilds_down_reconnect_ms: 60_000,
        links_base_sleep_ms: 50,
        links_cap_sleep_ms: 200,
      ),
    )
  })
}

fn is_linked(ref: SessionRef, guild: Int) -> Bool {
  case links.get(h.state_of(ref).links, h.guild_id(guild)) {
    Some(links.Linked(..)) -> True
    _ -> False
  }
}

fn hand_off_and_capture(ref: SessionRef, socket: h.FakeSocket) -> Seed {
  let source = h.session_pid(ref)
  let transfer = tap.start()
  let answer = process.new_subject()
  control.send(ref, control.HandOff(node.self(), answer))
  let assert Ok(#(exported, adopted)) = tap.committing(transfer, 2000)
  process.send(
    adopted,
    Ok(session.SessionRef(..ref, inbox: process.new_subject())),
  )
  process.receive(answer, 2000) |> should.equal(Ok(True))
  h.await_frame(socket, 2000, fn(frame) { frame == h.Rebound(exported.seq) })
  |> should.be_ok
  h.collect(socket, 50)
  |> list.contains(Other(ReconnectFrame))
  |> should.be_false
  h.await_exit(source, 2000) |> should.be_true
  tap.release(transfer)
  h.wait_until(
    fn() { registry.lookup(session.session_id_to_string(ref.id)) == Error(Nil) },
    2000,
  )
  |> should.be_true
  exported
}

fn adopt(exported: Seed) -> SessionRef {
  let reply = process.new_subject()
  shards.send(
    session.session_id_to_string(exported.id),
    shards.Adopt(exported, reply),
  )
  |> should.equal(Ok(Nil))
  let assert Ok(Ok(adopted)) = process.receive(reply, 2000)
  adopted
}

fn left(fake: fake_guild.FakeGuild) -> Bool {
  drain(fake.seen)
  |> list.any(fn(request) {
    case request {
      guild.Leave(..) -> True
      _ -> False
    }
  })
}

fn drain(subject: process.Subject(a)) -> List(a) {
  case process.receive(subject, 0) {
    Ok(item) -> [item, ..drain(subject)]
    Error(Nil) -> []
  }
}

pub fn hand_off_after_a_guild_crash_rejoins_every_guild_on_the_target_test() {
  fast_joins()
  let first = h.unique_user()
  let second = h.unique_user()
  let alive = fake_guild.start(first, fake_guild.Accept)
  let crashing = fake_guild.start(second, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [first, second])),
  )
  let #(ref, socket, ready_seq) = h.ready_session("tok-c1")
  h.wait_until(fn() { is_linked(ref, first) && is_linked(ref, second) }, 2000)
  |> should.be_true

  fake_guild.crash(crashing)
  h.wait_until(fn() { !is_linked(ref, second) }, 2000) |> should.be_true
  let exported = hand_off_and_capture(ref, socket)
  list.length(exported.guilds) |> should.equal(2)
  left(alive) |> should.be_false

  let restarted = fake_guild.start(second, fake_guild.Accept)
  let target = adopt(exported)
  h.wait_until(
    fn() { is_linked(target, first) && is_linked(target, second) },
    4000,
  )
  |> should.be_true
  left(alive) |> should.be_false

  let fresh = h.fake_socket()
  h.resume_id(session.session_id_to_string(ref.id), "tok-c1", ready_seq, fresh)
  |> should.equal(Ok(target))
  let replayed = h.collect(fresh, 300) |> h.events
  list.count(replayed, fn(event) { event == "GUILD_CREATE" }) |> should.equal(2)
  list.last(replayed) |> should.equal(Ok("RESUMED"))
  h.terminate(target)
  fake_guild.crash(alive)
  fake_guild.crash(restarted)
}

fn presence_linked(ref: SessionRef) -> Bool {
  case h.state_of(ref).presence {
    state.PresenceUp(..) -> True
    _ -> False
  }
}

fn auth_api(user: Int) -> h.Responder {
  h.session_api(
    h.session_data_with(user, False, [], [
      #("auth_session_id_hash", json.string("aGFzaA")),
    ]),
  )
}

fn terminate_through_presence(
  user: Int,
) -> Result(protocol.TerminateOutcome, Nil) {
  let assert Ok(inbox) = presence.presence_inbox(user)
  let reply = process.new_subject()
  process.send(inbox, protocol.TerminateSessions(["aGFzaA"], reply))
  process.receive(reply, 3000)
}

pub fn presence_crash_mid_hand_off_relinks_the_target_test() {
  let user = h.unique_user()
  use <- h.with_api(auth_api(user))
  let #(ref, socket, _) = h.ready_session("tok-c2")
  h.wait_until(fn() { presence_linked(ref) }, 3000) |> should.be_true
  let assert Ok(old_presence) = presence.presence_pid(user)
  let exported = hand_off_and_capture(ref, socket)
  process.kill(old_presence)
  h.await_exit(old_presence, 2000) |> should.be_true

  let target = adopt(exported)
  h.wait_until(fn() { presence_linked(target) }, 5000) |> should.be_true
  let assert Ok(fresh_presence) = presence.presence_pid(user)
  { fresh_presence != old_presence } |> should.be_true
  terminate_through_presence(user) |> should.equal(Ok(protocol.TerminateOk))
  h.await_exit(h.session_pid(target), 3000) |> should.be_true
}

pub fn presence_crash_after_the_target_linked_is_repaired_test() {
  let user = h.unique_user()
  use <- h.with_api(auth_api(user))
  let #(ref, socket, _) = h.ready_session("tok-c3")
  h.wait_until(fn() { presence_linked(ref) }, 3000) |> should.be_true
  let exported = hand_off_and_capture(ref, socket)
  let target = adopt(exported)
  h.wait_until(fn() { presence_linked(target) }, 5000) |> should.be_true
  let assert Ok(linked_presence) = presence.presence_pid(user)
  process.kill(linked_presence)
  h.wait_until(
    fn() {
      case presence.presence_pid(user) {
        Ok(pid) -> pid != linked_presence && presence_linked(target)
        Error(Nil) -> False
      }
    },
    5000,
  )
  |> should.be_true
  terminate_through_presence(user) |> should.equal(Ok(protocol.TerminateOk))
  h.await_exit(h.session_pid(target), 3000) |> should.be_true
}
