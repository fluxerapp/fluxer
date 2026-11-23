// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings.{
  type Durations, Durations, SessionDurations,
}
import fluxer_gateway/core/session
import fluxer_gateway/session/links
import fluxer_gateway/session/manager/names
import fluxer_gateway/session/manager/registry
import fluxer_gateway/session/manager/shards
import gleam/erlang/process.{type Pid}
import gleam/json
import gleam/option.{Some}
import gleeunit/should
import support/isolation
import support/session_fake_guild as fake_guild
import support/session_harness as h

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
        links_base_sleep_ms: 50,
        links_cap_sleep_ms: 200,
      ),
    )
  })
}

fn shard_pid(ref: session.SessionRef) -> Pid {
  let key = session.session_id_to_string(ref.id)
  let assert Ok(pid) = process.named(names.shard(shards.index(key)))
  pid
}

fn is_linked(ref: session.SessionRef, guild: Int) -> Bool {
  case links.get(h.state_of(ref).links, h.guild_id(guild)) {
    Some(links.Linked(..)) -> True
    _ -> False
  }
}

pub fn killed_session_gives_back_join_hold_and_user_slot_test() {
  fast_joins()
  let user = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(h.session_api(h.session_data(user, False, [guild])))
  let #(ref, _socket, _) = h.ready_session("tok-q1")
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  h.join_quota(guild) |> should.equal(1)
  h.user_sessions(user) |> should.equal(1)

  process.kill(h.session_pid(ref))
  h.wait_until(
    fn() { h.join_quota(guild) == 0 && h.user_sessions(user) == 0 },
    2000,
  )
  |> should.be_true
  registry.lookup(session.session_id_to_string(ref.id))
  |> should.equal(Error(Nil))
  fake_guild.crash(fake)
}

pub fn killed_shard_gives_back_every_slot_and_starts_again_test() {
  fast_joins()
  let user = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(h.session_api(h.session_data(user, False, [guild])))
  let #(ref, _socket, _) = h.ready_session("tok-q2")
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  let key = session.session_id_to_string(ref.id)
  let shard = shard_pid(ref)
  let pid = h.session_pid(ref)

  process.kill(shard)
  h.await_exit(pid, 2000) |> should.be_true
  h.wait_until(
    fn() {
      h.join_quota(guild) == 0
      && h.user_sessions(user) == 0
      && registry.lookup(key) == Error(Nil)
      && case process.named(names.shard(shards.index(key))) {
        Ok(restarted) -> restarted != shard
        Error(Nil) -> False
      }
    },
    3000,
  )
  |> should.be_true

  let #(again, _, _) = h.ready_session("tok-q2")
  h.user_sessions(user) |> should.equal(1)
  h.terminate(again)
  h.wait_until(fn() { h.user_sessions(user) == 0 }, 2000) |> should.be_true
  fake_guild.crash(fake)
}

pub fn shard_dying_during_the_start_fetch_leaks_no_user_slot_test() {
  let user = h.unique_user()
  let fetching = process.new_subject()
  let responder = fn(kind, _body) {
    case kind {
      "session" -> {
        process.send(fetching, Nil)
        process.sleep(300)
        h.ok_data(h.session_data(user, False, []))
      }
      _ -> h.ok_data(json.object([]))
    }
  }
  use <- h.with_api(responder)
  let socket = h.fake_socket()
  let session_id = session.new_session_id()
  let key = session.session_id_to_string(session_id)
  let assert Ok(shard) = process.named(names.shard(shards.index(key)))
  let reply = process.new_subject()
  shards.send(
    key,
    shards.StartSession(
      session_id,
      h.identify("tok-q3"),
      session.Peer(ip: "127.0.0.1", api_version: 1),
      socket.ref,
      reply,
      process.new_subject(),
    ),
  )
  |> should.equal(Ok(Nil))
  process.receive(fetching, 3000) |> should.equal(Ok(Nil))
  process.kill(shard)
  process.sleep(700)
  h.user_sessions(user) |> should.equal(0)
  registry.lookup(key) |> should.equal(Error(Nil))
}

pub fn guild_crash_during_a_join_frees_the_slot_and_rejoins_test() {
  fast_joins()
  let user = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(h.session_api(h.session_data(user, False, [guild])))
  let #(ref, socket, _) = h.ready_session("tok-q4")
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  h.join_quota(guild) |> should.equal(1)

  fake_guild.crash(fake)
  let restarted = fake_guild.start(guild, fake_guild.Accept)
  let assert Ok(_) = fake_guild.joins(restarted, 4000)
  h.wait_until(fn() { is_linked(ref, guild) }, 2000) |> should.be_true
  h.join_quota(guild) |> should.equal(0)
  h.await_event(socket, "GUILD_CREATE", 2000) |> should.be_ok
  h.terminate(ref)
  fake_guild.crash(restarted)
}

pub fn killed_router_restarts_the_tree_and_frees_every_slot_test() {
  fast_joins()
  let user = h.unique_user()
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(h.session_api(h.session_data(user, False, [guild])))
  let #(ref, _socket, _) = h.ready_session("tok-q5")
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  let pid = h.session_pid(ref)
  let assert Ok(router) = process.named(names.router())

  process.kill(router)
  h.await_exit(pid, 3000) |> should.be_true
  h.wait_until(
    fn() {
      h.join_quota(guild) == 0
      && h.user_sessions(user) == 0
      && case process.named(names.router()) {
        Ok(restarted) -> restarted != router
        Error(Nil) -> False
      }
    },
    3000,
  )
  |> should.be_true

  let #(again, _, _) = h.ready_session("tok-q5")
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  h.join_quota(guild) |> should.equal(1)
  h.user_sessions(user) |> should.equal(1)
  h.terminate(again)
  h.wait_until(
    fn() { h.join_quota(guild) == 0 && h.user_sessions(user) == 0 },
    2000,
  )
  |> should.be_true
  fake_guild.crash(fake)
}
