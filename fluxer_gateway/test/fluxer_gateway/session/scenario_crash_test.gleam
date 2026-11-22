// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/session.{GuildDown}
import fluxer_gateway/presence/client
import fluxer_gateway/session/links
import fluxer_gateway/session/state.{ConnectGuild, PresenceUp}
import gleam/dynamic/decode
import gleam/erlang/process.{type Pid}
import gleam/int
import gleam/json
import gleam/option.{Some}
import gleam/result
import gleeunit/should
import support/session_fake_guild as fake_guild
import support/session_harness.{Frame} as h

fn presence_pid(ref: session.SessionRef) -> Result(Pid, Nil) {
  case h.state_of(ref).presence {
    PresenceUp(link, Some(_)) -> client.link_pid(link)
    _ -> Error(Nil)
  }
}

fn is_linked(ref: session.SessionRef, guild: Int) -> Bool {
  case links.get(h.state_of(ref).links, h.guild_id(guild)) {
    Some(links.Linked(_, _)) -> True
    _ -> False
  }
}

pub fn crashed_presence_is_relinked_test() {
  use <- h.with_api(h.session_api(h.session_data(h.unique_user(), False, [])))
  let #(ref, socket, _) = h.ready_session("tok-p1")
  let assert Ok(first) = presence_pid(ref)
  process.kill(first)
  h.wait_until(
    fn() {
      case presence_pid(ref) {
        Ok(pid) -> pid != first && process.is_alive(pid)
        Error(Nil) -> False
      }
    },
    3000,
  )
  |> should.be_true
  h.drop_socket(socket)
  h.terminate(ref)
}

pub fn crashed_guild_goes_unavailable_and_comes_back_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let #(ref, socket, _) = h.ready_session("tok-g1")
  is_linked(ref, guild) |> should.be_true
  fake_guild.crash(fake)
  let assert Ok(Frame(_, "GUILD_DELETE", deleted)) =
    h.await_event(socket, "GUILD_DELETE", 2000)
  decode.run(deleted, decode.at(["unavailable"], decode.bool))
  |> should.equal(Ok(True))
  decode.run(deleted, decode.at(["id"], decode.string))
  |> should.equal(Ok(int.to_string(guild)))
  is_linked(ref, guild) |> should.be_false

  let back = fake_guild.start(guild, fake_guild.Accept)
  process.send(h.state_of(ref).internal, ConnectGuild(h.guild_id(guild), 0))
  let assert Ok(Frame(_, "GUILD_CREATE", created)) =
    h.await_event(socket, "GUILD_CREATE", 2000)
  decode.run(created, decode.at(["id"], decode.string))
  |> should.equal(Ok(int.to_string(guild)))
  is_linked(ref, guild) |> should.be_true
  h.terminate(ref)
  fake_guild.crash(back)
}

pub fn guild_down_notice_relinks_quietly_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let #(ref, socket, _) = h.ready_session("tok-g2")
  let assert Ok(_) = fake_guild.joins(fake, 1000)
  session.send(ref, GuildDown(h.guild_id(guild), 0))
  let assert Ok(rejoin) = fake_guild.joins(fake, 2000)
  rejoin.class |> should.equal(h.resume_class())
  h.await_event(socket, "GUILD_DELETE", 300) |> should.be_error
  h.wait_until(fn() { is_linked(ref, guild) }, 1000) |> should.be_true
  h.terminate(ref)
  fake_guild.crash(fake)
}

pub fn silent_guild_does_not_hold_ready_past_deadline_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-g3"), socket)
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 9000)
  decode.run(
    ready,
    decode.at(["guilds"], decode.list(decode.at(["unavailable"], decode.bool))),
  )
  |> should.equal(Ok([True]))
  h.terminate(ref)
  fake_guild.crash(fake)
}

pub fn guild_crash_during_join_still_reaches_ready_test() {
  let guild = h.unique_user()
  let fake = fake_guild.start(guild, fake_guild.Hold)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild])),
  )
  let socket = h.fake_socket()
  let assert Ok(ref) = h.start_session(h.identify("tok-g4"), socket)
  let assert Ok(_) = fake_guild.joins(fake, 2000)
  fake_guild.crash(fake)
  let back = fake_guild.start(guild, fake_guild.Accept)
  let assert Ok(Frame(_, "READY", ready)) = h.await_event(socket, "READY", 9000)
  decode.run(
    ready,
    decode.at(["guilds"], decode.list(decode.at(["id"], decode.string))),
  )
  |> should.equal(Ok([int.to_string(guild)]))
  h.terminate(ref)
  fake_guild.crash(back)
}

fn lazy_request(guild: Int) -> session.ClientOp {
  let assert Ok(data) =
    json.parse(
      json.to_string(
        json.object([
          #(
            "subscriptions",
            json.object([
              #(
                int.to_string(guild),
                json.object([
                  #("active", json.bool(True)),
                  #("typing", json.bool(True)),
                ]),
              ),
            ]),
          ),
        ]),
      ),
      decode.dynamic,
    )
  session.LazyRequest(data)
}

fn await_lazy(fake: fake_guild.FakeGuild) -> Result(Bool, Nil) {
  case process.receive(fake.seen, 2000) {
    Ok(guild.SessionOp(_, session.LazyRequest(data))) ->
      decode.run(
        data,
        decode.at(
          ["subscriptions", int.to_string(fake.guild), "typing"],
          decode.bool,
        ),
      )
      |> result.replace_error(Nil)
    Ok(_) -> await_lazy(fake)
    Error(Nil) -> Error(Nil)
  }
}

pub fn subscriptions_follow_guild_through_crash_test() {
  let guild_id = h.unique_user()
  let fake = fake_guild.start(guild_id, fake_guild.Accept)
  use <- h.with_api(
    h.session_api(h.session_data(h.unique_user(), False, [guild_id])),
  )
  let #(ref, socket, _) = h.ready_session("tok-g5")
  session.send(ref, session.SocketOp(lazy_request(guild_id)))
  await_lazy(fake) |> should.equal(Ok(True))
  fake_guild.crash(fake)
  let assert Ok(_) = h.await_event(socket, "GUILD_DELETE", 2000)
  let back = fake_guild.start(guild_id, fake_guild.Accept)
  process.send(h.state_of(ref).internal, ConnectGuild(h.guild_id(guild_id), 0))
  let assert Ok(rejoin) = fake_guild.joins(back, 2000)
  rejoin.class |> should.equal(h.resume_class())
  await_lazy(back) |> should.equal(Ok(True))
  h.terminate(ref)
  fake_guild.crash(back)
}
