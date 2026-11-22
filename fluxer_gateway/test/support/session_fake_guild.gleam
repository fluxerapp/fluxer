// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{
  type GuildRequest, type JoinReply, type JoinRequest, GuildRef, Join, Joined,
}
import fluxer_gateway/core/guild_create as created
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/manager/registry
import gleam/erlang/process.{type Pid, type Subject}
import gleam/int
import gleam/json

pub type Answer {
  Accept
  Hold
  Answer(fn(JoinRequest) -> JoinReply)
  Flaky(retry_after_ms: Int, times: Int)
  AcceptWith(guild_create: raw_json.RawJson)
  SlowWith(delay_ms: Int, guild_create: raw_json.RawJson)
  LateWith(retry_after_ms: Int, guild_create: raw_json.RawJson)
}

pub type FakeGuild {
  FakeGuild(guild: Int, pid: Pid, seen: Subject(GuildRequest))
}

pub fn guild_create(guild: Int) -> raw_json.RawJson {
  raw_json.from_json(
    json.object([
      #("id", json.string(int.to_string(guild))),
      #("name", json.string("g" <> int.to_string(guild))),
    ]),
  )
}

pub fn start(guild: Int, answer: Answer) -> FakeGuild {
  let seen = process.new_subject()
  let ready = process.new_subject()
  let assert Ok(guild_id) = id.guild_id(guild)
  let pid =
    process.spawn_unlinked(fn() {
      let inbox_subject = process.new_subject()
      let control = process.new_subject()
      let ref = GuildRef(guild: guild_id, inbox: inbox_subject)
      registry.put(guild_id, inbox.Handle(ref:, control:, pid: process.self()))
      process.send(ready, Nil)
      serve(ref, inbox_subject, answer, seen)
    })
  let assert Ok(Nil) = process.receive(ready, 1000)
  FakeGuild(guild:, pid:, seen:)
}

fn serve(
  ref: guild.GuildRef,
  inbox_subject: Subject(GuildRequest),
  answer: Answer,
  seen: Subject(GuildRequest),
) -> Nil {
  let request = process.receive_forever(inbox_subject)
  process.send(seen, request)
  let next = case request {
    Join(join, reply) -> respond(ref, answer, join, reply)
    guild.SessionMoved(_, from) -> {
      session.send(from, session.GuildRebound(ref.guild, True))
      answer
    }
    _ -> answer
  }
  serve(ref, inbox_subject, next, seen)
}

fn respond(
  ref: guild.GuildRef,
  answer: Answer,
  join: JoinRequest,
  reply: Subject(JoinReply),
) -> Answer {
  case answer {
    Accept -> {
      accept(ref, join, reply)
      Accept
    }
    Hold -> Hold
    Answer(decide) -> {
      process.send(reply, decide(join))
      answer
    }
    Flaky(retry_after_ms, times) if times > 0 -> {
      process.send(reply, guild.JoinUnavailable(retry_after_ms, join.attempt))
      Flaky(retry_after_ms, times - 1)
    }
    Flaky(..) -> {
      accept(ref, join, reply)
      Accept
    }
    AcceptWith(guild_create) -> {
      process.send(reply, joined(ref, guild_create, join))
      answer
    }
    SlowWith(delay_ms, guild_create) -> {
      process.sleep(delay_ms)
      process.send(reply, joined(ref, guild_create, join))
      AcceptWith(guild_create)
    }
    LateWith(retry_after_ms, guild_create) -> {
      process.send(reply, guild.JoinUnavailable(retry_after_ms, join.attempt))
      AcceptWith(guild_create)
    }
  }
}

fn accept(ref: guild.GuildRef, join: JoinRequest, reply: Subject(JoinReply)) {
  process.send(
    reply,
    joined(ref, guild_create(id.guild_id_to_int(ref.guild)), join),
  )
}

fn joined(
  ref: guild.GuildRef,
  wire: raw_json.RawJson,
  join: JoinRequest,
) -> JoinReply {
  Joined(ref, created.from_wire(ref.guild, wire), join.attempt)
}

pub fn crash(fake: FakeGuild) -> Nil {
  process.kill(fake.pid)
}

pub fn joins(fake: FakeGuild, timeout_ms: Int) -> Result(JoinRequest, Nil) {
  case process.receive(fake.seen, timeout_ms) {
    Ok(Join(request, _)) -> Ok(request)
    Ok(_) -> joins(fake, timeout_ms)
    Error(Nil) -> Error(Nil)
  }
}
