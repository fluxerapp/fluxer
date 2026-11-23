// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/voice/connection
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/geo
import fluxer_gateway/voice/guild_rpc
import fluxer_gateway/voice/guild_state.{State}
import fluxer_gateway/voice/store
import fluxer_gateway/voice/token
import gleam/dict
import gleam/erlang/process
import gleam/list
import gleam/option.{None, Some}
import support/voice_ids

fn guild_id() -> id.GuildId {
  let assert Ok(found) = id.guild_id(900)
  found
}

fn user_id(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn channel_id(value: Int) -> id.ChannelId {
  let assert Ok(found) = id.channel_id(value)
  found
}

fn context() -> guild_state.Ctx {
  let assert Ok(Nil) = nif.load()
  let #(_, reader) = perms.new(guild_id(), process.self())
  guild_state.Ctx(
    facts: guild.facts(
      id: guild_id(),
      owner: None,
      nsfw_level: guild.NsfwDefault,
      features: [],
      member_count: 0,
    ),
    reader:,
    member: fn(_) { None },
    channel: fn(_) { None },
    sessions_of: fn(_) { [] },
    now_ms: 1000,
    notify: fn(_) { Nil },
    notify_after: fn(_, _) { Nil },
  )
}

fn voice(user: id.UserId, channel: id.ChannelId, conn: String) -> entry.Entry {
  entry.new(
    user:,
    guild: Some(guild_id()),
    channel:,
    connection_id: voice_ids.conn(conn),
    session_id: Some("s1"),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
}

fn broadcasts_to(step: guild_state.Step, channel: id.ChannelId) -> Bool {
  list.any(step.effects, fn(effect) {
    case effect {
      guild_state.Broadcast(channel: Some(found), ..) -> found == channel
      _ -> False
    }
  })
}

pub fn leave_during_moderator_move_cancels_the_reissue_test() {
  let ctx = context()
  let user = user_id(1)
  let target = channel_id(20)
  let base = voice(user, channel_id(10), "c1")
  let job =
    guild_state.ReissueJob(
      user:,
      channel: target,
      session_id: Some("s1"),
      base:,
      geo: geo.unknown(),
      nonce: "n1",
      moved: True,
      suppress: None,
    )
  let fetch =
    guild_state.Fetch(
      job:,
      request: token.request(
        guild: Some(guild_id()),
        channel: target,
        user:,
        connection_id: Some(voice_ids.conn("c1")),
      ),
      started_at: 0,
    )
  let state =
    State(
      ..guild_state.new(guild_id()),
      fetches: dict.from_list([#("n1", fetch)]),
    )
  let step =
    connection.disconnect_user(guild_state.start(state), ctx, user, Some("s1"))
  assert dict.is_empty(step.state.fetches)
  assert broadcasts_to(step, target)
  let granted =
    token.Token(
      token: "t",
      endpoint: "e",
      connection_id: voice_ids.conn("c1"),
      region_id: None,
      server_id: None,
    )
  let after = connection.token_ready(step, ctx, job, Ok(token.Granted(granted)))
  assert after.effects == step.effects
  assert store.is_empty(after.state.pending)
}

pub fn moderator_disconnect_forgets_the_room_key_test() {
  let ctx = context()
  let user = user_id(1)
  let channel = channel_id(10)
  let state =
    State(
      ..guild_state.new(guild_id()),
      states: voice_ids.entries([voice(user, channel, "c1")]),
      room_keys: dict.from_list([#(channel, "key")]),
    )
  let #(step, _) =
    guild_rpc.handle(
      guild_state.start(state),
      ctx,
      guild_state.MoveMember(user, user_id(2), None, None),
    )
  assert store.is_empty(step.state.states)
  assert !dict.has_key(step.state.room_keys, channel)
}
