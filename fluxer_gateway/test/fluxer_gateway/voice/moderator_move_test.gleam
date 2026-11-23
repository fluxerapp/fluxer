// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/guild
import fluxer_gateway/core/id
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/user
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/perms/fixture
import fluxer_gateway/perms/snapshot
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/geo
import fluxer_gateway/voice/guild_rpc
import fluxer_gateway/voice/guild_state.{State}
import fluxer_gateway/voice/region
import fluxer_gateway/voice/store
import fluxer_gateway/voice/token
import fluxer_gateway/voice/virtual_access
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
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

fn full_channel() -> channel.Channel {
  channel.new(
    id: channel_id(20),
    kind: channel.kind_from_int(2),
    parent: None,
    position: 0,
    overwrites: [],
    raw: raw_json.wrap(<<"{\"id\":\"20\",\"user_limit\":2}">>),
  )
  |> channel.set_voice(channel.voice_limits(
    user_limit: 2,
    connection_limit: 5,
    rtc_p2p: False,
  ))
}

fn some_member(value: id.UserId) -> member.Member {
  member.new(
    user: user.new(
      id: value,
      bot: False,
      flags: 0,
      restricted: False,
      wire: raw_json.wrap(<<"{}">>),
    ),
    nick: None,
    roles: [],
    joined_at_ms: None,
    timed_out_until_ms: None,
    wire: raw_json.wrap(<<"{}">>),
  )
}

fn context(features: List(String)) -> guild_state.Ctx {
  let assert Ok(Nil) = nif.load()
  let #(_, reader) = perms.new(guild_id(), process.self())
  guild_state.Ctx(
    facts: guild.facts(
      id: guild_id(),
      owner: None,
      nsfw_level: guild.NsfwDefault,
      features:,
      member_count: 0,
    ),
    reader:,
    member: fn(value) { Some(some_member(value)) },
    channel: fn(found) {
      case found == channel_id(20) {
        True -> Some(full_channel())
        False -> None
      }
    },
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
    session_id: Some("s" <> conn),
    flags: entry.no_flags(),
    mute: False,
    deaf: False,
  )
}

fn full_channel_with_pending(moved: Bool) -> guild_state.State {
  let target = channel_id(20)
  let mover = user_id(1)
  let pending =
    guild_state.Pending(
      user: mover,
      channel: target,
      session_id: Some("sc9"),
      entry: voice(mover, target, "c9"),
      geo: geo.unknown(),
      nonce: Some("n"),
      created_at: 1000,
      expires_at: 60_000,
      moved:,
    )
  State(
    ..guild_state.new(guild_id()),
    states: voice_ids.entries([
      voice(user_id(2), target, "c2"),
      voice(user_id(3), target, "c3"),
    ]),
    pending: voice_ids.pendings([pending]),
    access: virtual_access.grant(virtual_access.new(), mover, target),
  )
}

pub fn moderator_move_into_full_channel_is_confirmed_test() {
  let #(step, _) =
    guild_rpc.handle(
      guild_state.start(full_channel_with_pending(True)),
      context([]),
      guild_state.Confirm(Some(voice_ids.conn("c9")), Some("n")),
    )
  assert voice_ids.has(step.state.states, "c9")
  assert store.is_empty(step.state.pending)
}

pub fn client_join_into_full_channel_is_refused_at_confirm_test() {
  let #(step, _) =
    guild_rpc.handle(
      guild_state.start(full_channel_with_pending(False)),
      context([]),
      guild_state.Confirm(Some(voice_ids.conn("c9")), Some("n")),
    )
  assert !voice_ids.has(step.state.states, "c9")
  assert store.is_empty(step.state.pending)
}

fn sent_key(step: guild_state.Step) -> Result(String, Nil) {
  list.find_map(step.effects, fn(effect) {
    case effect {
      guild_state.ToSession(
        session: "s1",
        event: "VOICE_SERVER_UPDATE",
        payload:,
      ) ->
        json.parse_bits(
          raw_json.to_bits(payload),
          decode.at(["e2ee_key"], decode.string),
        )
        |> result.replace_error(Nil)
      _ -> Error(Nil)
    }
  })
}

fn reissue(features: List(String), capable: Bool) -> guild_state.Step {
  reissue_as(features, capable, moved: True)
}

fn reissue_as(
  features: List(String),
  capable: Bool,
  moved moved: Bool,
) -> guild_state.Step {
  let user = user_id(1)
  let target = channel_id(20)
  let source = case moved {
    True -> channel_id(10)
    False -> target
  }
  let base =
    voice(user, source, "c1")
    |> entry.set_e2ee_capable(capable)
  let live = case moved {
    True -> voice_ids.entries([])
    False -> voice_ids.entries([base])
  }
  let job =
    guild_state.ReissueJob(
      user:,
      channel: target,
      session_id: Some("s1"),
      base:,
      geo: geo.unknown(),
      nonce: "n1",
      moved:,
      suppress: None,
    )
  let granted =
    token.Token(
      token: "t",
      endpoint: "e",
      connection_id: voice_ids.conn("c5"),
      region_id: None,
      server_id: None,
    )
  let state =
    State(
      ..guild_state.new(guild_id()),
      states: live,
      room_keys: dict.from_list([#(target, "room-key")]),
    )
  region.reissue_ready(
    guild_state.start(state),
    context(features),
    job,
    Ok(token.Granted(granted)),
  )
}

pub fn moderator_move_hands_over_the_room_key_test() {
  let step = reissue(["VOICE_E2EE"], True)
  assert sent_key(step) == Ok("room-key")
  assert dict.get(step.state.room_keys, channel_id(20)) == Ok("room-key")
}

pub fn moderator_move_without_e2ee_sends_no_key_test() {
  assert sent_key(reissue([], True)) == Error(Nil)
  assert sent_key(reissue(["VOICE_E2EE"], False)) == Error(Nil)
}

fn pending_capable(step: guild_state.Step) -> Result(Bool, Nil) {
  voice_ids.at(step.state.pending, "c5")
  |> result.map(fn(pending) { entry.e2ee_capable(pending.entry) })
}

pub fn p2p_to_sfu_conversion_hands_over_the_room_key_test() {
  let step = reissue_as(["VOICE_E2EE"], True, moved: False)
  assert sent_key(step) == Ok("room-key")
  assert pending_capable(step) == Ok(True)
}

pub fn plain_region_switch_hands_over_the_room_key_test() {
  let step = reissue_as(["VOICE_E2EE"], True, moved: False)
  assert sent_key(step) == Ok("room-key")
  assert pending_capable(step) == Ok(True)
}

pub fn plain_region_switch_for_a_gone_connection_sends_nothing_test() {
  let user = user_id(1)
  let job =
    guild_state.ReissueJob(
      user:,
      channel: channel_id(20),
      session_id: Some("s1"),
      base: voice(user, channel_id(20), "c1"),
      geo: geo.unknown(),
      nonce: "n1",
      moved: False,
      suppress: None,
    )
  let granted =
    token.Token(
      token: "t",
      endpoint: "e",
      connection_id: voice_ids.conn("c1"),
      region_id: None,
      server_id: None,
    )
  let step =
    region.reissue_ready(
      guild_state.start(guild_state.new(guild_id())),
      context([]),
      job,
      Ok(token.Granted(granted)),
    )
  assert store.is_empty(step.state.pending)
  assert step.effects == []
}

pub fn plain_region_switch_without_capability_sends_no_key_test() {
  let step = reissue_as(["VOICE_E2EE"], False, moved: False)
  assert sent_key(step) == Error(Nil)
  assert pending_capable(step) == Ok(False)
}

fn moved_into_deleted_channel() -> guild_state.State {
  let mover = user_id(1)
  let deleted = channel_id(30)
  State(
    ..guild_state.new(guild_id()),
    pending: voice_ids.pendings([
      guild_state.Pending(
        user: mover,
        channel: deleted,
        session_id: Some("sc9"),
        entry: voice(mover, deleted, "c9"),
        geo: geo.unknown(),
        nonce: Some("n"),
        created_at: 1000,
        expires_at: 60_000,
        moved: True,
      ),
    ]),
    access: virtual_access.new()
      |> virtual_access.grant(mover, deleted)
      |> virtual_access.mark_transition(mover, deleted),
  )
}

pub fn moderator_move_into_deleted_channel_is_refused_at_confirm_test() {
  let #(step, _) =
    guild_rpc.handle(
      guild_state.start(moved_into_deleted_channel()),
      context([]),
      guild_state.Confirm(Some(voice_ids.conn("c9")), Some("n")),
    )
  assert !voice_ids.has(step.state.states, "c9")
  assert store.is_empty(step.state.pending)
}

pub fn reissue_into_deleted_channel_stores_nothing_test() {
  let user = user_id(1)
  let job =
    guild_state.ReissueJob(
      user:,
      channel: channel_id(30),
      session_id: Some("s1"),
      base: voice(user, channel_id(10), "c1"),
      geo: geo.unknown(),
      nonce: "n1",
      moved: True,
      suppress: None,
    )
  let granted =
    token.Token(
      token: "t",
      endpoint: "e",
      connection_id: voice_ids.conn("c5"),
      region_id: None,
      server_id: None,
    )
  let step =
    region.reissue_ready(
      guild_state.start(guild_state.new(guild_id())),
      context([]),
      job,
      Ok(token.Granted(granted)),
    )
  assert store.is_empty(step.state.pending)
  assert !list.any(step.effects, fn(effect) {
    case effect {
      guild_state.ToSession(event: "VOICE_SERVER_UPDATE", ..) -> True
      _ -> False
    }
  })
}

fn voice_channel(value: Int) -> channel.Channel {
  fixture.make_channel(value, channel.Voice, None, [])
}

fn permitted_context() -> guild_state.Ctx {
  let assert Ok(Nil) = nif.load()
  let channels = [voice_channel(10), voice_channel(20)]
  let source =
    [1, 2, 5]
    |> list.fold(
      snapshot.new(guild_id())
        |> snapshot.set_roles([fixture.make_role(900, 1024 + 1_048_576)]),
      fn(acc, value) { snapshot.add_member(acc, some_member(user_id(value))) },
    )
  let source = list.fold(channels, source, snapshot.add_channel)
  let #(handle, reader) = perms.new(guild_id(), process.self())
  let assert Ok(_) = perms.load(handle, source)
  guild_state.Ctx(..context([]), reader:, channel: fn(wanted) {
    list.find(channels, fn(found) { channel.id(found) == wanted })
    |> option.from_result
  })
}

fn moved_out_of(others: List(entry.Entry)) -> guild_state.Step {
  let source = channel_id(10)
  let state =
    State(
      ..guild_state.new(guild_id()),
      states: voice_ids.entries([
        voice(user_id(1), source, "c1") |> entry.set_e2ee_capable(True),
        ..others
      ]),
      room_keys: dict.from_list([#(source, "old-key")]),
    )
  let #(step, _) =
    guild_rpc.handle(
      guild_state.start(state),
      permitted_context(),
      guild_state.MoveMember(user_id(1), user_id(5), Some(channel_id(20)), None),
    )
  step
}

pub fn moderator_move_forgets_the_key_of_the_emptied_channel_test() {
  let step = moved_out_of([])
  assert !voice_ids.has(step.state.states, "c1")
  assert dict.get(step.state.room_keys, channel_id(10)) == Error(Nil)
}

pub fn moderator_move_keeps_the_key_while_others_remain_test() {
  let step = moved_out_of([voice(user_id(2), channel_id(10), "c2")])
  assert !voice_ids.has(step.state.states, "c1")
  assert dict.get(step.state.room_keys, channel_id(10)) == Ok("old-key")
}
