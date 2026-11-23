// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session
import fluxer_gateway/voice/guild_state
import gleam/bit_array
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import support/geo_probe as probe
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/guild_voice as voice
import support/voice_ids
import support/wait

const first_channel = 40

const second_channel = 41

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn text(raw: RawJson) -> String {
  raw_json.to_bits(raw) |> bit_array.to_string |> result.unwrap("")
}

fn channel(value: Int) -> id.ChannelId {
  let assert Ok(found) = id.channel_id(value)
  found
}

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn voice_guild() -> fixture.Guild {
  fixture.new(1)
  |> fixture.with_text(10)
  |> fixture.with_channel(first_channel, 2, None, [])
  |> fixture.with_channel(second_channel, 2, None, [])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [])
}

fn frames(seen: List(fake.Seen)) -> List(String) {
  list.filter_map(seen, fn(item) {
    case item {
      fake.Inbox(session.GuildDispatch(_, event, payload, _))
      | fake.Inbox(session.Dispatch(event, payload)) ->
        Ok(event <> " " <> text(payload))
      _ -> Error(Nil)
    }
  })
}

fn token_calls(api: guild_api.FakeApi) -> List(guild_api.Call) {
  guild_api.calls(api)
  |> list.filter(fn(call) { call.kind == "voice_get_token" })
}

fn field(call: guild_api.Call, key: String) -> option.Option(String) {
  decode.run(call.body, decode.at([key], decode.string)) |> option.from_result
}

fn carries_coordinates(call: guild_api.Call) -> Bool {
  field(call, "latitude") == Some(probe.latitude)
  && field(call, "longitude") == Some(probe.longitude)
}

fn send(member: fake.FakeSession, handle, data: Json) -> Nil {
  fake.op(member, handle, session.VoiceStateUpdate(fixture.to_dynamic(data)))
}

fn await_server_update(
  member: fake.FakeSession,
  channel: Int,
  seen: List(fake.Seen),
) -> List(fake.Seen) {
  let assert Ok(item) = process.receive(member.seen, 3000)
  let seen = [item, ..seen]
  case item {
    fake.Inbox(session.GuildDispatch(_, "VOICE_SERVER_UPDATE", payload, _)) ->
      case
        fake.text_at(fake.parse(payload), ["channel_id"])
        == Some(int_text(channel))
      {
        True -> list.reverse(seen)
        False -> await_server_update(member, channel, seen)
      }
    _ -> await_server_update(member, channel, seen)
  }
}

fn confirm(handle, api: guild_api.FakeApi, connection_id: String) -> String {
  voice.rpc(
    handle,
    guild_state.Confirm(
      Some(voice_ids.conn(connection_id)),
      voice.nonce_of(api, 2),
    ),
  )
  |> result.map(json.to_string)
  |> result.unwrap("rpc_error")
}

pub fn guild_voice_never_shows_coordinates_to_anyone_test() {
  use api <- guild_api.with_api
  let logs = probe.capture_logs()
  let found = voice_guild()
  let handle = fixture.started(api, found)
  let member = fake.start(2)
  assert fake.is_joined(fake.join(member, handle))
  voice.serve(api, "conn-geo")
  send(
    member,
    handle,
    json.object([
      #("channel_id", json.string(int_text(first_channel))),
      ..probe.fields()
    ]),
  )
  let joined = await_server_update(member, first_channel, [])
  assert confirm(handle, api, "conn-geo") == "{\"success\":true}"
  let observer = fake.start(3)
  let reply = fake.join(observer, handle)
  let assert Some(created) = fake.guild_create(reply)
  let create_text = string.inspect(created)
  assert string.contains(create_text, "conn-geo")
  send(
    member,
    handle,
    json.object([
      #("channel_id", json.string(int_text(second_channel))),
      #("connection_id", json.string("conn-geo")),
    ]),
  )
  let moved = await_server_update(member, second_channel, [])
  assert confirm(handle, api, "conn-geo") == "{\"success\":true}"
  assert voice.channel_of(handle, 2) == Some(second_channel)
  let assert Ok(switched) =
    voice.rpc(handle, guild_state.SwitchRegion(channel(second_channel)))
  let reissued = await_server_update(member, second_channel, [])
  assert confirm(handle, api, "conn-geo") == "{\"success\":true}"
  let replies =
    [
      voice.rpc(handle, guild_state.GetVoiceState(user(2))),
      voice.rpc(handle, guild_state.ChannelStates(channel(second_channel))),
      Ok(switched),
    ]
    |> list.map(fn(found) {
      found |> result.map(json.to_string) |> result.unwrap("rpc_error")
    })
  send(member, handle, json.object([#("channel_id", json.null())]))
  assert wait.until(3000, fn() { !voice.in_voice(handle, 2) })
  let seen =
    [
      joined,
      moved,
      reissued,
      fake.drain(member, 200),
      fake.drain(observer, 200),
    ]
    |> list.flatten
    |> frames
  assert list.any(seen, string.starts_with(_, "VOICE_STATE_UPDATE"))
  assert list.any(seen, string.starts_with(_, "VOICE_SERVER_UPDATE"))
  assert probe.leaking(seen) == []
  assert !probe.leaks(create_text)
  assert probe.leaking(replies) == []
  let tokens = token_calls(api)
  assert list.length(tokens) == 3
  assert list.all(tokens, carries_coordinates)
  assert wait.until(3000, fn() {
    guild_api.calls(api)
    |> list.any(fn(call) { call.kind == "voice_state_upsert" })
  })
  let others =
    guild_api.calls(api)
    |> list.filter(fn(call) { call.kind != "voice_get_token" })
    |> list.map(fn(call) { call.kind <> " " <> string.inspect(call.body) })
  assert probe.leaking(others) == []
  fixture.stop(found)
  assert probe.leaking(probe.finish(logs)) == []
}
