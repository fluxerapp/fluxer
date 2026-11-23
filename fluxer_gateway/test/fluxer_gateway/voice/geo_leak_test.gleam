// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{type SessionRef}
import fluxer_gateway/voice/call/state as call_state
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/geo
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import fluxer_gateway/voice/rpc
import gleam/bit_array
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string
import logging
import support/geo_probe as probe
import support/voice_api as api
import support/voice_guild as g
import support/voice_ids

const public_keys = [
  "guild_id", "channel_id", "user_id", "connection_id", "session_id", "member",
  "mute", "deaf", "self_mute", "self_deaf", "self_video", "self_stream",
  "is_mobile", "suppress", "viewer_stream_keys", "e2ee_capable", "p2p",
  "version",
]

const routing_keys = ["region_id", "server_id"]

const ok = "{\"success\":true}"

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "gleeunit_ffi", "find_files")
fn find_files(matching matching: String, in in: String) -> List(String)

fn read(path: String) -> String {
  read_file(path)
  |> result.replace_error(Nil)
  |> result.try(bit_array.to_string)
  |> result.unwrap("")
}

fn text(raw: RawJson) -> String {
  raw_json.to_bits(raw) |> bit_array.to_string |> result.unwrap("")
}

fn keys(encoded: String) -> List(String) {
  json.parse(encoded, decode.dict(decode.string, decode.dynamic))
  |> result.map(dict.keys)
  |> result.unwrap([])
  |> list.sort(string.compare)
}

fn sorted(names: List(String)) -> List(String) {
  list.sort(names, string.compare)
}

fn sample() -> entry.Entry {
  entry.new(
    user: g.user_id(3),
    guild: id.guild_id(5) |> option.from_result,
    channel: g.channel_id(4),
    connection_id: voice_ids.conn("c1"),
    session_id: Some("s1"),
    flags: entry.Flags(..entry.no_flags(), self_video: True),
    mute: True,
    deaf: False,
  )
  |> entry.set_routing(Some("eu"), Some("sfu-1"))
  |> entry.set_viewer_stream_keys(["k1"])
  |> entry.set_e2ee_capable(True)
  |> entry.bump
}

pub fn every_voice_state_shape_has_exactly_the_public_keys_test() {
  let broadcast = sample() |> entry.to_json(entry.Broadcast) |> json.to_string
  let external = sample() |> entry.to_json(entry.External) |> json.to_string
  assert keys(broadcast) == sorted(public_keys)
  assert keys(external) == sorted(list.append(public_keys, routing_keys))
  assert !probe.leaks(broadcast)
  assert !probe.leaks(external)
  assert !probe.leaks(text(entry.to_raw(sample(), entry.External)))
  assert !probe.leaks(text(entry.to_raw(sample(), entry.Broadcast)))
}

pub fn call_encoders_carry_only_public_voice_state_keys_test() {
  let assert Ok(message) = id.message_id(77)
  let call =
    call_state.new(
      call_state.Init(
        channel: g.channel_id(4),
        message:,
        region: None,
        ringing: [],
        recipients: [g.user_id(3)],
        caller: None,
      ),
      0,
    )
    |> call_state.put_entry(sample())
  let event = text(call_state.event(call))
  let view = call_state.view(call)
  let listed = json.to_string(call_state.view_json(view))
  let rpc_listed = json.to_string(rpc.call_voice_states(view, g.channel_id(4)))
  assert call_state.entries(call) != []
  assert !probe.leaks(event)
  assert !probe.leaks(listed)
  assert !probe.leaks(rpc_listed)
  let states =
    json.parse(event, decode.at(["voice_states"], decode.list(decode.dynamic)))
    |> result.unwrap([])
  assert list.length(states) == 1
}

fn geo_of(data: json.Json) -> geo.Geo {
  let assert Ok(found) = json.parse(json.to_string(data), geo.decoder())
  found
}

pub fn geo_needs_both_coordinates_test() {
  assert !geo.is_known(geo.new(Some("1"), None))
  assert !geo.is_known(geo.new(None, Some("1")))
  assert geo.coordinates(geo.new(Some("1"), Some("2"))) == Some(#("1", "2"))
  let parsed = geo_of(json.object(probe.fields()))
  assert geo.coordinates(parsed) == Some(#(probe.latitude, probe.longitude))
  let numbers =
    geo_of(
      json.object([
        #("latitude", json.int(59)),
        #("longitude", json.float(18.5)),
      ]),
    )
  assert geo.coordinates(numbers) == Some(#("59", "18.5"))
  assert geo.or(geo.unknown(), parsed) == parsed
  assert geo.or(numbers, parsed) == numbers
  assert !geo.is_known(geo_of(json.object([])))
}

pub fn coordinates_are_named_only_where_they_may_go_test() {
  let allowed = [
    "voice/geo.gleam", "voice/token.gleam", "gateway/identify.gleam",
    "core/session.gleam", "session/manager/shard.gleam",
  ]
  let named =
    find_files(matching: "**/*.gleam", in: "src/fluxer_gateway")
    |> list.filter(fn(path) {
      let source = read("src/fluxer_gateway/" <> path)
      string.contains(source, "latitude")
      || string.contains(source, "longitude")
    })
    |> sorted
  assert named == sorted(allowed)
  let voice_state_modules = [
    "voice/entry.gleam", "core/voice_state.gleam", "voice/call/state.gleam",
  ]
  list.each(voice_state_modules, fn(path) {
    let source = read("src/fluxer_gateway/" <> path)
    assert source != ""
    assert !string.contains(source, "voice/geo")
  })
  let geo_source = read("src/fluxer_gateway/voice/geo.gleam")
  assert !string.contains(geo_source, "gleam/json")
  assert !string.contains(geo_source, "logging")
  find_files(matching: "**/*.gleam", in: "src/fluxer_gateway/telemetry")
  |> list.each(fn(path) {
    assert !string.contains(
      read("src/fluxer_gateway/telemetry/" <> path),
      "voice/geo",
    )
  })
}

fn effect_texts(effects: List(guild_handler.VoiceEffect)) -> List(String) {
  list.flat_map(effects, fn(effect) {
    case effect {
      guild_handler.Broadcast(_, payload, _) -> [text(payload)]
      guild_handler.BroadcastMove(_, payload, left, _, _) -> [
        text(payload),
        text(left),
      ]
      guild_handler.SendToSession(_, _, payload) -> [text(payload)]
      other -> [string.inspect(other)]
    }
  })
}

fn server_updates(host: g.Host, ref: SessionRef) -> List(Dynamic) {
  g.sent_to(g.effects(host), ref, "VOICE_SERVER_UPDATE")
}

fn await_server_update(host: g.Host, ref: SessionRef, count: Int) -> String {
  let assert True =
    api.eventually(fn() { list.length(server_updates(host, ref)) >= count })
  let assert Ok(last) = list.last(server_updates(host, ref))
  let assert Some(conn) = g.text_of(last, "connection_id")
  conn
}

fn last_nonce(stub: api.Api) -> option.Option(String) {
  api.calls_of(stub, "voice_get_token")
  |> list.last
  |> option.from_result
  |> option.then(api.field(_, "token_nonce"))
}

fn token_calls(stub: api.Api) -> List(api.Call) {
  api.calls_of(stub, "voice_get_token")
}

fn carries_coordinates(call: api.Call) -> Bool {
  api.field(call, "latitude") == Some(probe.latitude)
  && api.field(call, "longitude") == Some(probe.longitude)
}

fn other_api_texts(stub: api.Api) -> List(String) {
  api.calls(stub)
  |> list.filter(fn(call) { call.kind != "voice_get_token" })
  |> list.map(fn(call) { call.kind <> " " <> string.inspect(call.body) })
}

pub fn guild_join_move_switch_and_hand_off_never_expose_coordinates_test() {
  let world = g.World(..g.world(), guild: 947)
  let stub = api.start()
  let host = g.start(world)
  let logs = probe.capture_logs()
  let s1 = g.session(1)
  let s2 = g.session(2)
  g.client(host, s1, g.join_with(10, probe.fields()))
  let conn = await_server_update(host, s1, 1)
  assert g.confirm(host, conn, last_nonce(stub)) == ok
  g.client(host, s2, g.join(10))
  let other = await_server_update(host, s2, 1)
  assert g.confirm(host, other, last_nonce(stub)) == ok
  g.client(host, s1, g.update(Some(20), Some(conn), []))
  let moved = await_server_update(host, s1, 2)
  assert g.confirm(host, moved, last_nonce(stub)) == ok
  assert g.rpc_text(host, guild_state.SwitchRegion(g.channel_id(20))) == ok
  let switched = await_server_update(host, s1, 3)
  assert g.confirm(host, switched, last_nonce(stub)) == ok
  let replies = [
    g.rpc_text(host, guild_state.GetVoiceState(g.user_id(1))),
    g.rpc_text(host, guild_state.ChannelStates(g.channel_id(20))),
    g.rpc_text(host, guild_state.ChannelStates(g.channel_id(10))),
  ]
  let views = list.map(g.states(host), string.inspect)
  let before = effect_texts(g.effects(host))
  let #(voice, _successor) = g.freeze(host)
  let target = g.adopt(world, voice)
  g.forward(host)
  let assert True = api.eventually(fn() { !process.is_alive(host.pid) })
  assert g.rpc_text(target, guild_state.SwitchRegion(g.channel_id(20))) == ok
  let resumed = await_server_update(target, s1, 1)
  assert g.confirm(target, resumed, last_nonce(stub)) == ok
  g.client(target, s1, g.update(None, None, []))
  g.client(target, s2, g.update(None, None, []))
  assert api.eventually(fn() { g.states(target) == [] })
  api.settle()
  let tokens = token_calls(stub)
  let user_one =
    list.filter(tokens, fn(call) { api.field(call, "user_id") == Some("1") })
  assert list.length(user_one) == 4
  assert list.all(user_one, carries_coordinates)
  assert list.all(tokens, fn(call) {
    api.field(call, "user_id") == Some("1") || !carries_coordinates(call)
  })
  let frames = list.append(before, effect_texts(g.effects(target)))
  assert frames != []
  assert probe.leaking(frames) == []
  assert probe.leaking(replies) == []
  assert probe.leaking(views) == []
  let persisted = other_api_texts(stub)
  assert list.any(persisted, string.starts_with(_, "voice_state_upsert"))
  assert probe.leaking(persisted) == []
  g.stop(target)
  api.stop(stub)
  logging.log(logging.Info, "voice geo probe marker")
  let lines = probe.finish(logs)
  assert list.any(lines, string.contains(_, "voice geo probe marker"))
  assert probe.leaking(lines) == []
}

pub fn p2p_join_never_exposes_coordinates_test() {
  let world = g.World(..g.world(), guild: 948)
  let stub = api.start()
  api.set(
    stub,
    "voice_get_token",
    api.Respond(fn(body) {
      api.p2p_grant(
        "p2p-"
        <> option.unwrap(
          api.field(api.Call("voice_get_token", body), "user_id"),
          "x",
        ),
      )
    }),
  )
  let host = g.start(world)
  let s1 = g.session(1)
  g.client(
    host,
    s1,
    g.join_with(30, [#("p2p", json.bool(True)), ..probe.fields()]),
  )
  let _ = await_server_update(host, s1, 1)
  assert api.eventually(fn() { g.states(host) != [] })
  assert list.all(token_calls(stub), carries_coordinates)
  g.client(host, s1, g.update(None, None, []))
  assert api.eventually(fn() { g.states(host) == [] })
  api.settle()
  assert probe.leaking(effect_texts(g.effects(host))) == []
  assert probe.leaking(other_api_texts(stub)) == []
  g.stop(host)
  api.stop(stub)
}
