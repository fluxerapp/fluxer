// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/error.{type RpcError}
import fluxer_gateway/core/id
import fluxer_gateway/core/session
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/voice/guild_handler
import fluxer_gateway/voice/guild_state
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import support/guild_api
import support/guild_fixture as fixture
import support/guild_session as fake
import support/voice_ids
import support/wait

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

pub fn serve(api: guild_api.FakeApi, connection_id: String) -> Nil {
  guild_api.override(
    api,
    "voice_get_token",
    None,
    guild_api.Data(
      json.object([
        #("token", json.string("token-" <> connection_id)),
        #("endpoint", json.string("wss://sfu.test")),
        #("connectionId", json.string(connection_id)),
        #("regionId", json.string("eu")),
        #("serverId", json.string("sfu-1")),
      ]),
    ),
  )
  let success = guild_api.Data(json.object([#("success", json.bool(True))]))
  list.each(
    [
      "voice_state_upsert",
      "voice_state_remove",
      "voice_force_disconnect_participant",
      "voice_update_participant",
    ],
    fn(kind) { guild_api.override(api, kind, None, success) },
  )
}

pub fn request_join(
  found: fake.FakeSession,
  handle: Handle,
  channel: Int,
) -> Nil {
  fake.op(
    found,
    handle,
    session.VoiceStateUpdate(
      fixture.to_dynamic(
        json.object([#("channel_id", json.string(int_text(channel)))]),
      ),
    ),
  )
}

pub fn rpc(
  handle: Handle,
  call: guild_state.RpcCall,
) -> Result(Json, RpcError) {
  let reply = process.new_subject()
  process.send(
    handle.control,
    inbox.VoiceRpc(guild_handler.rpc_request(call, reply)),
  )
  let assert Ok(answer) = process.receive(reply, 5000)
  answer
}

fn calls_for(api: guild_api.FakeApi, kind: String, user: Int) {
  guild_api.calls(api)
  |> list.filter(fn(call) {
    call.kind == kind
    && decode.run(call.body, decode.at(["user_id"], decode.string))
    == Ok(int_text(user))
  })
}

pub fn nonce_of(api: guild_api.FakeApi, user: Int) -> Option(String) {
  calls_for(api, "voice_get_token", user)
  |> list.last
  |> option.from_result
  |> option.then(fn(call) {
    decode.run(call.body, decode.at(["token_nonce"], decode.string))
    |> option.from_result
  })
}

pub fn connect(
  api: guild_api.FakeApi,
  handle: Handle,
  found: fake.FakeSession,
  channel: Int,
  connection_id: String,
) -> Nil {
  let user = id.user_id_to_int(found.ref.user)
  serve(api, connection_id)
  let before = list.length(calls_for(api, "voice_get_token", user))
  request_join(found, handle, channel)
  let assert Some(_) = fake.await_event(found, "VOICE_SERVER_UPDATE", 2000)
  let assert True =
    wait.until(3000, fn() {
      list.length(calls_for(api, "voice_get_token", user)) > before
    })
  let assert Ok(_) =
    rpc(
      handle,
      guild_state.Confirm(
        Some(voice_ids.conn(connection_id)),
        nonce_of(api, user),
      ),
    )
  let assert Some(found_channel) = channel_of(handle, user)
  let assert True = found_channel == channel
  Nil
}

pub fn channel_of(handle: Handle, user: Int) -> Option(Int) {
  let assert Ok(user_id) = id.user_id(user)
  rpc(handle, guild_state.GetVoiceState(user_id))
  |> result.replace_error(Nil)
  |> result.try(fn(found) {
    decode.run(
      fixture.to_dynamic(found),
      decode.at(["voice_state", "channel_id"], decode.string),
    )
    |> result.replace_error(Nil)
  })
  |> result.try(int.parse)
  |> option.from_result
}

pub fn in_voice(handle: Handle, user: Int) -> Bool {
  option.is_some(channel_of(handle, user))
}

pub fn forced(api: guild_api.FakeApi, user: Int) -> Bool {
  calls_for(api, "voice_force_disconnect_participant", user) != []
}

pub fn forced_connection(
  api: guild_api.FakeApi,
  connection_id: String,
) -> Bool {
  guild_api.calls(api)
  |> list.any(fn(call) {
    call.kind == "voice_force_disconnect_participant"
    && decode.run(call.body, decode.at(["connection_id"], decode.string))
    == Ok(connection_id)
  })
}
