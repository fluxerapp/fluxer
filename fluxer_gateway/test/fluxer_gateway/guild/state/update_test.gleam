// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/init
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/guild/state/update
import fluxer_gateway/nif
import fluxer_gateway/util/decode as id_decode
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/option.{None}

fn parse(text: String) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<text:utf8>>)
  value
}

fn fixture() -> GuildState {
  let assert Ok(Nil) = nif.load()
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"1\"}")),
      #(
        "roles",
        whole("[{\"id\":\"100\",\"permissions\":\"0\",\"position\":0}]"),
      ),
      #("channels", whole("[{\"id\":\"5\",\"type\":0,\"position\":0}]")),
      #(
        "members",
        whole("[{\"user\":{\"id\":\"2\",\"username\":\"old\"},\"roles\":[]}]"),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(loaded) = data_decode.collections(guild, collected)
  let assert Ok(state) =
    init.build(
      core_guild.GuildRef(guild:, inbox: process.new_subject()),
      process.new_subject(),
      process.new_subject(),
      loaded,
      None,
      None,
      guild_sessions.new(),
    )
  state
}

fn bumps(state: GuildState, event_name: String, event: String) -> GuildState {
  let next = update.apply_event(state, event_name, parse(event)).state
  assert guild_state.version(next) == guild_state.version(state) + 1
  next
}

fn keeps(state: GuildState, event_name: String, event: String) -> GuildState {
  let next = update.apply_event(state, event_name, parse(event)).state
  assert guild_state.version(next) == guild_state.version(state)
  next
}

pub fn emoji_and_sticker_updates_bump_version_test() {
  fixture()
  |> bumps("GUILD_EMOJIS_UPDATE", "{\"emojis\":[{\"id\":\"7\"}]}")
  |> bumps("GUILD_STICKERS_UPDATE", "{\"stickers\":[{\"id\":\"8\"}]}")
}

pub fn newer_last_message_bumps_version_test() {
  fixture()
  |> bumps("MESSAGE_CREATE", "{\"id\":\"9\",\"channel_id\":\"5\"}")
  |> keeps("MESSAGE_CREATE", "{\"id\":\"9\",\"channel_id\":\"5\"}")
  |> keeps("MESSAGE_CREATE", "{\"id\":\"3\",\"channel_id\":\"5\"}")
  |> bumps("MESSAGE_CREATE", "{\"id\":\"12\",\"channel_id\":\"5\"}")
}

pub fn pins_update_bumps_version_test() {
  fixture()
  |> bumps(
    "CHANNEL_PINS_UPDATE",
    "{\"channel_id\":\"5\",\"last_pin_timestamp\":\"2026-11-18T00:00:00Z\"}",
  )
  |> keeps("CHANNEL_PINS_UPDATE", "{\"channel_id\":\"6\"}")
}

pub fn unrelated_event_keeps_version_test() {
  fixture()
  |> keeps("TYPING_START", "{\"channel_id\":\"5\"}")
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
