// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/guild/data.{type GuildData, GuildData}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleam/result
import gleam/string

pub fn guild_update(guild_data: GuildData, event: Dynamic) -> GuildData {
  data_decode.rewrite_facts(guild_data, event)
}

pub fn create(guild_data: GuildData, event: Dynamic) -> GuildData {
  case data_decode.decode_channel(event) {
    Ok(row) ->
      GuildData(..guild_data, channels: [
        row,
        ..list.filter(guild_data.channels, fn(old) {
          channel.id(old) != channel.id(row)
        })
      ])
    Error(_) -> guild_data
  }
}

pub fn update(guild_data: GuildData, event: Dynamic) -> GuildData {
  case data_decode.decode_channel(event) {
    Ok(row) -> replace_existing(guild_data, row)
    Error(_) -> guild_data
  }
}

fn replace_existing(guild_data: GuildData, row: channel.Channel) -> GuildData {
  case data.has_channel(guild_data, channel.id(row)) {
    True ->
      data.put_channel(
        data.drop_channel_patches(guild_data, channel.id(row)),
        row,
      )
    False -> guild_data
  }
}

pub fn update_bulk(guild_data: GuildData, event: Dynamic) -> GuildData {
  decode.run(
    event,
    decode.at(
      ["channels"],
      data_decode.lenient_list(data_decode.channel_decoder()),
    ),
  )
  |> result.unwrap([])
  |> list.fold(guild_data, replace_existing)
}

pub fn delete(guild_data: GuildData, event: Dynamic) -> GuildData {
  case decode.run(event, decode.at(["id"], id_decode.channel_id())) {
    Ok(target) -> data.remove_channel(guild_data, target)
    Error(_) -> guild_data
  }
}

pub fn message_create(guild_data: GuildData, event: Dynamic) -> GuildData {
  let kind =
    decode.run(event, decode.at(["type"], data_decode.flexible_int()))
    |> result.unwrap(0)
  let channel_id =
    decode.run(event, decode.at(["channel_id"], id_decode.channel_id()))
  let message = decode.run(event, decode.at(["id"], message_id()))
  case kind, channel_id, message {
    18, _, _ -> guild_data
    _, Ok(target), Ok(#(message, wire)) ->
      case data.find_channel(guild_data, target) {
        None -> guild_data
        Some(row) ->
          case channel.last_message(row) {
            Some(current) if current >= message -> guild_data
            _ ->
              guild_data
              |> data.put_channel(channel.set_last_message(row, Some(message)))
              |> data.set_channel_field(target, "last_message_id", wire)
          }
      }
    _, _, _ -> guild_data
  }
}

fn message_id() -> decode.Decoder(#(Int, RawJson)) {
  use value <- decode.then(id_decode.snowflake())
  use text <- decode.then(decode.one_of(decode.string, [decode.success("")]))
  case string.starts_with(text, "0") || text == "" {
    True -> decode.success(#(value, raw_json.from_json(id.json(value))))
    False -> decode.success(#(value, raw_json.wrap(<<"\"", text:utf8, "\"">>)))
  }
}

pub fn pins_update(guild_data: GuildData, event: Dynamic) -> GuildData {
  let channel_id =
    decode.run(event, decode.at(["channel_id"], id_decode.channel_id()))
  let last_pin =
    decode.run(event, decode.at(["last_pin_timestamp"], decode.dynamic))
  case channel_id, last_pin {
    Ok(target), Ok(value) ->
      case data.has_channel(guild_data, target) {
        True ->
          data.set_channel_field(
            guild_data,
            target,
            "last_pin_timestamp",
            data_decode.raw(value),
          )
        False -> guild_data
      }
    _, _ -> guild_data
  }
}

pub fn emojis_update(guild_data: GuildData, event: Dynamic) -> GuildData {
  GuildData(
    ..guild_data,
    emojis: data_decode.raw_list(
      decode.run(event, decode.at(["emojis"], decode.dynamic))
      |> result.replace_error(Nil),
    ),
  )
}

pub fn stickers_update(guild_data: GuildData, event: Dynamic) -> GuildData {
  GuildData(
    ..guild_data,
    stickers: data_decode.raw_list(
      decode.run(event, decode.at(["stickers"], decode.dynamic))
      |> result.replace_error(Nil),
    ),
  )
}

pub fn channel_ids(event_name: String, event: Dynamic) -> List(id.ChannelId) {
  case event_name {
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      decode.run(event, decode.at(["id"], id_decode.channel_id()))
      |> result.map(list.wrap)
      |> result.unwrap([])
    "CHANNEL_UPDATE_BULK" ->
      decode.run(
        event,
        decode.at(
          ["channels"],
          data_decode.lenient_list(decode.at(["id"], id_decode.channel_id())),
        ),
      )
      |> result.unwrap([])
    _ -> []
  }
}
