// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/cluster/relay
import fluxer_gateway/config/durations
import fluxer_gateway/core/clock
import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type ClientOp, LazyRequest, RequestGuildMembers, VoiceSignal, VoiceStateUpdate,
}
import fluxer_gateway/session/guilds
import fluxer_gateway/session/links.{Linked}
import fluxer_gateway/session/member_requests_limit
import fluxer_gateway/session/relations
import fluxer_gateway/session/state.{type State, Watched}
import fluxer_gateway/session/visits
import fluxer_gateway/util/decode as edge
import fluxer_gateway/voice/client as voice_client
import fluxer_gateway/voice/client_update
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/atom
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import logging

const max_nonce_bytes = 64

const max_guild_ids = 100

fn counts_timeout_ms() -> Int {
  durations.session().voice_counts_timeout_ms
}

type Field {
  Missing
  Null
  Value(Dynamic)
}

fn field(name: String) -> Decoder(Field) {
  decode.optional_field(
    name,
    Missing,
    decode.optional(decode.dynamic)
      |> decode.map(fn(value) {
        value |> option.map(Value) |> option.unwrap(Null)
      }),
    decode.success,
  )
}

fn snowflake(value: Field) -> Result(Option(Int), String) {
  case value {
    Missing | Null -> Ok(None)
    Value(raw) ->
      decode.run(raw, edge.snowflake())
      |> result.map(Some)
      |> result.replace_error("invalid_snowflake")
  }
}

type Update {
  Update(guild: Field, channel: Field, connection: Field)
}

fn update_decoder() -> Decoder(Update) {
  use guild <- decode.then(field("guild_id"))
  use channel <- decode.then(field("channel_id"))
  use connection <- decode.then(field("connection_id"))
  decode.success(Update(guild:, channel:, connection:))
}

pub fn state_update(state: State, data: Dynamic) -> Nil {
  case decode.run(data, update_decoder()) {
    Error(_) -> ignored(state, "invalid_params")
    Ok(update) ->
      case snowflake(update.guild), snowflake(update.channel) {
        Error(reason), _ | _, Error(reason) -> ignored(state, reason)
        Ok(None), Ok(channel) ->
          case channel, update.connection {
            None, Missing | None, Null -> disconnect_all(state)
            _, Value(raw) ->
              case is_string(raw) {
                True -> dm_update(state, enrich(state, data))
                False -> ignored(state, "invalid_params")
              }
            Some(_), _ -> dm_update(state, enrich(state, data))
          }
        Ok(Some(guild)), Ok(_) -> guild_update(state, guild, data)
      }
  }
}

fn guild_update(state: State, guild: Int, data: Dynamic) -> Nil {
  case id.guild_id(guild) {
    Error(_) -> ignored(state, "invalid_snowflake")
    Ok(guild_id) ->
      case links.get(state.links, guild_id) {
        None -> ignored(state, "guild_not_found")
        Some(Linked(_, _)) -> {
          guilds.send_op(state, guild_id, VoiceStateUpdate(enrich(state, data)))
          Nil
        }
        Some(_) -> ignored(state, "guild_not_connected")
      }
  }
}

fn ignored(state: State, reason: String) -> Nil {
  logging.log(
    logging.Debug,
    "voice_state_update_ignored: reason="
      <> reason
      <> " user_id="
      <> id.user_id_to_string(state.user_id),
  )
}

pub fn session_closed(state: State) -> Nil {
  disconnect_all(state)
  voice_client.session_closed(state.me)
}

pub fn session_handed_off(state: State) -> Nil {
  voice_client.session_handed_off(state.me)
}

pub fn session_rehydrated(state: State) -> Nil {
  relations.private_channel_ids(state.relations)
  |> list.filter_map(fn(channel) {
    id.channel_id(channel) |> result.replace_error(Nil)
  })
  |> voice_client.session_rehydrated(state.me, _)
}

fn disconnect_all(state: State) -> Nil {
  let request = disconnect_request(state)
  list.each(links.guild_ids(state.links), fn(guild) {
    guilds.send_op(state, guild, VoiceStateUpdate(request))
  })
  dm_update(state, request)
}

fn dm_update(state: State, data: Dynamic) -> Nil {
  decode.run(data, client_update.decoder())
  |> result.unwrap(client_update.empty())
  |> voice_client.dm_voice_update(state.me, _, calls: watched_calls(state))
}

fn watched_calls(state: State) -> List(id.ChannelId) {
  dict.keys(state.calls)
  |> list.filter_map(fn(channel) {
    id.channel_id(channel) |> result.replace_error(Nil)
  })
}

fn disconnect_request(state: State) -> Dynamic {
  dynamic.properties([
    #(dynamic.string("guild_id"), null()),
    #(dynamic.string("channel_id"), null()),
    #(dynamic.string("connection_id"), null()),
    #(dynamic.string("self_mute"), dynamic.bool(False)),
    #(dynamic.string("self_deaf"), dynamic.bool(False)),
    #(dynamic.string("self_video"), dynamic.bool(False)),
    #(dynamic.string("self_stream"), dynamic.bool(False)),
    #(dynamic.string("viewer_stream_keys"), dynamic.list([])),
    #(dynamic.string("is_mobile"), dynamic.bool(False)),
    ..session_facts(state)
  ])
}

fn session_facts(state: State) -> List(#(Dynamic, Dynamic)) {
  [
    #(dynamic.string("e2ee_capable"), dynamic.bool(state.e2ee_capable)),
    #(dynamic.string("bot"), dynamic.bool(state.bot)),
    #(dynamic.string("country_code"), case state.country_code {
      Some(code) -> dynamic.string(code)
      None -> null()
    }),
  ]
}

fn enrich(state: State, data: Dynamic) -> Dynamic {
  case is_map(data) {
    True -> merge(data, dynamic.properties(session_facts(state)))
    False -> data
  }
}

fn null() -> Dynamic {
  atom.to_dynamic(atom.create("null"))
}

@external(erlang, "erlang", "is_map")
fn is_map(value: Dynamic) -> Bool

@external(erlang, "erlang", "is_binary")
fn is_string(value: Dynamic) -> Bool

@external(erlang, "maps", "merge")
fn merge(base: Dynamic, overrides: Dynamic) -> Dynamic

fn raw_snowflake(
  fields: dict.Dict(String, RawJson),
  key: String,
) -> Result(Option(Int), Nil) {
  case dict.get(fields, key) {
    Error(Nil) -> Ok(None)
    Ok(raw) ->
      case raw_json.to_bits(raw) {
        <<"null">> -> Ok(None)
        _ -> raw_json.snowflake(raw) |> result.map(Some)
      }
  }
}

fn present(fields: dict.Dict(String, RawJson), key: String) -> Bool {
  case dict.get(fields, key) {
    Ok(raw) -> raw_json.to_bits(raw) != <<"null">>
    Error(Nil) -> False
  }
}

pub fn signal(state: State, data: RawJson) -> Nil {
  case raw_json.fields(data) {
    Error(Nil) -> Nil
    Ok(fields) -> {
      let fields = dict.from_list(fields)
      case
        raw_snowflake(fields, "guild_id"),
        raw_snowflake(fields, "channel_id"),
        present(fields, "to") && present(fields, "data")
      {
        Ok(Some(guild)), Ok(Some(_)), True -> {
          let typed =
            result.is_ok(result.try(dict.get(fields, "to"), raw_json.string))
            && result.is_ok(result.try(
              dict.get(fields, "data"),
              raw_json.fields,
            ))
          case typed, id.guild_id(guild) {
            True, Ok(guild_id) -> {
              guilds.send_op(state, guild_id, VoiceSignal(data))
              Nil
            }
            _, _ -> Nil
          }
        }
        Ok(None), Ok(Some(channel)), True ->
          case dict.get(state.calls, channel) {
            Ok(Watched(..)) ->
              case client_update.signal(data) {
                Ok(found) -> voice_client.voice_signal(state.me, found)
                Error(Nil) -> Nil
              }
            _ -> Nil
          }
        _, _, _ -> Nil
      }
    }
  }
}

pub fn request_members(state: State, op: ClientOp, data: Dynamic) -> Nil {
  case
    target_guilds(op, data) != [] && member_requests_limit.admit(state.user_id)
  {
    True -> {
      let _ = forward(state, op, data)
      Nil
    }
    False -> Nil
  }
}

pub fn forward(state: State, op: ClientOp, data: Dynamic) -> Int {
  let members_only = case op {
    LazyRequest(_) -> False
    _ -> True
  }
  target_guilds(op, data)
  |> list.count(fn(guild) {
    case
      members_only && visits.is_visiting(state.visits, guild),
      id.guild_id(guild)
    {
      False, Ok(guild_id) -> guilds.send_op(state, guild_id, op)
      _, _ -> False
    }
  })
}

pub fn guild_counts(state: State, data: Dynamic) -> Nil {
  let targets =
    decode.run(data, many_guilds())
    |> result.unwrap([])
    |> list.unique
    |> list.sort(int.compare)
    |> list.take(max_guild_ids)
    |> list.filter_map(fn(guild) {
      case visits.is_visiting(state.visits, guild), id.guild_id(guild) {
        False, Ok(guild_id) ->
          links.linked_ref(state.links, guild_id) |> option.to_result(Nil)
        _, _ -> Error(Nil)
      }
    })
  let me = state.me
  let user = state.user_id
  let _ =
    process.spawn_unlinked(fn() {
      let reply = process.new_subject()
      list.each(targets, fn(guild_ref) {
        core_guild.send(guild_ref, core_guild.CountsQuery(user, reply))
      })
      let entries =
        collect_counts(
          reply,
          list.length(targets),
          clock.monotonic_ms() + counts_timeout_ms(),
          [],
        )
      relay.send(
        [me],
        session.Dispatch("GUILD_COUNTS_UPDATE", counts_payload(entries, data)),
      )
    })
  Nil
}

fn collect_counts(
  reply: Subject(core_guild.GuildCounts),
  left: Int,
  deadline_ms: Int,
  acc: List(json.Json),
) -> List(json.Json) {
  case left {
    0 -> list.reverse(acc)
    _ ->
      case
        process.receive(reply, int.max(0, deadline_ms - clock.monotonic_ms()))
      {
        Ok(found) ->
          collect_counts(reply, left - 1, deadline_ms, [
            json.object([
              #("guild_id", id.guild_json(found.guild)),
              #("member_count", json.int(found.member_count)),
              #("online_count", json.int(found.online_count)),
            ]),
            ..acc
          ])
        Error(Nil) -> list.reverse(acc)
      }
  }
}

pub const channel_counts_event = "CHANNEL_MEMBER_COUNTS_UPDATE"

pub fn channel_counts(state: State, data: Dynamic) -> Bool {
  let target =
    decode.run(data, single_guild())
    |> result.unwrap([])
    |> list.find_map(fn(guild) {
      case visits.is_visiting(state.visits, guild), id.guild_id(guild) {
        False, Ok(guild_id) ->
          links.linked_ref(state.links, guild_id) |> option.to_result(Nil)
        _, _ -> Error(Nil)
      }
    })
  case target {
    Error(Nil) -> False
    Ok(guild_ref) -> {
      let me = state.me
      let _ =
        process.spawn_unlinked(fn() {
          let inbox = process.new_subject()
          core_guild.send(
            guild_ref,
            core_guild.SessionOp(
              session.SessionRef(..me, inbox:),
              session.RequestChannelMemberCounts(data),
            ),
          )
          let payload = case process.receive(inbox, counts_timeout_ms()) {
            Ok(session.Dispatch(_, payload)) -> payload
            _ -> empty_counts(data)
          }
          process.send(
            me.inbox,
            session.Dispatch(channel_counts_event, payload),
          )
        })
      True
    }
  }
}

pub fn empty_counts(data: Dynamic) -> RawJson {
  counts_payload([], data)
}

fn counts_payload(entries: List(json.Json), data: Dynamic) -> RawJson {
  let nonce =
    decode.run(data, decode.at(["nonce"], decode.string))
    |> result.try(fn(text) {
      case string.byte_size(text) {
        size if size > 0 && size <= max_nonce_bytes -> Ok(text)
        _ -> Error([])
      }
    })
  let fields = case nonce {
    Ok(text) -> [#("nonce", json.string(text))]
    Error(_) -> []
  }
  json.object([#("counts", json.preprocessed_array(entries)), ..fields])
  |> raw_json.from_json
}

fn target_guilds(op: ClientOp, data: Dynamic) -> List(Int) {
  let decoder = case op {
    LazyRequest(_) -> subscribed_guilds()
    RequestGuildMembers(_) ->
      decode.one_of(
        many_guilds()
          |> decode.then(fn(guilds) {
            case guilds {
              [] -> decode.failure([], "guild_ids")
              _ -> decode.success(guilds)
            }
          }),
        [single_guild()],
      )
    _ -> single_guild()
  }
  decode.run(data, decoder)
  |> result.map(list.unique)
  |> result.unwrap([])
}

fn single_guild() -> Decoder(List(Int)) {
  decode.optional_field(
    "guild_id",
    [],
    decode.one_of(edge.snowflake() |> decode.map(fn(guild) { [guild] }), [
      decode.success([]),
    ]),
    decode.success,
  )
}

fn many_guilds() -> Decoder(List(Int)) {
  decode.optional_field(
    "guild_ids",
    [],
    decode.one_of(
      decode.list(
        decode.one_of(edge.snowflake() |> decode.map(Some), [
          decode.success(None),
        ]),
      )
        |> decode.map(option.values),
      [decode.success([])],
    ),
    decode.success,
  )
}

fn subscribed_guilds() -> Decoder(List(Int)) {
  decode.optional_field(
    "subscriptions",
    [],
    decode.one_of(
      decode.dict(decode.string, decode.dynamic)
        |> decode.map(fn(entries) {
          entries
          |> dict.keys
          |> list.filter_map(fn(key) {
            id.parse(key) |> result.replace_error(Nil)
          })
        }),
      [decode.success([])],
    ),
    decode.success,
  )
}
