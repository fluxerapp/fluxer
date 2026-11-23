// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import bouncer_proto/codec_range.{type CodecRange, CodecRange}
import bouncer_proto/key
import bouncer_proto/message.{type MoveId}
import fluxer_gateway/core/guild.{type GuildRequest}
import fluxer_gateway/core/id
import fluxer_gateway/core/json_scan
import fluxer_gateway/core/member
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session.{type SessionRef, SessionRef}
import fluxer_gateway/guild/data.{type Loaded, Loaded}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/handoff/wire_v1.{
  type GuildBulkV1, type GuildTailV1, type SessionV1, GuildBulkV1, GuildTailV1,
  InboxV1, SessionV1,
}
import fluxer_gateway/guild/handoff/wire_v2.{
  type GuildBulkV2, type GuildTailV2, GuildBulkV2, GuildTailV2,
}
import fluxer_gateway/guild/handoff/wire_v3.{
  type GuildDeltaV3, type GuildTailV3, GuildDeltaV3, GuildTailV3,
}
import fluxer_gateway/guild/inbox
import fluxer_gateway/guild/sessions.{type Sessions}
import fluxer_gateway/handoff/codec.{type Codec, type CodecError, Codec}
import fluxer_gateway/threads/store.{type Handoff}
import fluxer_gateway/voice/guild_handler.{type GuildVoice, type VoiceRequest}
import gleam/bit_array
import gleam/dict
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result

pub const oldest_read = 1

pub const current = 3

pub const precopy_from = 3

pub type Tail {
  Tail(
    loaded: Loaded,
    sessions: List(SessionRef),
    threads: Handoff,
    voice: GuildVoice,
    table: Option(Sessions),
    staged: Option(Staged),
  )
}

pub type Staged {
  Staged(move: MoveId, round: Int, removed: List(Int))
}

pub type Delta {
  Delta(move: MoveId, round: Int, changed: Loaded, removed: List(Int))
}

pub type Inbound {
  Public(request: GuildRequest)
  Private(control: inbox.Control)
  VoiceOp(request: VoiceRequest)
}

pub fn range() -> CodecRange {
  range_up_to(current)
}

pub fn range_up_to(newest: Int) -> CodecRange {
  CodecRange(
    ns: key.guild,
    oldest_read: oldest_read,
    max_read: newest,
    max_write: newest,
  )
}

pub fn tail() -> Codec(Tail) {
  tail_with(range())
}

pub fn tail_with(range: CodecRange) -> Codec(Tail) {
  Codec(
    ns: key.guild,
    part: blob.Tail,
    range:,
    encode_body: encode_tail,
    decode_body: decode_tail,
  )
}

fn precopy_range() -> CodecRange {
  CodecRange(
    ns: key.guild,
    oldest_read: precopy_from,
    max_read: current,
    max_write: current,
  )
}

pub fn bulk() -> Codec(Loaded) {
  Codec(
    ns: key.guild,
    part: blob.Tail,
    range: precopy_range(),
    encode_body: fn(value, version) {
      case version >= precopy_from {
        True -> Ok(codec.term_body(bulk_record(value, 0)))
        False -> Error(codec.UnwritableVersion(version))
      }
    },
    decode_body: fn(_version, body) {
      use record <- result.try(codec.term_record(body, "guild_bulk_v2", 9))
      loaded_of(record)
    },
  )
}

pub fn delta() -> Codec(Delta) {
  Codec(
    ns: key.guild,
    part: blob.Tail,
    range: precopy_range(),
    encode_body: fn(value: Delta, version) {
      case version >= precopy_from {
        True -> Ok(codec.term_body(delta_record(value)))
        False -> Error(codec.UnwritableVersion(version))
      }
    },
    decode_body: fn(_version, body) {
      use record <- result.try(codec.term_record(body, "guild_delta_v3", 4))
      delta_of(record)
    },
  )
}

fn delta_record(value: Delta) -> GuildDeltaV3 {
  GuildDeltaV3(
    move: value.move,
    round: value.round,
    changed: bulk_record(value.changed, 0),
    removed: value.removed,
  )
}

fn delta_of(record: GuildDeltaV3) -> Result(Delta, CodecError) {
  use changed <- result.map(loaded_of(record.changed))
  Delta(
    move: record.move,
    round: record.round,
    changed:,
    removed: record.removed,
  )
}

pub fn inbox() -> Codec(Inbound) {
  Codec(
    ns: key.guild,
    part: blob.Inbox,
    range: range(),
    encode_body: fn(value, _version) {
      Ok(codec.term_body(InboxV1(codec.term_body(value))))
    },
    decode_body: fn(_version, body) {
      use InboxV1(message) <- result.try(codec.term_record(body, "inbox_v1", 1))
      codec.term_of(message)
    },
  )
}

pub fn upgrade_bulk_v1(old: GuildBulkV1) -> GuildBulkV2 {
  GuildBulkV2(
    guild_id: old.guild_id,
    event_index: old.event_index,
    guild: old.guild,
    roles: old.roles,
    channels: old.channels,
    emojis: old.emojis,
    stickers: old.stickers,
    member_count: list.length(old.members),
    members: join_array(old.members),
  )
}

pub fn downgrade_bulk_v2(new: GuildBulkV2) -> GuildBulkV1 {
  GuildBulkV1(
    guild_id: new.guild_id,
    event_index: new.event_index,
    guild: new.guild,
    roles: new.roles,
    channels: new.channels,
    emojis: new.emojis,
    stickers: new.stickers,
    members: json_scan.array(new.members) |> result.unwrap([]),
  )
}

pub fn upgrade_tail_v1(old: GuildTailV1) -> GuildTailV2 {
  GuildTailV2(
    guild_id: old.guild_id,
    event_index: old.event_index,
    loaded: option.map(old.loaded, upgrade_bulk_v1),
    sessions: old.sessions,
    threads: old.threads,
    voice: old.voice,
    delta: old.delta,
    table: old.table,
  )
}

pub fn downgrade_tail_v2(new: GuildTailV2) -> GuildTailV1 {
  GuildTailV1(
    guild_id: new.guild_id,
    event_index: new.event_index,
    loaded: option.map(new.loaded, downgrade_bulk_v2),
    sessions: new.sessions,
    threads: new.threads,
    voice: new.voice,
    delta: new.delta,
    table: new.table,
  )
}

pub fn upgrade_tail_v2(old: GuildTailV2) -> GuildTailV3 {
  GuildTailV3(
    guild_id: old.guild_id,
    event_index: old.event_index,
    loaded: old.loaded,
    staged: None,
    sessions: old.sessions,
    threads: old.threads,
    voice: old.voice,
    delta: old.delta,
    table: old.table,
  )
}

pub fn downgrade_tail_v3(new: GuildTailV3) -> Result(GuildTailV2, CodecError) {
  case new.staged {
    Some(_) -> Error(codec.UnwritableVersion(wire_v2.version))
    None ->
      Ok(GuildTailV2(
        guild_id: new.guild_id,
        event_index: new.event_index,
        loaded: new.loaded,
        sessions: new.sessions,
        threads: new.threads,
        voice: new.voice,
        delta: new.delta,
        table: new.table,
      ))
  }
}

pub fn read_tail(
  version: Int,
  body: BitArray,
) -> Result(GuildTailV3, CodecError) {
  case version {
    1 ->
      codec.term_record(body, "guild_tail_v1", 8)
      |> result.map(fn(old) { upgrade_tail_v2(upgrade_tail_v1(old)) })
    2 ->
      codec.term_record(body, "guild_tail_v2", 8)
      |> result.map(upgrade_tail_v2)
    3 -> codec.term_record(body, "guild_tail_v3", 9)
    other -> Error(codec.UnreadableVersion(other))
  }
}

pub fn write_tail(
  record: GuildTailV3,
  version: Int,
) -> Result(BitArray, CodecError) {
  case version {
    1 ->
      downgrade_tail_v3(record)
      |> result.map(fn(older) { codec.term_body(downgrade_tail_v2(older)) })
    2 -> downgrade_tail_v3(record) |> result.map(codec.term_body)
    3 -> Ok(codec.term_body(record))
    other -> Error(codec.UnwritableVersion(other))
  }
}

fn encode_tail(value: Tail, version: Int) -> Result(BitArray, CodecError) {
  let state = bulk_record(value.loaded, 0)
  let #(loaded, staged) = case value.staged {
    Some(found) -> #(
      None,
      Some(GuildDeltaV3(
        move: found.move,
        round: found.round,
        changed: state,
        removed: found.removed,
      )),
    )
    None -> #(Some(state), None)
  }
  GuildTailV3(
    guild_id: id.guild_id_to_int(data.guild_id(value.loaded.data)),
    event_index: 0,
    loaded:,
    staged:,
    sessions: list.map(value.sessions, session_record),
    threads: codec.term_body(value.threads),
    voice: codec.term_body(value.voice),
    delta: [],
    table: case value.table {
      Some(table) -> codec.term_body(table)
      None -> <<>>
    },
  )
  |> write_tail(version)
}

fn decode_tail(version: Int, body: BitArray) -> Result(Tail, CodecError) {
  use record <- result.try(read_tail(version, body))
  use #(loaded, staged) <- result.try(case record.loaded, record.staged {
    _, Some(found) ->
      loaded_of(found.changed)
      |> result.map(fn(loaded) {
        #(loaded, Some(Staged(found.move, found.round, found.removed)))
      })
    Some(bulk), None ->
      loaded_of(bulk) |> result.map(fn(loaded) { #(loaded, None) })
    None, None -> Error(codec.Malformed("guild tail without state"))
  })
  use sessions <- result.try(list.try_map(record.sessions, session_of))
  use threads <- result.try(codec.term_of(record.threads))
  use voice <- result.try(codec.term_of(record.voice))
  use table <- result.map(case record.table {
    <<>> -> Ok(None)
    body -> codec.term_of(body) |> result.map(Some)
  })
  Tail(loaded:, sessions:, threads:, voice:, table:, staged:)
}

pub fn bulk_record(loaded: Loaded, event_index: Int) -> GuildBulkV2 {
  let guild_data = loaded.data
  GuildBulkV2(
    guild_id: id.guild_id_to_int(data.guild_id(guild_data)),
    event_index:,
    guild: json_bytes(data.properties_json(guild_data, [])),
    roles: json_bytes(data.roles_json(guild_data)),
    channels: json_bytes(data.channels_json(guild_data, guild_data.channels)),
    emojis: raw_json.to_bits(guild_data.emojis),
    stickers: raw_json.to_bits(guild_data.stickers),
    member_count: guild.member_count(guild_data.facts),
    members: join_array(
      list.map(loaded.members, fn(row) { raw_json.to_bits(member.wire(row)) }),
    ),
  )
}

pub fn loaded_of(record: GuildBulkV2) -> Result(Loaded, CodecError) {
  use guild_id <- result.try(
    id.guild_id(record.guild_id)
    |> result.replace_error(codec.Malformed("guild id")),
  )
  [
    #("guild", data_decode.Whole(record.guild)),
    #("roles", data_decode.Whole(record.roles)),
    #("channels", data_decode.Whole(record.channels)),
    #("emojis", data_decode.Whole(record.emojis)),
    #("stickers", data_decode.Whole(record.stickers)),
    #("members", data_decode.Whole(record.members)),
  ]
  |> dict.from_list
  |> data_decode.collections(guild_id, _)
  |> result.map(fn(loaded) {
    Loaded(
      ..loaded,
      data: data.set_member_count(loaded.data, record.member_count),
    )
  })
  |> result.replace_error(codec.Malformed("guild bulk"))
}

fn session_record(ref: SessionRef) -> SessionV1 {
  SessionV1(
    session_id: session.session_id_to_string(ref.id),
    user_id: id.user_id_to_int(ref.user),
    inbox: ref.inbox,
  )
}

fn session_of(record: SessionV1) -> Result(SessionRef, CodecError) {
  case
    session.session_id_from_string(record.session_id),
    id.user_id(record.user_id)
  {
    Ok(session_id), Ok(user) ->
      Ok(SessionRef(id: session_id, user:, inbox: record.inbox))
    _, _ -> Error(codec.Malformed("session"))
  }
}

fn json_bytes(value: json.Json) -> BitArray {
  value |> json.to_string |> bit_array.from_string
}

fn join_array(rows: List(BitArray)) -> BitArray {
  bit_array.concat([<<"[">>, ..join_rows(rows, [])])
}

fn join_rows(rows: List(BitArray), acc: List(BitArray)) -> List(BitArray) {
  case rows {
    [] -> list.reverse([<<"]">>, ..acc])
    [row] -> list.reverse([<<"]">>, row, ..acc])
    [row, ..rest] -> join_rows(rest, [<<",">>, row, ..acc])
  }
}
