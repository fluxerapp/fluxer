// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild.{type GuildRef}
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/json_scan
import fluxer_gateway/core/presence.{type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/session.{
  type Identify, type IdentifyFlags, type SessionId, type SessionRef,
}
import fluxer_gateway/core/user.{type User}
import fluxer_gateway/gateway/sharding
import fluxer_gateway/presence/client.{type PresenceLink}
import fluxer_gateway/session/payload
import fluxer_gateway/session/pending_presences.{type PendingPresences}
import fluxer_gateway/session/presence_view.{type PresenceView}
import fluxer_gateway/session/ready.{type ReadyData, ReadyData}
import fluxer_gateway/session/relations.{type Relations, DmChannel}
import fluxer_gateway/session/replay_buffer.{type Entry}
import fluxer_gateway/session/subscriptions.{type GuildSub}
import fluxer_gateway/session/visits.{type Visits, Visits}
import gleam/bit_array
import gleam/bool
import gleam/crypto
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}

pub type Seed {
  Seed(
    id: SessionId,
    user: User,
    staff: Bool,
    token_hash: BitArray,
    auth_hash: BitArray,
    version: Int,
    flags: IdentifyFlags,
    shard: Option(#(Int, Int)),
    e2ee_capable: Bool,
    country_code: Option(String),
    ignored_events: List(String),
    initial_guild: Option(GuildId),
    status: Status,
    resume_status: Status,
    afk: Bool,
    mobile: Bool,
    custom_status: Option(RawJson),
    settings_invisible: Bool,
    guilds: List(GuildId),
    visits: Visits,
    relations: Relations,
    ready: Option(ReadyData),
    seq: Int,
    ack_seq: Int,
    replay_floor: Int,
    buffer: List(Entry),
    active: Set(Int),
    subscriptions: Dict(Int, GuildSub),
    health: Dict(Int, Bool),
    presence_view: PresenceView,
    transferred: Bool,
    moved: Option(Moved),
  )
}

pub type Moved {
  Moved(
    source: SessionRef,
    linked: List(GuildRef),
    presence: Option(PresenceLink),
    presence_targets: Set(Int),
    dm_mutual: Dict(Int, Set(Int)),
    suppress_presences: Bool,
    pending_presences: PendingPresences,
  )
}

const epoch_four_size = 32

@external(erlang, "erlang", "tuple_size")
fn size(seed: Seed) -> Int

@external(erlang, "erlang", "append_element")
fn append_moved(seed: Seed, moved: Option(Moved)) -> Seed

@external(erlang, "erlang", "delete_element")
fn delete_field(index: Int, seed: Seed) -> Seed

pub fn from_wire(seed: Seed) -> Seed {
  case size(seed) == epoch_four_size {
    True -> append_moved(seed, None)
    False -> seed
  }
}

pub fn for_peer(seed: Seed, speaks_moves: Bool) -> Seed {
  case speaks_moves {
    True -> seed
    False -> delete_field(epoch_four_size + 1, Seed(..seed, moved: None))
  }
}

pub type SeedError {
  MissingUser
  ShardingRequired
}

const max_guilds_per_shard = 2500

const read_state_thread_flag = 2

const internal_fields = [
  "visit_guild_ids", "operator_visit_guild_ids", "adult_content_allowed",
  "account_tier", "ip_key", "guilds",
]

fn hash_token(token: String) -> BitArray {
  crypto.hash(crypto.Sha256, bit_array.from_string(token))
}

pub fn token_matches(seed_hash: BitArray, token: String) -> Bool {
  let hashed = hash_token(token)
  bit_array.byte_size(hashed) == bit_array.byte_size(seed_hash)
  && crypto.secure_compare(hashed, seed_hash)
}

pub fn from_api(
  session_id: SessionId,
  identify: Identify,
  version: Int,
  data: BitArray,
) -> Result(Seed, SeedError) {
  let top =
    raw_json.wrap(data)
    |> raw_json.fields
    |> result.map(fn(fields) { list.reverse(fields) |> dict.from_list })
    |> result.unwrap(dict.new())
  use #(account, staff) <- result.try(decode_user(top))
  let bot = user.is_bot(account)
  let api_guilds = guild_ids(top)
  let in_shard = retain_in_shard(api_guilds, identify.shard)
  use <- bool.guard(
    bot && list.length(in_shard) > max_guilds_per_shard,
    Error(ShardingRequired),
  )
  let settings = user_settings(top)
  let requested = identify.presence |> option.map(identify_presence)
  let status = initial_status(requested, settings.0)
  let custom_status =
    option.lazy_or(settings.1, fn() {
      option.then(requested, fn(p) { p.custom_status })
    })
  let flags = identify.flags
  let capable = flags.guild_visits && !bot
  let visits = parse_visits(top, capable, in_shard, identify.shard)
  let relationship_rows = relationship_rows(top)
  let channel_rows =
    field(top, "private_channels", decode.list(channel_decoder()), [])
  let ready_users =
    list.flatten([
      list.filter_map(relationship_rows, fn(row) {
        option.to_result(row.user, Nil)
      }),
      list.flat_map(channel_rows, fn(row) { row.users }),
    ])
    |> dict.from_list
  let ready_fields =
    top
    |> dict.to_list
    |> list.filter(fn(entry) {
      !list.contains(internal_fields, entry.0) && entry.0 != "relationships"
    })
    |> list.map(fn(entry) {
      case entry.0 {
        "read_states" -> #(
          "read_states",
          filter_read_states(entry.1, flags.channel_threads || bot),
        )
        _ -> entry
      }
    })
  let relations =
    relations.new(
      relationships: relationship_rows
        |> list.filter_map(fn(row) {
          case row.id, row.kind {
            Some(other), Some(kind) -> Ok(#(other, kind))
            _, _ -> Error(Nil)
          }
        })
        |> dict.from_list,
      channels: channel_rows
        |> list.filter_map(fn(row) {
          case row.id {
            Some(channel_id) ->
              Ok(#(
                channel_id,
                DmChannel(kind: row.kind, recipients: row.recipients),
              ))
            None -> Error(Nil)
          }
        })
        |> dict.from_list,
    )
  Ok(Seed(
    id: session_id,
    user: account,
    staff:,
    token_hash: hash_token(identify.token),
    auth_hash: auth_hash(top),
    version:,
    flags:,
    shard: identify.shard,
    e2ee_capable: identify.e2ee_capable,
    country_code: field(top, "geoip_country_code", non_empty_string(), None),
    ignored_events: identify.ignored_events,
    initial_guild: identify.initial_guild_id,
    status:,
    resume_status: status,
    afk: option.map(requested, fn(p) { p.afk }) |> option.unwrap(False),
    mobile: identify.mobile,
    custom_status:,
    settings_invisible: False,
    guilds: list.append(
      in_shard,
      visits.guilds
        |> dict.keys
        |> list.filter_map(fn(guild) {
          id.guild_id(guild) |> result.replace_error(Nil)
        }),
    ),
    visits:,
    relations:,
    ready: Some(ReadyData(
      fields: ready_fields,
      relationships: list.map(relationship_rows, fn(row) { row.stripped }),
      users: ready_users,
      sessions: ready.no_sessions(),
    )),
    seq: 0,
    ack_seq: 0,
    replay_floor: 0,
    buffer: [],
    active: set.new(),
    subscriptions: dict.new(),
    health: dict.new(),
    presence_view: presence_view.new(),
    transferred: False,
    moved: None,
  ))
}

fn field(
  top: Dict(String, RawJson),
  key: String,
  decoder: Decoder(a),
  default: a,
) -> a {
  case dict.get(top, key) {
    Ok(value) -> raw_json.read(value, decoder) |> result.unwrap(default)
    Error(Nil) -> default
  }
}

fn decode_user(top: Dict(String, RawJson)) -> Result(#(User, Bool), SeedError) {
  dict.get(top, "user")
  |> result.try(raw_json.read(_, user_decoder()))
  |> result.try(option.to_result(_, Nil))
  |> result.replace_error(MissingUser)
}

fn user_decoder() -> Decoder(Option(#(User, Bool))) {
  use id_text <- decode.field("id", decode.string)
  use partial <- decode.then(ready.user_decoder())
  use bot <- decode.then(payload.bool_field("bot"))
  use flags <- decode.then(payload.int_field("flags"))
  use restricted <- decode.then(payload.bool_field("profile_restricted"))
  use staff_value <- decode.then(decode.optional_field(
    "is_staff",
    None,
    decode.one_of(decode.bool |> decode.map(Some), [
      decode.string
        |> decode.map(fn(text) {
          case text {
            "true" -> Some(True)
            "false" -> Some(False)
            _ -> None
          }
        }),
      decode.success(None),
    ]),
    decode.success,
  ))
  case id.parse_user_id(id_text) {
    Error(_) -> decode.success(None)
    Ok(user_id) -> {
      let flag_bits = option.unwrap(flags, 0)
      let staff = case staff_value {
        Some(value) -> value
        None -> int.bitwise_and(flag_bits, 1) != 0
      }
      let wire =
        payload.with_field(
          payload.with_field(
            partial.1,
            "profile_restricted",
            json.bool(restricted),
          ),
          "is_staff",
          json.bool(staff),
        )
      decode.success(
        Some(#(
          user.new(id: user_id, bot:, flags: flag_bits, restricted:, wire:),
          staff,
        )),
      )
    }
  }
}

fn guild_ids(top: Dict(String, RawJson)) -> List(GuildId) {
  field(
    top,
    "guilds",
    decode.list(decode.one_of(payload.id_field("id"), [decode.success(None)])),
    [],
  )
  |> option.values
  |> list.filter_map(fn(guild) {
    id.guild_id(guild) |> result.replace_error(Nil)
  })
}

pub fn retain_in_shard(
  guilds: List(GuildId),
  shard: Option(#(Int, Int)),
) -> List(GuildId) {
  case shard {
    None | Some(#(0, 1)) -> guilds
    Some(pair) ->
      list.filter(guilds, fn(guild) { sharding.guild_in_shard(guild, pair) })
  }
}

fn user_settings(
  top: Dict(String, RawJson),
) -> #(Option(String), Option(RawJson)) {
  let fields =
    dict.get(top, "user_settings")
    |> result.try(raw_json.fields)
    |> result.unwrap([])
  let status =
    list.key_find(fields, "status")
    |> result.try(raw_json.read(_, decode.string))
    |> option.from_result
  let custom =
    list.key_find(fields, "custom_status")
    |> result.try(fn(value) {
      raw_json.fields(value) |> result.replace(raw_json.detach(value))
    })
    |> option.from_result
  #(status, custom)
}

fn object_raw() -> Decoder(RawJson) {
  decode.dict(decode.string, decode.dynamic)
  |> decode.then(fn(_) { payload.raw() })
}

pub type IdentifyPresence {
  IdentifyPresence(
    status: Option(String),
    afk: Bool,
    custom_status: Option(RawJson),
  )
}

fn identify_presence(value: Dynamic) -> IdentifyPresence {
  let decoder = {
    use status <- decode.optional_field(
      "status",
      Some("online"),
      decode.one_of(decode.string |> decode.map(Some), [decode.success(None)]),
    )
    use afk <- decode.then(payload.bool_field("afk"))
    use custom_status <- decode.optional_field(
      "custom_status",
      None,
      decode.one_of(object_raw() |> decode.map(Some), [decode.success(None)]),
    )
    decode.success(IdentifyPresence(status:, afk:, custom_status:))
  }
  decode.run(value, decoder)
  |> result.unwrap(IdentifyPresence(None, False, None))
}

fn initial_status(
  requested: Option(IdentifyPresence),
  stored: Option(String),
) -> Status {
  let stored = option.then(stored, valid_status) |> option.unwrap("online")
  let asked =
    option.then(requested, fn(p) { option.then(p.status, valid_status) })
  let chosen = case asked {
    None -> stored
    Some("online") if stored != "online" -> stored
    Some(status) -> status
  }
  presence.status_from_wire(chosen) |> result.unwrap(presence.Online)
}

fn valid_status(text: String) -> Option(String) {
  case presence.status_from_wire(text) {
    Ok(_) if text != "" -> Some(text)
    _ -> None
  }
}

fn auth_hash(top: Dict(String, RawJson)) -> BitArray {
  case
    field(top, "auth_session_id_hash", decode.optional(decode.string), None)
  {
    Some(encoded) -> base64url_decode(encoded) |> result.unwrap(<<>>)
    None -> <<>>
  }
}

pub fn base64url_decode(encoded: String) -> Result(BitArray, Nil) {
  bit_array.base64_url_decode(encoded)
}

fn non_empty_string() -> Decoder(Option(String)) {
  decode.one_of(
    decode.string
      |> decode.map(fn(text) {
        case text {
          "" -> None
          _ -> Some(text)
        }
      }),
    [decode.success(None)],
  )
}

fn parse_visits(
  top: Dict(String, RawJson),
  capable: Bool,
  members: List(GuildId),
  shard: Option(#(Int, Int)),
) -> Visits {
  let ids = fn(key) {
    field(top, key, decode.list(payload.id()), []) |> option.values
  }
  let operators = set.from_list(ids("operator_visit_guild_ids"))
  let member_ids = list.map(members, id.guild_id_to_int) |> set.from_list
  let visited = case capable {
    False -> dict.new()
    True ->
      ids("visit_guild_ids")
      |> list.filter_map(fn(guild) {
        id.guild_id(guild) |> result.replace_error(Nil)
      })
      |> retain_in_shard(shard)
      |> list.map(id.guild_id_to_int)
      |> list.filter(fn(guild) { !set.contains(member_ids, guild) })
      |> list.map(fn(guild) { #(guild, set.contains(operators, guild)) })
      |> dict.from_list
  }
  Visits(
    guilds: visited,
    capable:,
    adult_content_allowed: field(
      top,
      "adult_content_allowed",
      decode.one_of(decode.bool, [decode.success(False)]),
      False,
    ),
    tier: case field(top, "account_tier", decode.string, "") {
      "verified" -> "verified"
      "unverified" -> "unverified"
      _ -> "unclaimed"
    },
    ip_key: field(top, "ip_key", non_empty_string(), None),
  )
}

type RelationshipRow {
  RelationshipRow(
    id: Option(Int),
    kind: Option(Int),
    user: Option(#(Int, RawJson)),
    stripped: RawJson,
  )
}

fn relationship_rows(top: Dict(String, RawJson)) -> List(RelationshipRow) {
  dict.get(top, "relationships")
  |> result.try(raw_json.items)
  |> result.try(list.try_map(_, relationship_row))
  |> result.unwrap([])
}

fn relationship_row(row: RawJson) -> Result(RelationshipRow, Nil) {
  use fields <- result.try(raw_json.fields(row))
  use #(row_id, kind, user) <- result.map(raw_json.read(
    row,
    relationship_facts(),
  ))
  let row_id = option.or(row_id, option.map(user, fn(pair) { pair.0 }))
  let without_user =
    list.reverse(fields) |> dict.from_list |> dict.delete("user")
  let with_id = case dict.has_key(without_user, "id"), user {
    False, Some(#(user_id, _)) ->
      dict.insert(without_user, "id", raw_json.from_json(id.json(user_id)))
    _, _ -> without_user
  }
  RelationshipRow(
    id: row_id,
    kind:,
    user:,
    stripped: raw_json.object(dict.to_list(with_id)) |> raw_json.flatten,
  )
}

fn relationship_facts() -> Decoder(
  #(Option(Int), Option(Int), Option(#(Int, RawJson))),
) {
  use row_id <- decode.then(payload.id_field("id"))
  use kind <- decode.then(payload.int_field("type"))
  use user <- decode.then(decode.optional_field(
    "user",
    None,
    decode.one_of(ready.user_decoder() |> decode.map(Some), [
      decode.success(None),
    ]),
    decode.success,
  ))
  decode.success(#(row_id, kind, user))
}

type ChannelRow {
  ChannelRow(
    id: Option(Int),
    kind: Int,
    recipients: Set(Int),
    users: List(#(Int, RawJson)),
  )
}

fn channel_decoder() -> Decoder(ChannelRow) {
  use event <- decode.then(relations.channel_decoder())
  use users <- decode.optional_field(
    "recipients",
    [],
    decode.one_of(
      decode.list(
        decode.one_of(ready.user_decoder() |> decode.map(Some), [
          decode.success(None),
        ]),
      )
        |> decode.map(option.values),
      [decode.success([])],
    ),
  )
  let kind = option.unwrap(event.kind, 0)
  case kind {
    1 | 3 ->
      decode.success(ChannelRow(
        id: event.id,
        kind:,
        recipients: option.unwrap(event.recipients, set.new()),
        users:,
      ))
    _ ->
      decode.success(
        ChannelRow(id: None, kind:, recipients: set.new(), users: []),
      )
  }
}

fn filter_read_states(value: RawJson, capable: Bool) -> RawJson {
  case capable {
    True -> value
    False ->
      case json_scan.array(raw_json.to_bits(value)) {
        Error(Nil) -> value
        Ok(rows) ->
          case filter_rows(rows, False, False, []) {
            #(False, _) -> value
            #(True, pieces) ->
              [<<"[">>, ..list.reverse([<<"]">>, ..pieces])]
              |> bit_array.concat
              |> raw_json.wrap
          }
      }
  }
}

type ReadStateRow {
  Unchanged
  Stripped(head: BitArray, tail: BitArray)
  Dropped
}

fn filter_rows(
  rows: List(BitArray),
  started: Bool,
  changed: Bool,
  pieces: List(BitArray),
) -> #(Bool, List(BitArray)) {
  case rows {
    [] -> #(changed, pieces)
    [row, ..rest] -> {
      let separated = case started {
        True -> [<<",">>, ..pieces]
        False -> pieces
      }
      case filter_read_state(row) {
        Unchanged -> filter_rows(rest, True, changed, [row, ..separated])
        Stripped(head:, tail:) ->
          filter_rows(rest, True, True, [tail, head, ..separated])
        Dropped -> filter_rows(rest, started, True, pieces)
      }
    }
  }
}

fn filter_read_state(row: BitArray) -> ReadStateRow {
  case json_scan.locate(row, "flags") {
    Ok(json_scan.Found(value_start:, value_end:, cut_start:, cut_end:)) ->
      case read_flags(row, value_start, value_end) {
        Ok(bits) ->
          case int.bitwise_and(bits, read_state_thread_flag) {
            0 -> stripped(row, cut_start, cut_end)
            _ -> Dropped
          }
        Error(Nil) -> stripped(row, cut_start, cut_end)
      }
    _ -> Unchanged
  }
}

fn read_flags(bytes: BitArray, start: Int, end: Int) -> Result(Int, Nil) {
  case bit_array.slice(bytes, start, end - start) {
    Ok(<<"-", digits:bytes>>) -> natural(digits, 0) |> result.map(int.negate)
    Ok(digits) -> natural(digits, 0)
    Error(Nil) -> Error(Nil)
  }
}

fn natural(digits: BitArray, value: Int) -> Result(Int, Nil) {
  case digits {
    <<digit, rest:bytes>> if digit >= 48 && digit <= 57 ->
      more_digits(rest, value * 10 + digit - 48)
    _ -> Error(Nil)
  }
}

fn more_digits(digits: BitArray, value: Int) -> Result(Int, Nil) {
  case digits {
    <<>> -> Ok(value)
    _ -> natural(digits, value)
  }
}

fn stripped(bytes: BitArray, cut_start: Int, cut_end: Int) -> ReadStateRow {
  let size = bit_array.byte_size(bytes)
  case
    bit_array.slice(bytes, 0, cut_start),
    bit_array.slice(bytes, cut_end, size - cut_end)
  {
    Ok(head), Ok(tail) -> Stripped(head:, tail:)
    _, _ -> Unchanged
  }
}
