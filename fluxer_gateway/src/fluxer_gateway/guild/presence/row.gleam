// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type GuildId, type UserId}
import fluxer_gateway/core/member.{type Member}
import fluxer_gateway/core/presence.{type Presence, type Status}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/core/user
import fluxer_gateway/util/iso8601
import gleam/dynamic/decode
import gleam/json.{type Json}
import gleam/option.{type Option, None, Some}

pub opaque type Row {
  Row(
    status: Status,
    mobile: Bool,
    afk: Bool,
    custom_status: Option(RawJson),
    user_wire: RawJson,
  )
}

pub type Display {
  Display(
    status: Status,
    mobile: Bool,
    afk: Bool,
    custom_status: Option(BitArray),
  )
}

pub fn from_presence(source: Presence, owner: Member) -> Row {
  let owner_user = member.user(owner)
  let status = presence.visible_status(presence.status(source))
  let custom_status = case status, user.is_restricted(owner_user) {
    presence.Offline, _ -> None
    _, True -> None
    _, False -> option.then(presence.custom_status(source), unexpired)
  }
  Row(
    status:,
    mobile: presence.mobile(source),
    afk: presence.afk(source),
    custom_status:,
    user_wire: user.wire(owner_user),
  )
}

fn unexpired(raw: RawJson) -> Option(RawJson) {
  let expiry =
    decode.optional_field(
      "expires_at",
      None,
      decode.one_of(decode.map(decode.string, Some), [decode.success(None)]),
      decode.success,
    )
  case raw_json.read(raw, expiry) {
    Ok(Some(text)) ->
      case iso8601.parse_ms(text) {
        Ok(at) ->
          case at <= clock.system_ms() {
            True -> None
            False -> Some(raw)
          }
        Error(Nil) -> Some(raw)
      }
    Ok(None) -> Some(raw)
    Error(_) -> None
  }
}

pub fn same(a: Row, b: Row) -> Bool {
  display(a) == display(b)
  && raw_json.to_bits(a.user_wire) == raw_json.to_bits(b.user_wire)
}

pub fn display(row: Row) -> Display {
  Display(
    status: row.status,
    mobile: row.mobile,
    afk: row.afk,
    custom_status: option.map(row.custom_status, raw_json.to_bits),
  )
}

pub fn presence_display(source: Presence) -> Display {
  let status = presence.visible_status(presence.status(source))
  Display(
    status:,
    mobile: presence.mobile(source),
    afk: presence.afk(source),
    custom_status: option.map(presence.custom_status(source), raw_json.to_bits),
  )
}

pub fn offline_display() -> Display {
  Display(
    status: presence.Offline,
    mobile: False,
    afk: False,
    custom_status: None,
  )
}

pub fn to_presence(row: Row, user: UserId) -> Presence {
  presence.new(
    user:,
    status: row.status,
    mobile: row.mobile,
    afk: row.afk,
    custom_status: row.custom_status,
    wire: raw_json.null(),
  )
}

pub fn status(row: Row) -> Status {
  row.status
}

pub fn is_online(row: Row) -> Bool {
  presence.is_online(row.status)
}

pub fn is_engaged(row: Row) -> Bool {
  let active_status = case row.status {
    presence.Online | presence.Dnd -> True
    _ -> False
  }
  active_status && !row.afk && !row.mobile
}

pub fn list_relevant_change(old: Option(Row), new: Row) -> Bool {
  case old {
    None -> True
    Some(old) ->
      old.status != new.status
      || old.mobile != new.mobile
      || option.map(old.custom_status, raw_json.to_bits)
      != option.map(new.custom_status, raw_json.to_bits)
      || raw_json.to_bits(old.user_wire) != raw_json.to_bits(new.user_wire)
  }
}

fn fields(row: Row) -> List(#(String, Json)) {
  [
    #("user", raw_json.to_json(row.user_wire)),
    #("status", json.string(presence.status_to_wire(row.status))),
    #("mobile", json.bool(row.mobile)),
    #("afk", json.bool(row.afk)),
    #("custom_status", json.nullable(row.custom_status, raw_json.to_json)),
  ]
}

pub fn to_json(row: Row) -> Json {
  json.object(fields(row))
}

pub fn to_raw(row: Row) -> RawJson {
  raw_json.from_json(to_json(row))
}

pub fn update_payload(row: Row, guild: GuildId) -> RawJson {
  [#("guild_id", id.json(id.guild_id_to_int(guild))), ..fields(row)]
  |> json.object
  |> raw_json.from_json
}
