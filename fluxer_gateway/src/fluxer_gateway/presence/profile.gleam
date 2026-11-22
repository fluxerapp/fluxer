// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/raw_json
import fluxer_gateway/core/user.{type User}
import gleam/bit_array
import gleam/json
import gleam/list
import gleam/result

pub type UserField {
  Id
  Username
  Discriminator
  GlobalName
  Avatar
  AvatarColor
  Bot
  System
  Flags
  MentionFlags
}

pub opaque type Profile {
  Profile(
    fields: List(#(UserField, BitArray)),
    restricted: Bool,
    bot: Bool,
    wire: BitArray,
  )
}

pub type ProfileUpdate {
  ProfileUpdate(profile: Profile, differs: Bool, restriction_changed: Bool)
}

const wire_order = [
  Avatar,
  AvatarColor,
  Bot,
  Discriminator,
  Flags,
  GlobalName,
  Id,
  MentionFlags,
  System,
  Username,
]

fn field_name(field: UserField) -> String {
  case field {
    Id -> "id"
    Username -> "username"
    Discriminator -> "discriminator"
    GlobalName -> "global_name"
    Avatar -> "avatar"
    AvatarColor -> "avatar_color"
    Bot -> "bot"
    System -> "system"
    Flags -> "flags"
    MentionFlags -> "mention_flags"
  }
}

pub fn new(
  fields fields: List(#(UserField, BitArray)),
  restricted restricted: Bool,
  bot bot: Bool,
) -> Profile {
  let normalized = normalize(fields, restricted)
  Profile(fields: normalized, restricted:, bot:, wire: encode(normalized))
}

pub fn from_user(source: User) -> Profile {
  new(
    fields: fields_of(json_tree.from_raw(user.wire(source))),
    restricted: user.is_restricted(source),
    bot: user.is_bot(source),
  )
}

pub fn from_fields(fields: List(#(String, JsonTree))) -> Profile {
  new(
    fields: normalized_fields(fields),
    restricted: flag(fields, "profile_restricted"),
    bot: flag(fields, "bot"),
  )
}

fn flag(fields: List(#(String, JsonTree)), name: String) -> Bool {
  list.key_find(fields, name) |> result.try(json_tree.bool) == Ok(True)
}

fn fields_of(value: JsonTree) -> List(#(UserField, BitArray)) {
  json_tree.fields(value)
  |> result.map(normalized_fields)
  |> result.unwrap([])
}

fn normalized_fields(
  fields: List(#(String, JsonTree)),
) -> List(#(UserField, BitArray)) {
  list.filter_map(wire_order, fn(field) {
    use raw <- result.try(list.key_find(fields, field_name(field)))
    normalize_field(field, raw)
    |> result.map(fn(bytes) { #(field, bytes) })
  })
}

fn normalize_field(field: UserField, raw: JsonTree) -> Result(BitArray, Nil) {
  case field {
    Id -> snowflake_digits(raw)
    AvatarColor ->
      case json_tree.expand(raw) {
        Ok(json_tree.Null) -> Ok(<<"null":utf8>>)
        Ok(json_tree.Int(number)) -> int32(number)
        Ok(json_tree.String(text)) -> signed(<<text:utf8>>) |> result.try(int32)
        _ -> Error(Nil)
      }
    Flags | MentionFlags ->
      case json_tree.expand(raw) {
        Ok(json_tree.Int(number)) if number >= 0 -> Ok(number_bytes(number))
        Ok(json_tree.String(text)) ->
          digits(<<text:utf8>>, 0, False) |> result.map(number_bytes)
        _ -> Error(Nil)
      }
    _ -> json_tree.canonical(raw)
  }
}

fn snowflake_digits(raw: JsonTree) -> Result(BitArray, Nil) {
  case json_tree.expand(raw) {
    Ok(json_tree.String(text)) -> id.parse(text) |> result.map(snowflake_text)
    Ok(json_tree.Int(number)) ->
      id.check(number)
      |> result.map(snowflake_text)
      |> result.replace_error(Nil)
    _ -> Error(Nil)
  }
}

fn snowflake_text(value: Int) -> BitArray {
  <<"\"":utf8, id.write(value):utf8, "\"":utf8>>
}

fn int32(number: Int) -> Result(BitArray, Nil) {
  case number >= -2_147_483_648 && number <= 2_147_483_647 {
    True -> Ok(number_bytes(number))
    False -> Error(Nil)
  }
}

fn number_bytes(number: Int) -> BitArray {
  raw_json.to_bits(raw_json.from_json(json.int(number)))
}

fn signed(text: BitArray) -> Result(Int, Nil) {
  case text {
    <<"-", rest:bits>> -> digits(rest, 0, False) |> result.map(fn(n) { 0 - n })
    <<"+", rest:bits>> -> digits(rest, 0, False)
    _ -> digits(text, 0, False)
  }
}

fn digits(text: BitArray, acc: Int, seen: Bool) -> Result(Int, Nil) {
  case text {
    <<>> if seen -> Ok(acc)
    <<digit, rest:bits>> if digit >= 48 && digit <= 57 ->
      digits(rest, acc * 10 + digit - 48, True)
    _ -> Error(Nil)
  }
}

fn normalize(
  fields: List(#(UserField, BitArray)),
  restricted: Bool,
) -> List(#(UserField, BitArray)) {
  let fields = case restricted {
    True ->
      fields
      |> list.key_set(GlobalName, <<"null":utf8>>)
      |> list.key_set(Avatar, <<"null":utf8>>)
    False -> fields
  }
  list.filter_map(wire_order, fn(field) {
    list.key_find(fields, field) |> result.map(fn(value) { #(field, value) })
  })
}

fn encode(fields: List(#(UserField, BitArray))) -> BitArray {
  let body =
    fields
    |> list.map(fn(pair) {
      bit_array.concat([
        <<"\"":utf8>>,
        bit_array.from_string(field_name(pair.0)),
        <<"\":":utf8>>,
        pair.1,
      ])
    })
    |> list.intersperse(<<",":utf8>>)
  bit_array.concat([<<"{":utf8>>, ..list.append(body, [<<"}":utf8>>])])
}

pub fn wire(profile: Profile) -> BitArray {
  profile.wire
}

pub fn is_bot(profile: Profile) -> Bool {
  profile.bot
}

pub fn restricted(profile: Profile) -> Bool {
  profile.restricted
}

pub fn apply_update(current: Profile, update: Profile) -> ProfileUpdate {
  let differs =
    list.any(update.fields, fn(pair) {
      list.key_find(current.fields, pair.0) != Ok(pair.1)
    })
  let restriction_changed = current.restricted != update.restricted
  let profile = case differs || restriction_changed {
    True -> update
    False -> current
  }
  ProfileUpdate(profile:, differs:, restriction_changed:)
}

pub fn fields(profile: Profile) -> List(#(UserField, BitArray)) {
  profile.fields
}

pub fn name_of(field: UserField) -> String {
  field_name(field)
}

pub fn field_named(name: String) -> Result(UserField, Nil) {
  list.find(wire_order, fn(field) { field_name(field) == name })
}
