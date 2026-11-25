// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/int
import gleam/json.{type Json}
import gleam/order.{type Order}
import gleam/result

pub opaque type GuildId {
  GuildId(Int)
}

pub opaque type UserId {
  UserId(Int)
}

pub opaque type ChannelId {
  ChannelId(Int)
}

pub opaque type RoleId {
  RoleId(Int)
}

pub opaque type MessageId {
  MessageId(Int)
}

pub type IdError {
  NotPositive(value: Int)
  OutOfRange(value: Int)
  NotDecimal(text: String)
}

pub const max_id = 9_223_372_036_854_775_807

const max_digits = 19

pub fn check(value: Int) -> Result(Int, IdError) {
  case value <= 0, value > max_id {
    True, _ -> Error(NotPositive(value))
    _, True -> Error(OutOfRange(value))
    _, _ -> Ok(value)
  }
}

pub fn parse(text: String) -> Result(Int, Nil) {
  parse_bits(<<text:utf8>>)
}

pub fn parse_bits(bytes: BitArray) -> Result(Int, Nil) {
  decimal(bytes)
  |> result.try(fn(value) { check(value) |> result.replace_error(Nil) })
}

fn parse_checked(text: String) -> Result(Int, IdError) {
  case decimal(<<text:utf8>>) {
    Ok(value) -> check(value)
    Error(Nil) -> Error(NotDecimal(text))
  }
}

pub fn parse_decimal(text: String) -> Result(Int, Nil) {
  int.parse(text)
}

@external(erlang, "erlang", "integer_to_binary")
pub fn write(value: Int) -> String

pub fn json(value: Int) -> Json {
  json.string(write(value))
}

fn decimal(bytes: BitArray) -> Result(Int, Nil) {
  case bytes, bit_array.byte_size(bytes) <= max_digits {
    <<first, _:bits>>, True if first >= 49 && first <= 57 -> integer(bytes)
    _, _ -> Error(Nil)
  }
}

@external(erlang, "gleam_stdlib", "parse_int")
fn integer(bytes: BitArray) -> Result(Int, Nil)

pub fn guild_id(value: Int) -> Result(GuildId, IdError) {
  check(value) |> result.map(GuildId)
}

pub fn user_id(value: Int) -> Result(UserId, IdError) {
  check(value) |> result.map(UserId)
}

pub fn channel_id(value: Int) -> Result(ChannelId, IdError) {
  check(value) |> result.map(ChannelId)
}

pub fn role_id(value: Int) -> Result(RoleId, IdError) {
  check(value) |> result.map(RoleId)
}

pub fn message_id(value: Int) -> Result(MessageId, IdError) {
  check(value) |> result.map(MessageId)
}

pub fn parse_guild_id(text: String) -> Result(GuildId, IdError) {
  parse_checked(text) |> result.map(GuildId)
}

pub fn parse_user_id(text: String) -> Result(UserId, IdError) {
  parse_checked(text) |> result.map(UserId)
}

pub fn parse_channel_id(text: String) -> Result(ChannelId, IdError) {
  parse_checked(text) |> result.map(ChannelId)
}

pub fn guild_id_to_int(id: GuildId) -> Int {
  let GuildId(value) = id
  value
}

pub fn user_id_to_int(id: UserId) -> Int {
  let UserId(value) = id
  value
}

pub fn channel_id_to_int(id: ChannelId) -> Int {
  let ChannelId(value) = id
  value
}

pub fn role_id_to_int(id: RoleId) -> Int {
  let RoleId(value) = id
  value
}

pub fn message_id_to_int(id: MessageId) -> Int {
  let MessageId(value) = id
  value
}

pub fn guild_id_to_string(id: GuildId) -> String {
  write(guild_id_to_int(id))
}

pub fn user_id_to_string(id: UserId) -> String {
  write(user_id_to_int(id))
}

pub fn channel_id_to_string(id: ChannelId) -> String {
  write(channel_id_to_int(id))
}

pub fn guild_json(id: GuildId) -> Json {
  json(guild_id_to_int(id))
}

pub fn user_json(id: UserId) -> Json {
  json(user_id_to_int(id))
}

pub fn channel_json(id: ChannelId) -> Json {
  json(channel_id_to_int(id))
}

pub fn message_json(id: MessageId) -> Json {
  json(message_id_to_int(id))
}

pub fn compare_user_id(a: UserId, b: UserId) -> Order {
  int.compare(user_id_to_int(a), user_id_to_int(b))
}

pub fn compare_channel_id(a: ChannelId, b: ChannelId) -> Order {
  int.compare(channel_id_to_int(a), channel_id_to_int(b))
}

pub fn compare_role_id(a: RoleId, b: RoleId) -> Order {
  int.compare(role_id_to_int(a), role_id_to_int(b))
}

pub fn everyone_role(guild: GuildId) -> RoleId {
  RoleId(guild_id_to_int(guild))
}
