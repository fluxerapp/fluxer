// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

pub opaque type User {
  User(
    id: UserId,
    bot: Bool,
    flags: Int,
    restricted: Bool,
    wire: RawJson,
    name: Option(String),
    search_names: List(String),
  )
}

const staff_flag = 1

pub fn new(
  id id: UserId,
  bot bot: Bool,
  flags flags: Int,
  restricted restricted: Bool,
  wire wire: RawJson,
) -> User {
  User(id:, bot:, flags:, restricted:, wire:, name: None, search_names: [])
}

pub fn id(user: User) -> UserId {
  user.id
}

pub fn is_bot(user: User) -> Bool {
  user.bot
}

pub fn flags(user: User) -> Int {
  user.flags
}

pub fn is_staff(user: User) -> Bool {
  int.bitwise_and(user.flags, staff_flag) != 0
}

pub fn is_restricted(user: User) -> Bool {
  user.restricted
}

pub fn wire(user: User) -> RawJson {
  user.wire
}

pub fn with_name(
  user: User,
  global_name global_name: String,
  username username: String,
) -> User {
  let name = case global_name {
    "" -> username
    _ -> global_name
  }
  User(..user, name: Some(name))
}

pub fn name(user: User) -> Option(String) {
  user.name
}

pub fn with_search_names(user: User, names: List(Option(String))) -> User {
  User(..user, search_names: option.values(names) |> list.map(string.lowercase))
}

pub fn search_names(user: User) -> List(String) {
  user.search_names
}
