// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members
import fluxer_gateway/guild/members/roles
import gleam/dict
import gleam/option.{None}

fn setup() {
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"1\"}")),
      #(
        "roles",
        whole(
          "[{\"id\":\"100\",\"permissions\":\"0\",\"position\":0},"
          <> "{\"id\":\"200\",\"permissions\":\"268435456\",\"position\":5},"
          <> "{\"id\":\"300\",\"permissions\":\"0\",\"position\":5},"
          <> "{\"id\":\"400\",\"permissions\":\"0\",\"position\":2}]",
        ),
      ),
      #("channels", whole("[]")),
      #(
        "members",
        whole(
          "[{\"user\":{\"id\":\"1\"},\"roles\":[]},"
          <> "{\"user\":{\"id\":\"2\"},\"roles\":[\"200\"]},"
          <> "{\"user\":{\"id\":\"3\"},\"roles\":[\"400\"]},"
          <> "{\"user\":{\"id\":\"4\"},\"roles\":[\"300\"]}]",
        ),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(loaded) = data_decode.collections(guild, collected)
  let table = members.new(None)
  members.put_many(table, loaded.members)
  #(loaded.data, table)
}

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn role(value: Int) -> id.RoleId {
  let assert Ok(found) = id.role_id(value)
  found
}

pub fn owner_manages_everyone_test() {
  let #(guild_data, table) = setup()
  let assert True =
    roles.check_target_member(guild_data, table, user(1), user(2))
  let assert False =
    roles.check_target_member(guild_data, table, user(2), user(1))
  members.drop(table)
}

pub fn higher_role_manages_lower_member_test() {
  let #(guild_data, table) = setup()
  let assert True =
    roles.check_target_member(guild_data, table, user(2), user(3))
  let assert False =
    roles.check_target_member(guild_data, table, user(3), user(2))
  members.drop(table)
}

pub fn equal_position_lower_role_id_wins_test() {
  let #(guild_data, table) = setup()
  let assert True =
    roles.check_target_member(guild_data, table, user(2), user(4))
  let assert False =
    roles.check_target_member(guild_data, table, user(4), user(2))
  let assert True = roles.can_manage_role(guild_data, table, user(2), role(300))
  let assert False =
    roles.can_manage_role(guild_data, table, user(4), role(200))
  members.drop(table)
}

pub fn members_with_role_are_sorted_test() {
  let #(_, table) = setup()
  let assert [2] = members.with_role(table, role(200))
  let assert 4 = members.count(table)
  members.drop(table)
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
