// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/members
import fluxer_gateway/guild/user_data
import fluxer_gateway/util/decode as id_decode
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/option.{None, Some}

fn parse(text: String) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<text:utf8>>)
  value
}

fn setup() -> #(id.GuildId, members.MembersTable) {
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"1\"}")),
      #(
        "roles",
        whole("[{\"id\":\"100\",\"permissions\":\"0\",\"position\":0}]"),
      ),
      #("channels", whole("[]")),
      #(
        "members",
        whole("[{\"user\":{\"id\":\"2\",\"username\":\"old\"},\"roles\":[]}]"),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(loaded) = data_decode.collections(guild, collected)
  let table = members.new(None)
  members.put_many(table, loaded.members)
  #(guild, table)
}

fn message(username: String) -> Dynamic {
  parse(
    "{\"id\":\"9\",\"channel_id\":\"5\",\"author\":{\"id\":\"2\",\"username\":\""
    <> username
    <> "\"}}",
  )
}

pub fn changed_author_yields_one_member_update_test() {
  let #(guild, table) = setup()
  let assert Some(payload) =
    user_data.refresh_author(table, guild, "MESSAGE_CREATE", message("new"))
  let fields = {
    use guild_id <- decode.field("guild_id", decode.string)
    use username <- decode.subfield(["user", "username"], decode.string)
    decode.success(#(guild_id, username))
  }
  assert decode.run(payload, fields) == Ok(#("100", "new"))
  assert user_data.refresh_author(
      table,
      guild,
      "MESSAGE_CREATE",
      message("new"),
    )
    == None
  assert user_data.refresh_author(
      table,
      guild,
      "MESSAGE_UPDATE",
      message("new"),
    )
    == None
}

pub fn unchanged_author_yields_nothing_test() {
  let #(guild, table) = setup()
  assert user_data.refresh_author(
      table,
      guild,
      "MESSAGE_CREATE",
      message("old"),
    )
    == None
}

pub fn other_events_never_refresh_test() {
  let #(guild, table) = setup()
  assert user_data.refresh_author(table, guild, "TYPING_START", message("new"))
    == None
  assert user_data.refresh_author(
      table,
      guild,
      "MESSAGE_CREATE",
      message("old"),
    )
    == None
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
