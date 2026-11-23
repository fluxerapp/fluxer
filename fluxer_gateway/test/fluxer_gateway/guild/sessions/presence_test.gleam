// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/core/presence
import fluxer_gateway/core/raw_json
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/init
import fluxer_gateway/guild/presence/row
import fluxer_gateway/guild/sessions as table
import fluxer_gateway/guild/sessions/presence as session_presence
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/nif
import gleam/dict
import gleam/erlang/process
import gleam/option.{None, Some}

fn fixture() -> GuildState {
  let assert Ok(Nil) = nif.load()
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"1\"}")),
      #(
        "roles",
        whole("[{\"id\":\"100\",\"permissions\":\"0\",\"position\":0}]"),
      ),
      #("channels", whole("[{\"id\":\"5\",\"type\":0,\"position\":0}]")),
      #(
        "members",
        whole("[{\"user\":{\"id\":\"2\",\"username\":\"b\"},\"roles\":[]}]"),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(loaded) = data_decode.collections(guild, collected)
  let assert Ok(state) =
    init.build(
      core_guild.GuildRef(guild:, inbox: process.new_subject()),
      process.new_subject(),
      process.new_subject(),
      loaded,
      None,
      None,
      table.new(),
    )
  state
}

fn watched_online(state: GuildState, user: Int) -> GuildState {
  let assert Ok(user_id) = id.user_id(user)
  let assert Some(owner) = guild_state.member(state, user_id)
  let online =
    row.from_presence(
      presence.new(
        user: user_id,
        status: presence.Online,
        mobile: False,
        afk: False,
        custom_status: None,
        wire: raw_json.null(),
      ),
      owner,
    )
  guild_state.sessions(state)
  |> table.add_interest([user])
  |> table.put_row(user, online)
  |> guild_state.set_sessions(state, _)
}

pub fn released_user_drops_cached_presence_row_test() {
  let state = watched_online(fixture(), 2)
  assert table.presence_row(guild_state.sessions(state), 2) != None
  let state = session_presence.release(state, [2])
  let sessions = guild_state.sessions(state)
  assert !table.is_interested(sessions, 2)
  assert table.presence_row(sessions, 2) == None
  assert table.presence_count(sessions) == 0
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
