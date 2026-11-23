// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/dispatch
import fluxer_gateway/guild/init
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/nif
import fluxer_gateway/perms
import fluxer_gateway/util/decode as id_decode
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/json
import gleam/option.{None}
import support/guild_api
import support/guild_fixture as fixture
import support/wait

fn parse(text: String) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<text:utf8>>)
  value
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}

fn fixture() -> GuildState {
  let assert Ok(Nil) = nif.load()
  let collected =
    dict.from_list([
      #("guild", whole("{\"id\":\"100\",\"owner_id\":\"1\"}")),
      #(
        "roles",
        whole("[{\"id\":\"100\",\"permissions\":\"1024\",\"position\":0}]"),
      ),
      #("channels", whole("[{\"id\":\"5\",\"type\":0,\"position\":0}]")),
      #(
        "members",
        whole("[{\"user\":{\"id\":\"2\",\"username\":\"two\"},\"roles\":[]}]"),
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
      guild_sessions.new(),
    )
  state
}

fn hide_channel() -> Dynamic {
  parse(
    "{\"id\":\"5\",\"guild_id\":\"100\",\"type\":0,\"position\":0,\"permission_overwrites\":[{\"id\":\"100\",\"type\":0,\"allow\":\"0\",\"deny\":\"1024\"}]}",
  )
}

fn can_view(state: GuildState) -> Result(Bool, perms.PermsError) {
  let assert Ok(user) = id.user_id(2)
  let assert Ok(channel) = id.channel_id(5)
  perms.can_view(guild_state.perms_reader(state), user, channel)
}

pub fn failed_perm_apply_rebuilds_from_guild_data_test() {
  let state = fixture()
  assert can_view(state) == Ok(True)
  perms.release(guild_state.perms_handle(state))
  let after = dispatch.handle_event(state, "CHANNEL_UPDATE", hide_channel())
  assert can_view(after) == Ok(False)
  assert guild_state.repair(after)
    == guild_state.Rebuilt([guild_state.perms_handle(state)])
}

pub fn unrepresentable_guild_data_marks_the_guild_for_restart_test() {
  let state =
    dispatch.handle_event(
      fixture(),
      "CHANNEL_CREATE",
      parse("{\"id\":\"6\",\"guild_id\":\"100\",\"type\":0,\"position\":1}"),
    )
  let bulk =
    parse(
      "{\"channels\":[{\"id\":\"5\",\"guild_id\":\"100\",\"type\":0,\"position\":0,\"permission_overwrites\":[{\"id\":\"100\",\"type\":0,\"allow\":\"0\",\"deny\":\"1024\"}]},{\"id\":\"6\",\"guild_id\":\"100\",\"type\":4294967296,\"position\":1}]}",
    )
  let after = dispatch.handle_event(state, "CHANNEL_UPDATE_BULK", bulk)
  assert guild_state.repair(after) == guild_state.Unrepairable
}

pub fn running_guild_stops_when_perms_cannot_be_rebuilt_test() {
  use api <- guild_api.with_api
  let found = fixture.new(1) |> fixture.with_text(10) |> fixture.with_text(11)
  let handle = fixture.started(api, found)
  fixture.sync(handle)
  fixture.dispatch(
    handle,
    "CHANNEL_UPDATE_BULK",
    json.object([
      #(
        "channels",
        json.preprocessed_array([
          fixture.channel(found.id, 10, 0, None, []),
          fixture.channel(found.id, 11, 4_294_967_296, None, []),
        ]),
      ),
    ]),
  )
  assert wait.until(3000, fn() { !process.is_alive(handle.pid) })
}
