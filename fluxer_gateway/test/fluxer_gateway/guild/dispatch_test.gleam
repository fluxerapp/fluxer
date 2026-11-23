// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/guild as core_guild
import fluxer_gateway/core/id
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/dispatch
import fluxer_gateway/guild/inbox.{type Control}
import fluxer_gateway/guild/init
import fluxer_gateway/guild/sessions as guild_sessions
import fluxer_gateway/guild/state.{type GuildState} as guild_state
import fluxer_gateway/nif
import fluxer_gateway/util/decode as id_decode
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process.{type Subject}
import gleam/option.{None}

fn parse(text: String) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<text:utf8>>)
  value
}

fn fixture() -> #(GuildState, Subject(Control)) {
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
        whole("[{\"user\":{\"id\":\"2\",\"username\":\"old\"},\"roles\":[]}]"),
      ),
    ])
  let assert Ok(guild) = id.guild_id(100)
  let assert Ok(loaded) = data_decode.collections(guild, collected)
  let control = process.new_subject()
  let assert Ok(state) =
    init.build(
      core_guild.GuildRef(guild:, inbox: process.new_subject()),
      control,
      process.new_subject(),
      loaded,
      None,
      None,
      guild_sessions.new(),
    )
  #(state, control)
}

fn changed_author_message() -> Dynamic {
  parse(
    "{\"id\":\"9\",\"channel_id\":\"5\",\"content\":\"hi\",\"author\":{\"id\":\"2\",\"username\":\"new\"}}",
  )
}

fn member_updates(control: Subject(Control)) -> Int {
  drain(control, 0)
}

fn drain(control: Subject(Control), count: Int) -> Int {
  case process.receive(control, 0) {
    Ok(inbox.Dispatch("GUILD_MEMBER_UPDATE", _)) -> drain(control, count + 1)
    Ok(_) -> drain(control, count)
    Error(Nil) -> count
  }
}

pub fn live_changed_author_casts_one_member_update_test() {
  let #(state, control) = fixture()
  let _ =
    dispatch.handle_event(state, "MESSAGE_CREATE", changed_author_message())
  assert member_updates(control) == 1
}

pub fn replayed_changed_author_casts_nothing_and_skips_push_test() {
  let #(state, control) = fixture()
  let before = guild_state.push(state)
  let after =
    dispatch.replay_event(state, "MESSAGE_CREATE", changed_author_message())
  assert member_updates(control) == 0
  assert guild_state.push(after) == before
}

pub fn live_message_reaches_push_test() {
  let #(state, _) = fixture()
  let before = guild_state.push(state)
  let after =
    dispatch.handle_event(state, "MESSAGE_CREATE", changed_author_message())
  assert guild_state.push(after) != before
}

pub fn unchanged_author_casts_nothing_test() {
  let #(state, control) = fixture()
  let message =
    parse(
      "{\"id\":\"9\",\"channel_id\":\"5\",\"content\":\"hi\",\"author\":{\"id\":\"2\",\"username\":\"old\"}}",
    )
  let _ = dispatch.handle_event(state, "MESSAGE_CREATE", message)
  assert member_updates(control) == 0
}

fn whole(text: String) -> data_decode.Collection {
  data_decode.Whole(<<text:utf8>>)
}
