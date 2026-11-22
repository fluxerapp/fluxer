// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/core/session
import fluxer_gateway/nif
import fluxer_gateway/session/seed
import gleam/dict
import gleam/list
import gleam/option.{None, Some}

fn identify(threads: Bool) -> session.Identify {
  session.Identify(
    token: "token",
    properties: session.ClientProperties(os: "", browser: "", device: ""),
    shard: None,
    presence: None,
    flags: session.IdentifyFlags(
      debounce_message_reactions: False,
      channel_threads: threads,
      guild_visits: False,
    ),
    ignored_events: [],
    initial_guild_id: None,
    e2ee_capable: False,
    mobile: False,
    latitude: None,
    longitude: None,
  )
}

const user = "\"user\":{\"id\":\"123456789012345678\",\"username\":\"u\"}"

fn seeded(threads: Bool, rest: String) -> seed.Seed {
  let assert Ok(Nil) = nif.load()
  let data = <<"{":utf8, user:utf8, ",":utf8, rest:utf8, "}":utf8>>
  let assert Ok(found) =
    seed.from_api(session.new_session_id(), identify(threads), 1, data)
  found
}

fn ready_field(found: seed.Seed, key: String) -> BitArray {
  let assert Some(ready) = found.ready
  let assert Ok(value) = list.key_find(ready.fields, key)
  raw_json.to_bits(value)
}

const read_states =
  "\"read_states\":[{\"id\":\"1\",\"flags\":2},{\"id\":\"2\",\"flags\":0,\"x\":1},{\"id\":\"3\"},7]"

pub fn read_states_drop_threads_and_flags_for_old_clients_test() {
  assert ready_field(seeded(False, read_states), "read_states")
    == <<"[{\"id\":\"2\",\"x\":1},{\"id\":\"3\"},7]">>
}

pub fn read_states_pass_through_for_thread_clients_test() {
  assert ready_field(seeded(True, read_states), "read_states")
    == <<
      "[{\"id\":\"1\",\"flags\":2},{\"id\":\"2\",\"flags\":0,\"x\":1},{\"id\":\"3\"},7]",
    >>
}

pub fn read_states_without_flags_keep_their_bytes_test() {
  let rows =
    "\"read_states\":[{\"id\":\"1\",\"mention_count\":0}, {\"id\":\"2\"}]"
  assert ready_field(seeded(False, rows), "read_states")
    == <<"[{\"id\":\"1\",\"mention_count\":0}, {\"id\":\"2\"}]">>
}

pub fn passthrough_fields_keep_their_bytes_test() {
  let found = seeded(False, "\"user_guild_settings\":{\"b\":1, \"a\":[2]}")
  assert ready_field(found, "user_guild_settings") == <<"{\"b\":1, \"a\":[2]}">>
  let assert Some(ready) = found.ready
  assert list.key_find(ready.fields, "guilds") == Error(Nil)
}

pub fn relationships_lose_the_user_and_keep_an_id_test() {
  let found =
    seeded(
      False,
      "\"relationships\":[{\"type\":1,\"nickname\":null,\"user\":{\"id\":\"9\",\"username\":\"f\"}}]",
    )
  let assert Some(ready) = found.ready
  assert list.map(ready.relationships, raw_json.to_bits)
    == [<<"{\"id\":\"9\",\"nickname\":null,\"type\":1}">>]
  assert dict.keys(ready.users) == [9]
}

pub fn settings_custom_status_is_kept_test() {
  let found =
    seeded(
      False,
      "\"user_settings\":{\"status\":\"idle\",\"custom_status\":{\"text\":\"hi\"}}",
    )
  assert option.map(found.custom_status, raw_json.to_bits)
    == Some(<<"{\"text\":\"hi\"}">>)
}

pub fn settings_keep_regional_and_timestamp_formats_test() {
  let settings =
    "{\"locale\":\"en-GB\",\"regional_format\":\"sv-SE\",\"time_format\":1,\"timestamp_format\":2}"
  let found = seeded(False, "\"user_settings\":" <> settings)
  assert ready_field(found, "user_settings") == <<settings:utf8>>
}

pub fn missing_user_is_an_error_test() {
  let assert Ok(Nil) = nif.load()
  assert seed.from_api(session.new_session_id(), identify(False), 1, <<
      "{\"read_states\":[]}",
    >>)
    == Error(seed.MissingUser)
}

pub fn the_first_of_duplicate_keys_wins_test() {
  let found = seeded(False, "\"locale\":\"sv\",\"locale\":\"en\"")
  assert ready_field(found, "locale") == <<"\"sv\"">>
}
