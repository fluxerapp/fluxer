// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/session.{ClientProperties, IdentifyFlags}
import fluxer_gateway/gateway/identify
import gleam/json
import gleam/option.{None, Some}
import gleam/string

fn validate(text: String) {
  let assert Ok(outcome) = json.parse(text, identify.decoder())
  outcome
}

const properties =
  "\"properties\":{\"os\":\"Mac OS X\",\"browser\":\"Chrome\",\"device\":\"\"}"

pub fn minimal_identify_test() {
  let assert Ok(request) = validate("{\"token\":\"abc\"," <> properties <> "}")
  assert request.token == "abc"
  assert request.properties == ClientProperties("Mac OS X", "Chrome", "")
  assert request.shard == None
  assert request.presence == None
  assert request.ignored_events == []
  assert request.initial_guild_id == None
  assert request.flags == IdentifyFlags(False, False, False)
}

pub fn full_identify_test() {
  let text =
    "{\"token\":\"abc\","
    <> properties
    <> ",\"flags\":14,\"shard\":[1,2],\"ignored_events\":[\"typing_start\",\"MESSAGE_CREATE\",\"Typing_Start\"],"
    <> "\"initial_guild_id\":\"1234567890123\",\"presence\":{\"status\":\"idle\"}}"
  let assert Ok(request) = validate(text)
  assert request.flags == IdentifyFlags(True, True, True)
  assert request.shard == Some(#(1, 2))
  assert request.ignored_events == ["MESSAGE_CREATE", "TYPING_START"]
  assert request.initial_guild_id
    == id.parse_guild_id("1234567890123") |> option.from_result
  assert request.presence != None
}

pub fn numeric_initial_guild_id_is_ignored_test() {
  let text =
    "{\"token\":\"abc\","
    <> properties
    <> ",\"initial_guild_id\":1234567890123}"
  let assert Ok(request) = validate(text)
  assert request.initial_guild_id == None
}

pub fn missing_fields_test() {
  assert validate("{\"token\":\"abc\"}") == Error(identify.InvalidPayload)
  assert validate("{" <> properties <> "}") == Error(identify.InvalidPayload)
  assert validate("[]") == Error(identify.InvalidPayload)
  assert validate(
      "{\"token\":\"abc\",\"properties\":{\"os\":\"x\",\"browser\":\"y\"}}",
    )
    == Error(identify.InvalidPayload)
}

pub fn token_checks_test() {
  assert validate("{\"token\":\"   \"," <> properties <> "}")
    == Error(identify.InvalidToken)
  assert validate("{\"token\":12," <> properties <> "}")
    == Error(identify.InvalidToken)
  let long = string.repeat("a", 257)
  assert validate("{\"token\":\"" <> long <> "\"," <> properties <> "}")
    == Error(identify.InvalidToken)
}

pub fn flags_and_shard_errors_test() {
  assert validate("{\"token\":\"a\"," <> properties <> ",\"flags\":-1}")
    == Error(identify.InvalidPayload)
  assert validate("{\"token\":\"a\"," <> properties <> ",\"flags\":null}")
    == Error(identify.InvalidPayload)
  assert validate("{\"token\":\"a\"," <> properties <> ",\"shard\":[2,2]}")
    == Error(identify.InvalidShard)
  assert validate(
      "{\"token\":\"a\"," <> properties <> ",\"ignored_events\":[1]}",
    )
    == Error(identify.InvalidPayload)
}
