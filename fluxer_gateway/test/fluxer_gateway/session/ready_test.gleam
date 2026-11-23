// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts
import fluxer_gateway/core/guild_create.{Available, ReadyGuild} as created
import fluxer_gateway/core/id
import fluxer_gateway/core/raw_json
import fluxer_gateway/session/payload
import fluxer_gateway/session/pending_presences.{Held}
import fluxer_gateway/session/ready.{Build}
import fluxer_gateway/session/visits.{ConnectVisit, Ignore, MarkVisit}
import gleam/bit_array
import gleam/dict
import gleam/json
import gleam/option.{None, Some}
import gleam/string
import gleeunit/should

fn raw(text: String) -> raw_json.RawJson {
  raw_json.wrap(bit_array.from_string(text))
}

fn guild(value: Int) -> id.GuildId {
  let assert Ok(guild) = id.guild_id(value)
  guild
}

fn text(value: raw_json.RawJson) -> String {
  let assert Ok(text) = bit_array.to_string(raw_json.to_bits(value))
  text
}

pub fn strip_members_moves_users_out_test() {
  let assert Available(_, body) =
    created.from_wire(
      guild(1),
      raw(
        "{\"id\":\"1\",\"members\":[{\"nick\":\"a\",\"user\":{\"id\":\"42\",\"username\":\"x\"}}]}",
      ),
    )
  let ReadyGuild(wire: stripped, users:) = created.ready(body)
  dict.keys(dict.from_list(users)) |> should.equal([42])
  string.contains(text(stripped), "\"username\"") |> should.be_false
  string.contains(text(stripped), "\"42\"") |> should.be_true
}

pub fn unavailable_placeholder_test() {
  ready.unavailable_guild(guild(7), True)
  |> text
  |> should.equal(
    "{\"id\":\"7\",\"unavailable\":true,\"unavailable_hidden\":true}",
  )
}

pub fn bot_ready_lists_placeholders_only_test() {
  let payload =
    ready.build(Build(
      data: ready.empty(),
      session_id: "abc",
      version: 1,
      shard: None,
      bot: True,
      staff: False,
      guilds: [guild(3)],
      collected: [created.from_wire(guild(3), raw("{\"id\":\"3\"}"))],
      presences: [],
      timings: None,
    ))
  let body = text(payload)
  string.contains(body, "{\"id\":\"3\",\"unavailable\":true}") |> should.be_true
  string.contains(body, "\"session_id\":\"abc\"") |> should.be_true
}

pub fn missing_guilds_ship_as_unavailable_test() {
  let payload =
    ready.build(Build(
      data: ready.empty(),
      session_id: "abc",
      version: 1,
      shard: Some(#(0, 2)),
      bot: False,
      staff: False,
      guilds: [guild(3), guild(4)],
      collected: [created.from_wire(guild(3), raw("{\"id\":\"3\"}"))],
      presences: [],
      timings: None,
    ))
  let body = text(payload)
  string.contains(body, "{\"id\":\"4\",\"unavailable\":true}") |> should.be_true
  string.contains(body, "\"shard\"") |> should.be_true
}

pub fn with_field_prefixes_object_test() {
  payload.with_field(raw("{\"id\":\"1\"}"), "unavailable", json.bool(False))
  |> text
  |> should.equal("{\"unavailable\":false,\"id\":\"1\"}")
  payload.with_field(raw("{}"), "a", json.int(1))
  |> text
  |> should.equal("{\"a\":1}")
}

pub fn pending_presences_release_by_user_test() {
  let pending =
    pending_presences.new()
    |> pending_presences.hold(Held(
      Some(1),
      None,
      raw("{}"),
      dispatch_facts.plain(),
    ))
    |> pending_presences.hold(Held(
      Some(2),
      None,
      raw("{}"),
      dispatch_facts.plain(),
    ))
  let #(left, released) = pending_presences.release_user(pending, 2)
  pending_presences.size(left) |> should.equal(1)
  case released {
    [Held(user: Some(2), ..)] -> True
    _ -> False
  }
  |> should.be_true
}

pub fn visit_start_decisions_test() {
  let capable = visits.Visits(..visits.none(), capable: True)
  visits.decide_start(visits.none(), 1, bot: False, known: False)
  |> should.equal(Ignore)
  visits.decide_start(capable, 1, bot: False, known: False)
  |> should.equal(ConnectVisit)
  visits.decide_start(visits.put(capable, 1, False), 1, bot: False, known: True)
  |> should.equal(MarkVisit)
  visits.decide_start(capable, 1, bot: True, known: False)
  |> should.equal(Ignore)
}

pub fn guild_create_wire_round_trips_test() {
  let wire =
    "{\"id\":\"1\",\"channels\":[],\"members\":[{\"user\":{\"id\":\"42\"}},{\"user\":{\"id\":\"43\"}}],\"member_count\":2}"
  created.from_wire(guild(1), raw(wire))
  |> created.to_raw
  |> text
  |> should.equal(wire)
}

pub fn ready_member_keeps_key_order_and_echoes_ids_test() {
  let assert Available(_, body) =
    created.from_wire(
      guild(1),
      raw(
        "{\"id\":\"1\",\"members\":[{\"nick\":\"a\",\"user\":{\"id\":42,\"username\":\"x\",\"global_name\":\"G\",\"avatar\":\"h\",\"profile_restricted\":true,\"bio\":\"b\"},\"roles\":[]}],\"member_count\":1}",
      ),
    )
  let ReadyGuild(wire:, users:) = created.ready(body)
  text(wire)
  |> should.equal(
    "{\"id\":\"1\",\"members\":[{\"nick\":\"a\",\"user\":{\"id\":\"42\"},\"roles\":[]}],\"member_count\":1}",
  )
  case users {
    [#(42, partial)] ->
      text(partial)
      == "{\"id\":\"42\",\"username\":\"x\",\"global_name\":null,\"avatar\":null}"
    _ -> False
  }
  |> should.be_true
}

pub fn member_without_user_id_stays_whole_test() {
  let assert Available(_, body) =
    created.from_wire(
      guild(1),
      raw("{\"id\":\"1\",\"members\":[{\"user\":{\"username\":\"x\"}}]}"),
    )
  let ReadyGuild(wire:, users:) = created.ready(body)
  users |> should.equal([])
  text(wire)
  |> should.equal(
    "{\"id\":\"1\",\"members\":[{\"user\":{\"username\":\"x\"}}]}",
  )
}

pub fn bot_guild_create_is_marked_available_test() {
  let assert Available(_, body) =
    created.from_wire(
      guild(1),
      raw(
        "{\"id\":\"1\",\"members\":[{\"user\":{\"id\":\"42\",\"username\":\"x\"}}]}",
      ),
    )
  created.bot_wire(body)
  |> text
  |> should.equal(
    "{\"unavailable\":false,\"id\":\"1\",\"members\":[{\"user\":{\"id\":\"42\"}}]}",
  )
}

pub fn unavailable_wire_reads_as_unavailable_test() {
  created.from_wire(
    guild(7),
    raw("{\"id\":\"7\",\"unavailable\":true,\"unavailable_hidden\":true}"),
  )
  |> should.equal(created.Unavailable(guild(7), True))
}
