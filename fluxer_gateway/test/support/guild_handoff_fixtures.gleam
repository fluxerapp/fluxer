// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import fluxer_gateway/core/id
import fluxer_gateway/guild/data.{type Loaded}
import fluxer_gateway/guild/data/decode as data_decode
import fluxer_gateway/guild/handoff/codec as guild_codec
import fluxer_gateway/guild/handoff/wire_v1.{EventV1}
import fluxer_gateway/guild/handoff/wire_v2.{type GuildBulkV2}
import fluxer_gateway/guild/handoff/wire_v3.{type GuildTailV3, GuildTailV3}
import fluxer_gateway/handoff/codec
import gleam/dict
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

pub const directory = "test/fixtures/handoff/guild"

pub const fixture_epoch = 7

pub fn guild_json() -> BitArray {
  <<
    "{\"id\":\"1200\",\"name\":\"Fixture\",\"owner_id\":\"5\",\"features\":[],\"nsfw_level\":0}":utf8,
  >>
}

pub fn roles_json() -> BitArray {
  <<
    "[{\"id\":\"1200\",\"name\":\"@everyone\",\"permissions\":\"1024\",\"position\":0,\"color\":0,\"hoist\":false,\"mentionable\":false}]":utf8,
  >>
}

pub fn channels_json() -> BitArray {
  <<
    "[{\"id\":\"1300\",\"type\":0,\"name\":\"general\",\"position\":0,\"permission_overwrites\":[]}]":utf8,
  >>
}

pub fn member_json(user: Int) -> BitArray {
  let id = int.to_string(user)
  <<
    "{\"user\":{\"id\":\"":utf8,
    id:utf8,
    "\",\"username\":\"user":utf8,
    id:utf8,
    "\"},\"roles\":[],\"joined_at\":\"2024-01-01T00:00:00Z\"}":utf8,
  >>
}

pub fn loaded(member_ids: List(Int)) -> Loaded {
  let members =
    list.map(member_ids, member_json)
    |> list.intersperse(<<",">>)
    |> fn(rows) { [<<"[">>, ..list.append(rows, [<<"]">>])] }
  let assert Ok(found) =
    [
      #("guild", data_decode.Whole(guild_json())),
      #("roles", data_decode.Whole(roles_json())),
      #("channels", data_decode.Whole(channels_json())),
      #("emojis", data_decode.Whole(<<"[]">>)),
      #("stickers", data_decode.Whole(<<"[]">>)),
      #("members", data_decode.Whole(bit_concat(members))),
    ]
    |> dict.from_list
    |> data_decode.collections(guild_id(), _)
  found
}

fn bit_concat(parts: List(BitArray)) -> BitArray {
  list.fold(parts, <<>>, fn(acc, part) { <<acc:bits, part:bits>> })
}

fn guild_id() -> id.GuildId {
  let assert Ok(found) = id.guild_id(1200)
  found
}

pub fn expected_bulk() -> GuildBulkV2 {
  guild_codec.bulk_record(loaded([5, 6, 7]), 41)
}

@external(erlang, "gleam_stdlib", "identity")
fn to_dynamic(value: a) -> Dynamic

pub fn expected_tail() -> GuildTailV3 {
  GuildTailV3(
    guild_id: 1200,
    event_index: 44,
    loaded: Some(expected_bulk()),
    staged: None,
    sessions: [],
    threads: codec.term_body(#("threads", 1)),
    voice: codec.term_body(#("voice", 2)),
    delta: [
      EventV1(
        name: "GUILD_UPDATE",
        payload: codec.term_body(
          to_dynamic(dict.from_list([#("name", "Renamed")])),
        ),
      ),
      EventV1(name: "MESSAGE_CREATE", payload: codec.term_body(to_dynamic(42))),
    ],
    table: <<>>,
  )
}

pub fn tail_bytes(version: Int) -> Result(BitArray, codec.CodecError) {
  use body <- result.try(guild_codec.write_tail(expected_tail(), version))
  blob.wrap(
    ns: 0,
    codec_version: version,
    part: blob.Tail,
    flags: 0,
    epoch: fixture_epoch,
    body:,
  )
  |> result.map_error(codec.Envelope)
}

pub fn path(part: String, version: Int) -> String {
  directory <> "/" <> part <> "_v" <> int.to_string(version) <> ".bin"
}

@external(erlang, "file", "write_file")
fn write_file(path: String, bytes: BitArray) -> Dynamic

@external(erlang, "filelib", "ensure_dir")
fn ensure_dir(path: String) -> Dynamic

pub fn write_release(version: Int) -> Nil {
  let _ = ensure_dir(directory <> "/")
  let assert Ok(tail) = tail_bytes(version)
  let _ = write_file(path("tail", version), tail)
  Nil
}
