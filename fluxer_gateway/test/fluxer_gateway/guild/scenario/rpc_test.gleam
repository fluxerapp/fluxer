// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/rpc/server
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{None}
import gleam/string
import support/guild_api
import support/guild_fixture.{MemberAllow, RoleAllow, RoleDeny} as fixture

const role = 50

fn int_text(value: Int) -> String {
  json.to_string(json.int(value))
}

fn call(method: String, fields: List(#(String, Json))) -> Dynamic {
  let body =
    server.execute(method, <<json.to_string(json.object(fields)):utf8>>)
  let assert Ok(value) = id_decode.decode_json(<<body:utf8>>)
  value
}

fn ok_field(
  reply: Dynamic,
  path: List(String),
  decoder: decode.Decoder(a),
) -> Result(a, List(decode.DecodeError)) {
  let assert Ok(True) = decode.run(reply, decode.at(["ok"], decode.bool))
  decode.run(reply, decode.at(["result", ..path], decoder))
}

fn error_of(reply: Dynamic) -> Result(String, List(decode.DecodeError)) {
  decode.run(reply, decode.at(["error"], decode.string))
}

fn rpc_guild() -> fixture.Guild {
  let found = fixture.new(1)
  found
  |> fixture.with_role(role, 0, 1)
  |> fixture.with_text(10)
  |> fixture.with_channel(20, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    RoleAllow(role, fixture.view_channel),
  ])
  |> fixture.with_channel(30, 0, None, [
    RoleDeny(found.id, fixture.view_channel),
    MemberAllow(2, fixture.view_channel),
  ])
  |> fixture.with_member(2, [])
  |> fixture.with_member(3, [role])
}

fn guild_field(found: fixture.Guild) -> #(String, Json) {
  #("guild_id", json.string(int_text(found.id)))
}

pub fn rpc_starts_the_guild_and_answers_member_queries_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  fixture.publish(api, found)
  let reply =
    call("guild.has_member", [
      guild_field(found),
      #("user_id", json.string("3")),
    ])
  assert ok_field(reply, ["has_member"], decode.bool) == Ok(True)
  let reply =
    call("guild.get_member", [
      guild_field(found),
      #("user_id", json.string("9")),
    ])
  assert ok_field(reply, ["success"], decode.bool) == Ok(False)
  let reply =
    call("guild.get_members_with_role", [
      guild_field(found),
      #("role_id", json.string(int_text(role))),
    ])
  assert ok_field(reply, ["user_ids"], decode.list(decode.string)) == Ok(["3"])
  let first =
    call("guild.list_members_cursor", [
      guild_field(found),
      #("limit", json.int(2)),
    ])
  assert ok_field(
      first,
      ["members"],
      decode.list(decode.at(["user", "id"], decode.string)),
    )
    == Ok(["1", "2"])
  let rest =
    call("guild.list_members_cursor", [
      guild_field(found),
      #("limit", json.int(2)),
      #("after", json.string("2")),
    ])
  assert ok_field(
      rest,
      ["members"],
      decode.list(decode.at(["user", "id"], decode.string)),
    )
    == Ok(["3"])
  fixture.stop(found)
}

pub fn rpc_dispatch_reaches_the_read_model_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  let handle = fixture.started(api, found)
  let reply =
    call("guild.dispatch", [
      guild_field(found),
      #("event", json.string("GUILD_MEMBER_ADD")),
      #("data", fixture.member(4, [role])),
    ])
  assert ok_field(reply, [], decode.bool) == Ok(True)
  fixture.sync(handle)
  let reply =
    call("guild.has_member", [
      guild_field(found),
      #("user_id", json.string("4")),
    ])
  assert ok_field(reply, ["has_member"], decode.bool) == Ok(True)
  let channels =
    call("guild.get_viewable_channels", [
      guild_field(found),
      #("user_id", json.string("4")),
    ])
  assert ok_field(channels, ["channel_ids"], decode.list(decode.string))
    |> sorted
    == Ok(["10", "20"])
  let reply =
    call("guild.dispatch_many", [
      guild_field(found),
      #(
        "events",
        json.preprocessed_array([
          json.object([
            #("event", json.string("GUILD_MEMBER_REMOVE")),
            #("data", json.object([#("user", fixture.user(4))])),
          ]),
        ]),
      ),
    ])
  assert ok_field(reply, [], decode.bool) == Ok(True)
  fixture.sync(handle)
  let reply =
    call("guild.has_member", [
      guild_field(found),
      #("user_id", json.string("4")),
    ])
  assert ok_field(reply, ["has_member"], decode.bool) == Ok(False)
  let channels =
    call("guild.get_viewable_channels", [
      guild_field(found),
      #("user_id", json.string("4")),
    ])
  assert ok_field(channels, ["channel_ids"], decode.list(decode.string))
    == Ok([])
  fixture.stop(found)
}

fn sorted(found: Result(List(String), e)) -> Result(List(String), e) {
  case found {
    Ok(items) -> Ok(list.sort(items, string.compare))
    other -> other
  }
}

pub fn rpc_guild_data_is_filtered_per_member_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  let _ = fixture.started(api, found)
  let ids = fn(user: String) {
    call("guild.get_data", [guild_field(found), #("user_id", json.string(user))])
    |> ok_field(["channels"], decode.list(decode.at(["id"], decode.string)))
    |> sorted
  }
  assert ids("2") == Ok(["10", "30"])
  assert ids("3") == Ok(["10", "20"])
  assert ids("1") == Ok(["10", "20", "30"])
  let stranger =
    call("guild.get_data", [guild_field(found), #("user_id", json.string("9"))])
  assert error_of(stranger) == Ok("forbidden")
  fixture.stop(found)
}

pub fn rpc_permissions_follow_overwrites_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  let handle = fixture.started(api, found)
  let bits = fn(user: String, channel: String) {
    call("guild.get_user_permissions", [
      guild_field(found),
      #("user_id", json.string(user)),
      #("channel_id", json.string(channel)),
    ])
    |> ok_field(["permissions"], decode.string)
  }
  let view = int_text(fixture.view_channel)
  assert has_bit(bits("2", "30"), view)
  assert !has_bit(bits("2", "20"), view)
  assert has_bit(bits("3", "20"), view)
  fixture.dispatch(
    handle,
    "GUILD_ROLE_DELETE",
    json.object([
      guild_field(found),
      #("role_id", json.string(int_text(role))),
    ]),
  )
  fixture.sync(handle)
  process.sleep(10)
  assert !has_bit(bits("3", "20"), view)
  fixture.stop(found)
}

fn has_bit(found: Result(String, e), bit: String) -> Bool {
  let assert Ok(text) = found
  let assert Ok(value) = int.parse(text)
  let assert Ok(wanted) = int.parse(bit)
  int.bitwise_and(value, wanted) == wanted
}

pub fn rpc_reload_picks_up_new_api_state_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  let _ = fixture.started(api, found)
  fixture.publish(api, fixture.with_member(found, 7, []))
  let reply = call("guild.reload", [guild_field(found)])
  assert ok_field(reply, [], decode.bool) == Ok(True)
  let reply =
    call("guild.has_member", [
      guild_field(found),
      #("user_id", json.string("7")),
    ])
  assert ok_field(reply, ["has_member"], decode.bool) == Ok(True)
  fixture.stop(found)
}

pub fn rpc_for_an_unknown_guild_fails_cleanly_test() {
  use _api <- guild_api.with_api
  let reply =
    call("guild.has_member", [
      #("guild_id", json.string("4242")),
      #("user_id", json.string("1")),
    ])
  assert error_of(reply) == Ok("guild_not_found")
}

pub fn rpc_counts_follow_member_changes_test() {
  use api <- guild_api.with_api
  let found = rpc_guild()
  let handle = fixture.started(api, found)
  let counts = fn() {
    let reply = call("guild.get_counts", [guild_field(found)])
    ok_field(reply, ["member_count"], decode.int)
  }
  assert counts() == Ok(3)
  fixture.dispatch(handle, "GUILD_MEMBER_ADD", fixture.member(8, []))
  fixture.sync(handle)
  assert counts() == Ok(4)
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([guild_field(found), #("user", fixture.user(2))]),
  )
  fixture.dispatch(
    handle,
    "GUILD_MEMBER_REMOVE",
    json.object([guild_field(found), #("user", fixture.user(3))]),
  )
  fixture.sync(handle)
  assert counts() == Ok(2)
  let batch =
    call("guild.get_online_counts_batch", [
      #("guild_ids", json.array([int_text(found.id)], json.string)),
    ])
  assert ok_field(
      batch,
      ["online_counts"],
      decode.list(decode.at(["member_count"], decode.int)),
    )
    == Ok([2])
  fixture.stop(found)
}
