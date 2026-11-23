// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/core/id.{type GuildId}
import fluxer_gateway/core/node
import fluxer_gateway/guild/inbox.{type Handle}
import fluxer_gateway/guild/manager
import fluxer_gateway/guild/manager/shard
import fluxer_gateway/util/decode as id_decode
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom
import gleam/erlang/process
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import support/guild_api.{type FakeApi}

pub const view_channel = 1024

pub const send_messages = 2048

pub const read_history = 65_536

pub const connect = 1_048_576

pub const administrator = 8

pub const view_members = 18_014_398_509_481_984

pub const member_bits = 3_213_312

pub type Guild {
  Guild(
    id: Int,
    owner: Int,
    properties: List(#(String, Json)),
    everyone: Int,
    roles: List(Json),
    channels: List(Json),
    forums: List(Json),
    members: List(#(Int, Json)),
  )
}

pub fn unique_guild_id() -> Int {
  1_000_000_000 + int.random(1_000_000_000)
}

pub fn new(owner: Int) -> Guild {
  let guild = unique_guild_id()
  Guild(
    id: guild,
    owner:,
    properties: [#("name", json.string("Scenario"))],
    everyone: everyone_bits(),
    roles: [],
    channels: [],
    forums: [],
    members: [#(owner, member(owner, []))],
  )
}

pub fn everyone_bits() -> Int {
  view_channel + send_messages + read_history + connect + view_members
}

pub fn role(role_id: Int, permissions: Int, position: Int) -> Json {
  json.object([
    #("id", json.string(int.to_string(role_id))),
    #("name", json.string("role" <> int.to_string(role_id))),
    #("permissions", json.string(int.to_string(permissions))),
    #("position", json.int(position)),
  ])
}

pub fn with_role(
  found: Guild,
  role_id: Int,
  permissions: Int,
  position: Int,
) -> Guild {
  Guild(
    ..found,
    roles: list.append(found.roles, [role(role_id, permissions, position)]),
  )
}

pub fn with_everyone(found: Guild, permissions: Int) -> Guild {
  Guild(..found, everyone: permissions)
}

pub fn roles_json(found: Guild) -> Json {
  json.preprocessed_array([role(found.id, found.everyone, 0), ..found.roles])
}

pub type Overwrite {
  RoleDeny(role: Int, deny: Int)
  RoleAllow(role: Int, allow: Int)
  MemberAllow(user: Int, allow: Int)
  MemberDeny(user: Int, deny: Int)
}

pub fn overwrite_json(item: Overwrite) -> Json {
  let #(target, kind, allow, deny) = case item {
    RoleDeny(role, deny) -> #(role, 0, 0, deny)
    RoleAllow(role, allow) -> #(role, 0, allow, 0)
    MemberAllow(user, allow) -> #(user, 1, allow, 0)
    MemberDeny(user, deny) -> #(user, 1, 0, deny)
  }
  json.object([
    #("id", json.string(int.to_string(target))),
    #("type", json.int(kind)),
    #("allow", json.string(int.to_string(allow))),
    #("deny", json.string(int.to_string(deny))),
  ])
}

pub fn channel(
  guild: Int,
  channel_id: Int,
  kind: Int,
  parent: Option(Int),
  overwrites: List(Overwrite),
) -> Json {
  json.object([
    #("id", json.string(int.to_string(channel_id))),
    #("guild_id", json.string(int.to_string(guild))),
    #("type", json.int(kind)),
    #("name", json.string("c" <> int.to_string(channel_id))),
    #("position", json.int(0)),
    #("parent_id", case parent {
      Some(found) -> json.string(int.to_string(found))
      None -> json.null()
    }),
    #("permission_overwrites", json.array(overwrites, overwrite_json)),
  ])
}

pub fn with_channel(
  found: Guild,
  channel_id: Int,
  kind: Int,
  parent: Option(Int),
  overwrites: List(Overwrite),
) -> Guild {
  Guild(
    ..found,
    channels: list.append(found.channels, [
      channel(found.id, channel_id, kind, parent, overwrites),
    ]),
  )
}

pub fn with_text(found: Guild, channel_id: Int) -> Guild {
  with_channel(found, channel_id, 0, None, [])
}

pub fn user(user_id: Int) -> Json {
  json.object([
    #("id", json.string(int.to_string(user_id))),
    #("username", json.string("user" <> int.to_string(user_id))),
    #("global_name", json.null()),
  ])
}

pub fn member(user_id: Int, roles: List(Int)) -> Json {
  json.object([
    #("user", user(user_id)),
    #("nick", json.null()),
    #("roles", json.array(roles, fn(role) { json.string(int.to_string(role)) })),
    #("joined_at", json.string("2026-01-01T00:00:00.000Z")),
  ])
}

pub fn with_member(found: Guild, user_id: Int, roles: List(Int)) -> Guild {
  Guild(
    ..found,
    members: list.append(found.members, [#(user_id, member(user_id, roles))]),
  )
}

pub fn with_property(found: Guild, key: String, value: Json) -> Guild {
  Guild(..found, properties: [#(key, value), ..found.properties])
}

pub fn guild_json(found: Guild) -> Json {
  json.object(
    list.append(found.properties, [
      #("id", json.string(int.to_string(found.id))),
      #("owner_id", json.string(int.to_string(found.owner))),
    ]),
  )
}

pub fn publish(api: FakeApi, found: Guild) -> Nil {
  guild_api.put(api, found.id, "guild", guild_json(found))
  guild_api.put(api, found.id, "roles", roles_json(found))
  guild_api.put_data(
    api,
    found.id,
    "channels",
    json.object([
      #("channels", json.preprocessed_array(found.channels)),
      #("thread_only_channels", json.preprocessed_array(found.forums)),
    ]),
  )
  guild_api.put(api, found.id, "emojis", json.preprocessed_array([]))
  guild_api.put(api, found.id, "stickers", json.preprocessed_array([]))
  guild_api.put_members(api, found.id, found.members)
}

pub fn guild_id(found: Guild) -> GuildId {
  let assert Ok(guild) = id.guild_id(found.id)
  guild
}

pub fn start(api: FakeApi, found: Guild) -> Result(Handle, shard.LookupError) {
  publish(api, found)
  manager.start_or_lookup(node.self(), guild_id(found), 10_000)
}

pub fn started(api: FakeApi, found: Guild) -> Handle {
  let assert Ok(handle) = start(api, found)
  handle
}

pub fn dispatch(handle: Handle, event: String, payload: Json) -> Nil {
  process.send(handle.control, inbox.Dispatch(event, to_dynamic(payload)))
}

pub fn to_dynamic(payload: Json) -> Dynamic {
  let assert Ok(value) = id_decode.decode_json(<<json.to_string(payload):utf8>>)
  value
}

pub fn ask(handle: Handle, query: inbox.Query) -> inbox.QueryReply {
  let reply = process.new_subject()
  process.send(
    handle.control,
    inbox.Ask(query, clock.monotonic_ms() + 5000, reply),
  )
  let assert Ok(answer) = process.receive(reply, 5000)
  answer
}

pub fn sync(handle: Handle) -> Nil {
  let reply = process.new_subject()
  process.send(handle.control, inbox.Describe(reply))
  let assert Ok(_) = process.receive(reply, 5000)
  Nil
}

pub fn stop(found: Guild) -> Nil {
  let reply = process.new_subject()
  let _ =
    manager.send_local(guild_id(found), shard.StopGuild(guild_id(found), reply))
  let _ = process.receive(reply, 5000)
  Nil
}

pub fn slow(seconds: Int, body: fn() -> Nil) -> #(atom.Atom, Int, fn() -> Nil) {
  #(atom.create("timeout"), seconds, body)
}
