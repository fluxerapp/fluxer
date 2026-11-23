// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/channel.{type ChannelKind}
import fluxer_gateway/core/id.{type ChannelId, type UserId} as core_id
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/threads/value.{type Object}
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string

pub opaque type Thread {
  Thread(
    id: ChannelId,
    parent: Option(ChannelId),
    kind: ChannelKind,
    archived: Bool,
    listed: List(Int),
    object: Object,
  )
}

pub opaque type ThreadMember {
  ThreadMember(thread: ChannelId, user: UserId, object: Object)
}

const dropped_thread_keys = [
  "member", "newly_created", "position", "permission_overwrites", "nsfw",
  "topic",
]

fn normalize(object: Object) -> Object {
  let object = case value.field(object, "_fluxer_member_ids_preview") {
    Some(json_tree.Array(preview)) ->
      value.insert(object, "member_ids_preview", json_tree.Array(preview))
    _ -> object
  }
  object
  |> value.without(dropped_thread_keys)
  |> strip_internal
}

pub fn strip_internal(object: Object) -> Object {
  value.filter_keys(object, fn(key) { !is_internal_key(key) })
}

pub fn is_internal_key(key: String) -> Bool {
  string.starts_with(key, "_fluxer_") || string.starts_with(key, "__thread_")
}

pub fn from_event(object: Object) -> Result(Thread, Nil) {
  from_object(normalize(object), [])
}

pub fn from_object(object: Object, listed: List(Int)) -> Result(Thread, Nil) {
  use raw_id <- result.try(
    value.snowflake_field(object, "id") |> option.to_result(Nil),
  )
  use thread_id <- result.map(
    core_id.channel_id(raw_id) |> result.replace_error(Nil),
  )
  Thread(
    id: thread_id,
    parent: parent_of(object),
    kind: kind_of(object),
    archived: archived_in(object),
    listed:,
    object:,
  )
}

pub fn context(
  thread_id: ChannelId,
  object: Object,
  listed: List(Int),
) -> Thread {
  let object =
    value.insert(
      object,
      "id",
      value.id_string(core_id.channel_id_to_int(thread_id)),
    )
  Thread(
    id: thread_id,
    parent: parent_of(object),
    kind: kind_of(object),
    archived: archived_in(object),
    listed:,
    object:,
  )
}

fn parent_of(object: Object) -> Option(ChannelId) {
  value.snowflake_field(object, "parent_id")
  |> option.then(fn(raw) { core_id.channel_id(raw) |> option.from_result })
}

fn kind_of(object: Object) -> ChannelKind {
  case value.int_field(object, "type") {
    Some(code) -> channel.kind_from_int(code)
    None -> channel.OtherKind(-1)
  }
}

fn archived_in(object: Object) -> Bool {
  case value.object_field(object, "thread_metadata") {
    Some(metadata) -> value.is_true(metadata, "archived")
    None -> False
  }
}

pub fn id(thread: Thread) -> ChannelId {
  thread.id
}

pub fn id_int(thread: Thread) -> Int {
  core_id.channel_id_to_int(thread.id)
}

pub fn parent(thread: Thread) -> Option(ChannelId) {
  thread.parent
}

pub fn kind(thread: Thread) -> ChannelKind {
  thread.kind
}

pub fn is_private(thread: Thread) -> Bool {
  thread.kind == channel.PrivateThread
}

pub fn is_archived(thread: Thread) -> Bool {
  thread.archived
}

pub fn listed_members(thread: Thread) -> List(Int) {
  thread.listed
}

pub fn object(thread: Thread) -> Object {
  thread.object
}

pub fn name(thread: Thread) -> Option(String) {
  case value.string_field(thread.object, "name") {
    Some("") -> None
    other -> other
  }
}

pub fn last_message_id(thread: Thread) -> Option(Int) {
  value.snowflake_field(thread.object, "last_message_id")
}

pub fn counter(thread: Thread, key: String) -> Int {
  case value.int_field(thread.object, key) {
    Some(count) if count >= 0 -> count
    _ -> 0
  }
}

pub fn put(thread: Thread, key: String, entry: JsonTree) -> Thread {
  Thread(..thread, object: value.insert(thread.object, key, entry))
}

pub fn merge(existing: Thread, update: Thread) -> Thread {
  let object = value.merge(existing.object, update.object)
  Thread(
    ..existing,
    parent: parent_of(object),
    kind: kind_of(object),
    archived: archived_in(object),
    object:,
  )
}

pub fn payload(thread: Thread) -> Object {
  value.without(thread.object, ["member_ids_preview", "member"])
}

pub fn same_parent_public(a: Thread, b: Thread) -> Bool {
  a.parent == b.parent && !is_private(a) && !is_private(b)
}

pub fn member_from_object(object: Object) -> Result(ThreadMember, Nil) {
  let thread_raw = value.snowflake_field(object, "id")
  let user_raw = value.snowflake_field(object, "user_id")
  case thread_raw, user_raw {
    Some(thread_raw), Some(user_raw) ->
      case core_id.channel_id(thread_raw), core_id.user_id(user_raw) {
        Ok(thread), Ok(user) ->
          Ok(ThreadMember(
            thread:,
            user:,
            object: value.without(object, ["member", "presence", "guild_id"]),
          ))
        _, _ -> Error(Nil)
      }
    _, _ -> Error(Nil)
  }
}

pub fn member_thread(member: ThreadMember) -> ChannelId {
  member.thread
}

pub fn member_user(member: ThreadMember) -> UserId {
  member.user
}

pub fn member_object(member: ThreadMember) -> Object {
  member.object
}

pub fn own_member_payload(member: ThreadMember) -> Object {
  value.without(member.object, ["id", "user_id", "member", "presence"])
}

pub fn member_flags(member: ThreadMember) -> JsonTree {
  value.get(member.object, "flags") |> result.unwrap(json_tree.Int(0))
}

pub fn member_join_timestamp(member: ThreadMember) -> JsonTree {
  value.get(member.object, "join_timestamp") |> result.unwrap(json_tree.Null)
}
