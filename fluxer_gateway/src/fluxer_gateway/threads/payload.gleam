// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type ChannelId}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions.{type Permissions}
import fluxer_gateway/core/snowflake
import fluxer_gateway/threads/thread
import fluxer_gateway/threads/value.{type Object}
import gleam/int
import gleam/list
import gleam/option.{None, Some}
import gleam/result

const thread_message_flags = 0x120

const thread_created_message = 18

const thread_starter_message = 21

const thread_permission_bits = 0x5C00000000

const surface_keys = [
  "thread_metadata", "applied_tags", "available_tags",
  "default_auto_archive_duration", "default_thread_rate_limit_per_user",
  "default_reaction_emoji", "default_sort_order", "default_forum_layout",
  "default_tag_setting", "message_count", "total_message_sent",
  "member_ids_preview", "member_count", "flags",
]

pub type StarterSource {
  NoSource
  UnknownSource
  ParentSource(parent: ChannelId, message: Int)
}

pub fn masks(event: String) -> Bool {
  case event {
    "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_UPDATE_BULK"
    | "GUILD_ROLE_CREATE"
    | "GUILD_ROLE_UPDATE"
    | "GUILD_ROLE_UPDATE_BULK" -> True
    _ -> False
  }
}

pub fn mask(event: String, data: JsonTree) -> JsonTree {
  case event {
    "MESSAGE_CREATE" | "MESSAGE_UPDATE" -> mask_message_tree(data)
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      mask_channel_tree(data)
    "CHANNEL_UPDATE_BULK" -> change_objects(data, "channels", mask_channel_tree)
    "GUILD_ROLE_CREATE" | "GUILD_ROLE_UPDATE" ->
      case child(data, "role") {
        Ok(json_tree.Object(_) as role) ->
          replace(data, "role", role, mask_role_tree(role))
        _ -> data
      }
    "GUILD_ROLE_UPDATE_BULK" -> change_objects(data, "roles", mask_role_tree)
    _ -> data
  }
}

fn change_objects(
  data: JsonTree,
  key: String,
  change: fn(JsonTree) -> JsonTree,
) -> JsonTree {
  case child(data, key) {
    Ok(json_tree.Array(items) as found) -> {
      let changed =
        list.filter_map(items, fn(item) {
          case json_tree.expand(item) {
            Ok(json_tree.Object(_) as object) -> Ok(change(object))
            _ -> Error(Nil)
          }
        })
      replace(data, key, found, json_tree.Array(changed))
    }
    _ -> data
  }
}

fn replace(
  data: JsonTree,
  key: String,
  old: JsonTree,
  new: JsonTree,
) -> JsonTree {
  case old == new {
    True -> data
    False -> json_tree.set(data, key, new) |> result.unwrap(data)
  }
}

fn child(data: JsonTree, key: String) -> Result(JsonTree, Nil) {
  json_tree.field(data, key) |> result.try(json_tree.expand)
}

fn without_key(data: JsonTree, key: String) -> JsonTree {
  case json_tree.field(data, key) {
    Ok(_) -> json_tree.remove(data, key) |> result.unwrap(data)
    Error(Nil) -> data
  }
}

fn mask_message_tree(message: JsonTree) -> JsonTree {
  let message = without_key(message, "thread")
  let message = case child(message, "flags") {
    Ok(json_tree.Int(flags) as found) ->
      replace(
        message,
        "flags",
        found,
        json_tree.Int(int.bitwise_and(
          flags,
          int.bitwise_not(thread_message_flags),
        )),
      )
    _ -> message
  }
  let message = case child(message, "referenced_message") {
    Ok(json_tree.Object(_) as referenced) ->
      case is_kind(referenced, thread_created_message) {
        True ->
          replace(message, "referenced_message", referenced, json_tree.Null)
        False ->
          replace(
            message,
            "referenced_message",
            referenced,
            mask_message_tree(referenced),
          )
      }
    _ -> message
  }
  case child(message, "message_snapshots") {
    Ok(json_tree.Array(snapshots) as found) ->
      replace(
        message,
        "message_snapshots",
        found,
        json_tree.Array(list.map(snapshots, mask_snapshot_tree)),
      )
    _ -> message
  }
}

fn mask_snapshot_tree(snapshot: JsonTree) -> JsonTree {
  case json_tree.expand(snapshot) |> result.unwrap(snapshot) {
    json_tree.Object(_) ->
      case child(snapshot, "message") {
        Ok(json_tree.Object(_) as inner) ->
          replace(snapshot, "message", inner, mask_message_tree(inner))
        _ -> mask_message_tree(snapshot)
      }
    other -> other
  }
}

fn is_kind(data: JsonTree, kind: Int) -> Bool {
  case child(data, "type") {
    Ok(json_tree.Int(number)) -> number == kind
    Ok(json_tree.String(text)) -> text == int.to_string(kind)
    _ -> False
  }
}

pub fn mask_channel_tree(channel: JsonTree) -> JsonTree {
  mask_overwrites_tree(strip_channel_surface_tree(channel))
}

pub fn strip_channel_surface_tree(channel: JsonTree) -> JsonTree {
  list.fold(surface_keys, channel, without_key)
}

pub fn mask_overwrites_tree(channel: JsonTree) -> JsonTree {
  case child(channel, "permission_overwrites") {
    Ok(json_tree.Array(overwrites) as found) ->
      replace(
        channel,
        "permission_overwrites",
        found,
        json_tree.Array(list.map(overwrites, mask_overwrite_tree)),
      )
    _ -> channel
  }
}

fn mask_overwrite_tree(overwrite: JsonTree) -> JsonTree {
  case json_tree.expand(overwrite) {
    Ok(json_tree.Object(fields)) -> {
      let masked =
        list.map(fields, fn(pair) {
          case pair.0 {
            "allow" | "deny" -> #(pair.0, mask_bits_tree(pair.1))
            _ -> pair
          }
        })
      case masked == fields {
        True -> overwrite
        False -> json_tree.Object(masked)
      }
    }
    _ -> overwrite
  }
}

pub fn mask_role_tree(role: JsonTree) -> JsonTree {
  case child(role, "permissions") {
    Ok(bits) -> replace(role, "permissions", bits, mask_bits_tree(bits))
    Error(Nil) -> role
  }
}

fn mask_bits_tree(entry: JsonTree) -> JsonTree {
  let mask = int.bitwise_not(thread_permission_bits)
  case json_tree.expand(entry) {
    Ok(json_tree.Int(bits)) ->
      case int.bitwise_and(bits, mask) {
        masked if masked == bits -> entry
        masked -> json_tree.Int(masked)
      }
    Ok(json_tree.String(text)) ->
      case permissions.from_wire(text) {
        Ok(bits) -> {
          let masked =
            int.to_string(int.bitwise_and(permissions.to_int(bits), mask))
          case masked == text {
            True -> entry
            False -> json_tree.String(masked)
          }
        }
        Error(_) -> entry
      }
    _ -> entry
  }
}

pub fn mask_channel(channel: Object) -> Object {
  mask_overwrites(strip_channel_surface(channel))
}

pub fn strip_channel_surface(channel: Object) -> Object {
  value.without(channel, surface_keys)
}

pub fn mask_role(role: Object) -> Object {
  case value.get(role, "permissions") {
    Ok(bits) -> value.insert(role, "permissions", mask_bits(bits))
    Error(Nil) -> role
  }
}

pub fn mask_overwrites(channel: Object) -> Object {
  case value.get(channel, "permission_overwrites") {
    Ok(json_tree.Array(overwrites)) ->
      value.insert(
        channel,
        "permission_overwrites",
        json_tree.Array(list.map(overwrites, mask_overwrite)),
      )
    _ -> channel
  }
}

fn mask_overwrite(overwrite: JsonTree) -> JsonTree {
  case overwrite {
    json_tree.Object(object) ->
      json_tree.Object(
        list.map(object, fn(pair) {
          case pair.0 {
            "allow" | "deny" -> #(pair.0, mask_bits(pair.1))
            _ -> pair
          }
        }),
      )
    other -> other
  }
}

fn mask_bits(entry: JsonTree) -> JsonTree {
  let mask = int.bitwise_not(thread_permission_bits)
  case entry {
    json_tree.Int(bits) -> json_tree.Int(int.bitwise_and(bits, mask))
    json_tree.String(text) ->
      case permissions.from_wire(text) {
        Ok(bits) ->
          json_tree.String(
            int.to_string(int.bitwise_and(permissions.to_int(bits), mask)),
          )
        Error(_) -> entry
      }
    other -> other
  }
}

pub fn strip_internal(data: Object) -> Object {
  thread.strip_internal(data)
}

pub fn starter_source(event: String, data: JsonTree) -> StarterSource {
  case
    event,
    is_kind(data, thread_starter_message),
    json_tree.field(data, "referenced_message") |> result.try(json_tree.expand)
  {
    "MESSAGE_CREATE", True, Ok(json_tree.Object(_)) -> parent_source(data)
    _, _, _ -> NoSource
  }
}

fn parent_source(data: JsonTree) -> StarterSource {
  let parsed = {
    use reference <- result.try(json_tree.field(data, "message_reference"))
    use parent <- result.try(
      json_tree.field(reference, "channel_id")
      |> result.try(json_tree.snowflake),
    )
    use raw_message <- result.try(
      case json_tree.field(reference, "message_id") {
        Ok(json_tree.String(raw_message)) -> Ok(raw_message)
        _ -> Error(Nil)
      },
    )
    use parent <- result.try(id.channel_id(parent) |> result.replace_error(Nil))
    use message <- result.map(id.parse(raw_message))
    ParentSource(parent, message)
  }
  result.unwrap(parsed, UnknownSource)
}

pub fn without_source(data: JsonTree) -> JsonTree {
  json_tree.set(data, "referenced_message", json_tree.Null)
  |> result.unwrap(data)
}

pub fn source_readable(
  source: StarterSource,
  permissions_for: fn(ChannelId) -> Permissions,
  cutoff_ms: option.Option(Int),
) -> Bool {
  case source {
    NoSource -> True
    UnknownSource -> False
    ParentSource(parent, message) ->
      permissions.has(permissions_for(parent), permissions.ReadMessageHistory)
      || case cutoff_ms {
        Some(cutoff) -> snowflake.timestamp_ms(message) >= cutoff
        None -> False
      }
  }
}

pub fn hide_unreadable_source(
  event: String,
  data: JsonTree,
  permissions_for: fn(ChannelId) -> Permissions,
  cutoff_ms: option.Option(Int),
) -> JsonTree {
  case starter_source(event, data) {
    NoSource -> data
    source ->
      case source_readable(source, permissions_for, cutoff_ms) {
        True -> data
        False -> without_source(data)
      }
  }
}
