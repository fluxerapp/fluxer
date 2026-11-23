// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/dispatch_facts.{
  type Detail, type DispatchFacts, DispatchFacts, ReactionFacts,
}
import fluxer_gateway/core/json_tree.{type JsonTree}
import fluxer_gateway/core/permissions.{type Permission}
import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

const max_mention_roles = 100

pub type Placement {
  MessagePlacement
  UserPlacement
  NoPlacement
}

pub type Scope {
  GuildWide
  ChannelEvent(channel: Option(Int))
  InviteEvent(channel: Option(Int))
  GuildPermissionEvent(required: Permission)
}

pub type EventFacts {
  EventFacts(
    channel: Option(Int),
    user: Option(Int),
    author: Option(Int),
    mentions: Mentions,
  )
}

pub type Mentions {
  Mentions(everyone: Bool, here: Bool, users: Set(Int), roles: Set(Int))
}

pub fn no_facts() -> EventFacts {
  EventFacts(
    channel: None,
    user: None,
    author: None,
    mentions: Mentions(
      everyone: False,
      here: False,
      users: set.new(),
      roles: set.new(),
    ),
  )
}

pub fn normalize(event: String) -> Result(String, Nil) {
  let upper = string.uppercase(event)
  case upper != "" && all_name_bytes(<<upper:utf8>>) {
    True -> Ok(upper)
    False -> Error(Nil)
  }
}

fn all_name_bytes(bytes: BitArray) -> Bool {
  case bytes {
    <<>> -> True
    <<byte, rest:bits>>
      if byte == 95
      || { byte >= 65 && byte <= 90 }
      || { byte >= 48 && byte <= 57 }
    -> all_name_bytes(rest)
    _ -> False
  }
}

fn is_channel_scoped(event: String) -> Bool {
  case event {
    "CHANNEL_CREATE"
    | "CHANNEL_UPDATE"
    | "CHANNEL_DELETE"
    | "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI"
    | "TYPING_START"
    | "CHANNEL_PINS_UPDATE"
    | "WEBHOOKS_UPDATE" -> True
    _ -> False
  }
}

pub fn is_message_access(event: String) -> Bool {
  case event {
    "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_REACTION_ADD"
    | "MESSAGE_REACTION_REMOVE"
    | "MESSAGE_REACTION_REMOVE_ALL"
    | "MESSAGE_REACTION_REMOVE_EMOJI" -> True
    _ -> False
  }
}

fn is_invite(event: String) -> Bool {
  event == "INVITE_CREATE" || event == "INVITE_DELETE"
}

fn required_permission(event: String) -> Option(Permission) {
  case event {
    "GUILD_AUDIT_LOG_ENTRY_CREATE" -> Some(permissions.ViewAuditLog)
    "GUILD_BAN_ADD" | "GUILD_BAN_REMOVE" -> Some(permissions.BanMembers)
    _ -> None
  }
}

pub fn is_bulk_update(event: String) -> Bool {
  event == "CHANNEL_UPDATE_BULK"
}

pub fn is_reaction(event: String) -> Bool {
  event == "MESSAGE_REACTION_ADD" || event == "MESSAGE_REACTION_REMOVE"
}

pub fn member_placement(event: String) -> Placement {
  case event {
    "MESSAGE_CREATE"
    | "MESSAGE_UPDATE"
    | "MESSAGE_DELETE"
    | "MESSAGE_DELETE_BULK" -> MessagePlacement
    "TYPING_START" | "MESSAGE_REACTION_ADD" | "MESSAGE_REACTION_REMOVE" ->
      UserPlacement
    _ -> NoPlacement
  }
}

pub fn scope(event: String, data: JsonTree) -> Scope {
  case is_channel_scoped(event), is_invite(event), required_permission(event) {
    True, _, _ -> ChannelEvent(channel_of(event, data))
    False, True, _ -> InviteEvent(invite_channel(data))
    False, False, Some(required) -> GuildPermissionEvent(required)
    False, False, None -> GuildWide
  }
}

pub fn channel_of(event: String, data: JsonTree) -> Option(Int) {
  case event {
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      snowflake(data, "id")
    _ -> snowflake(data, "channel_id")
  }
}

pub fn invite_channel(data: JsonTree) -> Option(Int) {
  case json_tree.field(data, "channel_id") {
    Ok(_) -> snowflake(data, "channel_id")
    Error(Nil) -> at(data, ["channel", "id"])
  }
}

pub fn message_id(data: JsonTree) -> Option(Int) {
  case json_tree.field(data, "message_id") {
    Ok(_) -> snowflake(data, "message_id")
    Error(Nil) -> snowflake(data, "id")
  }
}

pub fn member_user(event: String, data: JsonTree) -> Option(Int) {
  case member_placement(event) {
    MessagePlacement ->
      case json_tree.path(data, ["author", "id"]) {
        Ok(author) -> json_tree.snowflake(author) |> option.from_result
        Error(Nil) -> snowflake(data, "author_id")
      }
    UserPlacement -> snowflake(data, "user_id")
    NoPlacement -> None
  }
}

pub fn event_user(data: JsonTree) -> Option(Int) {
  at(data, ["user", "id"])
}

pub fn event_author(data: JsonTree) -> Option(Int) {
  at(data, ["author", "id"])
}

pub fn facts(event: String, data: JsonTree) -> EventFacts {
  EventFacts(
    channel: channel_of(event, data),
    user: event_user(data),
    author: event_author(data),
    mentions: Mentions(
      everyone: flag(data, "mention_everyone"),
      here: flag(data, "mention_here"),
      users: json_tree.field(data, "mentions")
        |> result.try(json_tree.items)
        |> result.unwrap([])
        |> list.filter_map(json_tree.snowflake_field(_, "id"))
        |> set.from_list,
      roles: json_tree.field(data, "mention_roles")
        |> result.try(json_tree.items)
        |> result.unwrap([])
        |> list.take(max_mention_roles)
        |> list.filter_map(json_tree.snowflake)
        |> set.from_list,
    ),
  )
}

fn flag(data: JsonTree, key: String) -> Bool {
  json_tree.field(data, key) == Ok(json_tree.Bool(True))
}

fn snowflake(data: JsonTree, key: String) -> Option(Int) {
  json_tree.snowflake_field(data, key) |> option.from_result
}

fn at(data: JsonTree, keys: List(String)) -> Option(Int) {
  json_tree.path(data, keys)
  |> result.try(json_tree.snowflake)
  |> option.from_result
}

pub fn unavailable_feature(feature: String) -> Bool {
  feature == "UNAVAILABLE_FOR_EVERYONE"
  || feature == "UNAVAILABLE_FOR_EVERYONE_BUT_STAFF"
}

pub fn dispatch_facts(event: String, data: JsonTree) -> DispatchFacts {
  case json_tree.expand(data), event {
    Ok(json_tree.Object(_)), "PRESENCE_UPDATE" ->
      dispatch_facts.scan(event, json_tree.to_raw(data))
    Ok(json_tree.Object(_) as object), _ ->
      DispatchFacts(
        guild_id: snowflake(object, "guild_id"),
        id: snowflake(object, "id"),
        thread: thread_marker(object),
        detail: detail(event, object),
      )
    _, _ ->
      DispatchFacts(
        guild_id: None,
        id: None,
        thread: dispatch_facts.Unmarked,
        detail: dispatch_facts.NotObject,
      )
  }
}

fn thread_marker(data: JsonTree) -> dispatch_facts.ThreadMarker {
  case
    json_tree.field(data, dispatch_facts.scoped_marker),
    json_tree.field(data, dispatch_facts.unscoped_marker)
  {
    Ok(_), _ -> dispatch_facts.Scoped
    Error(Nil), Ok(_) -> dispatch_facts.Unscoped
    Error(Nil), Error(Nil) -> dispatch_facts.Unmarked
  }
}

fn detail(event: String, data: JsonTree) -> Detail {
  case event {
    "GUILD_CREATE" ->
      dispatch_facts.Guild(
        unavailable: flag(data, "unavailable"),
        members: present(data, "members"),
      )
    "GUILD_DELETE" ->
      dispatch_facts.Guild(
        unavailable: flag(data, "unavailable"),
        members: None,
      )
    "GUILD_MEMBER_REMOVE" -> dispatch_facts.MemberRemove(user: event_user(data))
    "MESSAGE_CREATE" ->
      dispatch_facts.Message(
        everyone: flag(data, "mention_everyone"),
        here: flag(data, "mention_here"),
        mentions: json_tree.field(data, "mentions")
          |> result.try(json_tree.items)
          |> result.unwrap([])
          |> list.filter_map(json_tree.snowflake_field(_, "id")),
      )
    "MESSAGE_REACTION_ADD" | "MESSAGE_REACTION_REMOVE" ->
      dispatch_facts.Reaction(reaction(data))
    "CHANNEL_CREATE" | "CHANNEL_UPDATE" | "CHANNEL_DELETE" ->
      dispatch_facts.Channel(
        kind: json_tree.field(data, "type")
          |> result.try(json_tree.int)
          |> option.from_result,
        recipients: recipients(data),
      )
    "CHANNEL_RECIPIENT_ADD" | "CHANNEL_RECIPIENT_REMOVE" ->
      dispatch_facts.Recipient(
        channel: snowflake(data, "channel_id"),
        user: event_user(data),
      )
    "RELATIONSHIP_ADD" | "RELATIONSHIP_UPDATE" | "RELATIONSHIP_REMOVE" ->
      dispatch_facts.Relationship(
        kind: json_tree.field(data, "type")
        |> result.try(json_tree.int)
        |> option.from_result,
      )
    "USER_SETTINGS_UPDATE" ->
      dispatch_facts.UserSettings(
        status: json_tree.field(data, "status")
        |> result.try(json_tree.string)
        |> option.from_result,
      )
    "USER_UPDATE" ->
      dispatch_facts.UserUpdate(
        restricted: json_tree.field(data, "profile_restricted")
        |> result.map(fn(value) { json_tree.bool(value) == Ok(True) })
        |> option.from_result,
      )
    _ -> dispatch_facts.Plain
  }
}

fn reaction(data: JsonTree) -> dispatch_facts.ReactionFacts {
  let emoji = json_tree.field(data, "emoji")
  ReactionFacts(
    guild: snowflake(data, "guild_id"),
    channel: snowflake(data, "channel_id"),
    message: snowflake(data, "message_id"),
    user: snowflake(data, "user_id"),
    emoji_id: emoji
      |> result.try(json_tree.snowflake_field(_, "id"))
      |> option.from_result,
    emoji_name: emoji
      |> result.try(json_tree.field(_, "name"))
      |> result.try(json_tree.string)
      |> option.from_result,
    guild_raw: present(data, "guild_id"),
    channel_raw: present(data, "channel_id"),
    message_raw: present(data, "message_id"),
    user_raw: present(data, "user_id"),
    emoji_raw: present(data, "emoji"),
    member_raw: present(data, "member"),
  )
}

fn recipients(data: JsonTree) -> Option(Set(Int)) {
  let listed =
    json_tree.field(data, "recipient_ids")
    |> result.try(json_tree.items)
    |> result.unwrap([])
    |> list.filter_map(json_tree.snowflake)
  let users =
    json_tree.field(data, "recipients")
    |> result.try(json_tree.items)
    |> result.unwrap([])
    |> list.filter_map(recipient)
  case list.append(listed, users) {
    [] -> None
    ids -> Some(set.from_list(ids))
  }
}

fn recipient(item: JsonTree) -> Result(Int, Nil) {
  case json_tree.expand(item) {
    Ok(json_tree.Object(_) as object) -> json_tree.snowflake_field(object, "id")
    _ -> json_tree.snowflake(item)
  }
}

fn present(data: JsonTree, key: String) -> Option(RawJson) {
  case json_tree.field(data, key) {
    Ok(json_tree.Null) | Error(Nil) -> None
    Ok(json_tree.Raw(raw)) ->
      case raw_json.to_bits(raw) {
        <<"null">> -> None
        _ -> Some(raw)
      }
    Ok(value) -> Some(json_tree.to_raw(value))
  }
}
