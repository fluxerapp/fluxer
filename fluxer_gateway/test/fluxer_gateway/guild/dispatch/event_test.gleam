// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/json_tree
import fluxer_gateway/core/permissions
import fluxer_gateway/guild/dispatch/event
import gleam/option.{None, Some}
import gleam/set

pub fn normalize_test() {
  assert event.normalize("message_create") == Ok("MESSAGE_CREATE")
  assert event.normalize("") == Error(Nil)
  assert event.normalize("bad name") == Error(Nil)
}

pub fn channel_of_uses_id_for_channel_events_test() {
  let data =
    json_tree.Object([
      #("id", json_tree.String("5")),
      #("channel_id", json_tree.String("6")),
    ])
  assert event.channel_of("CHANNEL_UPDATE", data) == Some(5)
  assert event.channel_of("MESSAGE_CREATE", data) == Some(6)
}

pub fn scope_test() {
  let data = json_tree.Object([#("channel_id", json_tree.String("6"))])
  assert event.scope("TYPING_START", data) == event.ChannelEvent(Some(6))
  assert event.scope("INVITE_CREATE", data) == event.InviteEvent(Some(6))
  assert event.scope("GUILD_AUDIT_LOG_ENTRY_CREATE", data)
    == event.GuildPermissionEvent(permissions.ViewAuditLog)
  assert event.scope("GUILD_BAN_ADD", data)
    == event.GuildPermissionEvent(permissions.BanMembers)
  assert event.scope("GUILD_BAN_REMOVE", data)
    == event.GuildPermissionEvent(permissions.BanMembers)
  assert event.scope("GUILD_UPDATE", data) == event.GuildWide
}

pub fn invite_channel_falls_back_to_nested_channel_test() {
  let data =
    json_tree.Object([
      #("channel", json_tree.Object([#("id", json_tree.String("11"))])),
    ])
  assert event.invite_channel(data) == Some(11)
  assert event.invite_channel(json_tree.Object([])) == None
}

pub fn message_id_prefers_message_id_test() {
  let data =
    json_tree.Object([
      #("message_id", json_tree.String("3")),
      #("id", json_tree.String("4")),
    ])
  assert event.message_id(data) == Some(3)
  assert event.message_id(json_tree.Object([#("id", json_tree.String("4"))]))
    == Some(4)
}

pub fn member_user_test() {
  let message =
    json_tree.Object([
      #("author", json_tree.Object([#("id", json_tree.String("8"))])),
    ])
  assert event.member_user("MESSAGE_CREATE", message) == Some(8)
  let typing = json_tree.Object([#("user_id", json_tree.String("9"))])
  assert event.member_user("TYPING_START", typing) == Some(9)
  assert event.member_user("GUILD_UPDATE", typing) == None
}

pub fn facts_collect_mentions_once_test() {
  let data =
    json_tree.Object([
      #("channel_id", json_tree.String("6")),
      #("author", json_tree.Object([#("id", json_tree.String("8"))])),
      #(
        "mentions",
        json_tree.Array([json_tree.Object([#("id", json_tree.String("4"))])]),
      ),
      #("mention_roles", json_tree.Array([json_tree.String("9")])),
      #("mention_here", json_tree.Bool(True)),
    ])
  let facts = event.facts("MESSAGE_CREATE", data)
  assert facts.channel == Some(6)
  assert facts.author == Some(8)
  assert facts.user == None
  assert set.to_list(facts.mentions.users) == [4]
  assert set.to_list(facts.mentions.roles) == [9]
  assert facts.mentions.here
  assert !facts.mentions.everyone
}
