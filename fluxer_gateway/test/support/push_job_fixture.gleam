// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/format
import fluxer_gateway/push/markdown.{type PreviewContext}
import fluxer_gateway/push/message.{type MessageCreate, type MessageData}
import gleam/dict
import gleam/option.{type Option, None, Some}
import gleam/set

pub const guild = 1_400_000_000_000_000_900

pub const channel = 1_400_000_000_000_000_100

pub const message_id = 1_400_000_000_000_000_500

pub const author = 1_400_000_000_000_000_007

pub const alice = 1_400_000_000_000_000_001

pub const bob = 1_400_000_000_000_000_002

pub const mods = 1_400_000_000_000_000_200

pub const other_role = 1_400_000_000_000_000_201

pub const parent = 1_400_000_000_000_000_050

pub const linked = 1_400_000_000_000_000_101

fn author_data() -> message.Author {
  message.Author(
    id: Some(author),
    username: Some("writer"),
    global_name: Some("The \"Writer\""),
    avatar: Some("a_hash"),
  )
}

fn rich_data() -> MessageData {
  message.MessageData(
    ..message.empty_data(),
    id: Some(message_id),
    channel_id: Some(channel),
    author: author_data(),
    content: "hey <@1400000000000000001> and <@1400000000000000002>, **see** <#1400000000000000100> <#1400000000000000101> ping <@&1400000000000000200> \u{1F600}",
    mentions: [
      message.Mention(
        id: Some(alice),
        username: Some("alice"),
        global_name: None,
      ),
      message.Mention(
        id: Some(bob),
        username: Some("bob"),
        global_name: Some("Bobby"),
      ),
    ],
    mention_roles: [mods],
    channel_type: Some(0),
    mention_channels: [
      message.MentionChannel(
        id: Some(linked),
        name: Some("linked"),
        kind: Some(0),
      ),
    ],
  )
}

pub fn guild_message() -> MessageCreate {
  let data = rich_data()
  let nicknames =
    dict.from_list([
      #(alice, "Ally"),
      #(author, "Scribe"),
    ])
  let roles = dict.from_list([#(mods, "mods"), #(other_role, "other")])
  let channels = dict.from_list([#(channel, "general")])
  create(
    data,
    guild,
    Some("Guild <One>"),
    Some("general"),
    format.markdown_context(data, nicknames, roles, channels),
  )
}

pub fn thread_message() -> MessageCreate {
  let data =
    message.MessageData(
      ..rich_data(),
      thread: Some(message.ThreadPush(
        parent_id: parent,
        parent_name: Some("forum"),
        category_id: None,
        forum_thread_created: True,
        members: dict.new(),
      )),
    )
  create(
    data,
    guild,
    Some("Guild"),
    Some("a thread"),
    format.markdown_context(data, dict.new(), dict.new(), dict.new()),
  )
}

pub fn age_restricted_message() -> MessageCreate {
  let data =
    message.MessageData(
      ..rich_data(),
      age_restricted: True,
      thread: Some(message.ThreadPush(
        parent_id: parent,
        parent_name: Some("forum"),
        category_id: None,
        forum_thread_created: False,
        members: dict.new(),
      )),
    )
  create(
    data,
    guild,
    Some("Guild"),
    Some("a thread"),
    format.markdown_context(data, dict.new(), dict.new(), dict.new()),
  )
}

pub fn dm_message() -> MessageCreate {
  let data =
    message.MessageData(
      ..rich_data(),
      channel_type: Some(1),
      nicks: dict.from_list([#(author, "Pal")]),
    )
  create(data, 0, None, None, format.private_markdown_context(data))
}

pub fn group_dm_attachment() -> MessageCreate {
  let data =
    message.MessageData(
      ..message.empty_data(),
      id: Some(message_id),
      channel_id: Some(channel),
      author: message.Author(
        id: Some(author),
        username: Some("writer"),
        global_name: None,
        avatar: None,
      ),
      channel_type: Some(3),
      attachments: [
        Some(message.Media(
          url: Some("https://cdn.example/a.png"),
          content_type: Some("image/png"),
          flags: Some(0),
          nsfw: False,
          filename: Some("cat.png"),
        )),
      ],
    )
  create(data, 0, None, Some("friends"), format.private_markdown_context(data))
}

fn create(
  data: MessageData,
  guild: Int,
  guild_name: Option(String),
  channel_name: Option(String),
  markdown: PreviewContext,
) -> MessageCreate {
  message.MessageCreate(
    data:,
    recipients: [alice, bob],
    guild:,
    author:,
    channel:,
    message: message_id,
    guild_default_level: 0,
    guild_name:,
    channel_name:,
    user_roles: dict.new(),
    connected: set.new(),
    markdown:,
    large: None,
    prefilter: message.NotFiltered,
  )
}
