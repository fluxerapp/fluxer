// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/push/markdown.{type PreviewContext}
import fluxer_gateway/push/settings.{type ThreadMember}
import fluxer_gateway/util/decode as edge
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode.{type Decoder}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/set.{type Set}
import gleam/string

const message_type_default = 0

const message_type_reply = 19

const flag_suppress_notifications = 4096

const max_guild_features = 64

pub type Author {
  Author(
    id: Option(Int),
    username: Option(String),
    global_name: Option(String),
    avatar: Option(String),
  )
}

pub type Mention {
  Mention(
    id: Option(Int),
    username: Option(String),
    global_name: Option(String),
  )
}

pub type Media {
  Media(
    url: Option(String),
    content_type: Option(String),
    flags: Option(Int),
    nsfw: Bool,
    filename: Option(String),
  )
}

pub type EmbedField {
  EmbedField(name: Option(String), value: Option(String))
}

pub type Embed {
  Embed(
    image: Option(Media),
    thumbnail: Option(Media),
    title: Option(String),
    description: Option(String),
    fields: List(Option(EmbedField)),
  )
}

pub type Snapshot {
  Snapshot(
    content: String,
    stickers: List(Option(String)),
    attachments: List(Option(Media)),
    embeds: List(Option(Embed)),
  )
}

pub type MentionChannel {
  MentionChannel(id: Option(Int), name: Option(String), kind: Option(Int))
}

pub type ThreadPush {
  ThreadPush(
    parent_id: Int,
    parent_name: Option(String),
    category_id: Option(Int),
    forum_thread_created: Bool,
    members: Dict(Int, ThreadMember),
  )
}

pub type MessageData {
  MessageData(
    id: Option(Int),
    channel_id: Option(Int),
    kind: Int,
    flags: Int,
    author: Author,
    content: String,
    mentions: List(Mention),
    mention_roles: List(Int),
    mention_everyone: Bool,
    mention_here: Bool,
    channel_type: Option(Int),
    age_restricted: Bool,
    thread: Option(ThreadPush),
    attachments: List(Option(Media)),
    embeds: List(Option(Embed)),
    stickers: List(Option(String)),
    snapshots: List(Option(Snapshot)),
    mention_channels: List(MentionChannel),
    nicks: Dict(Int, String),
    id_texts: Dict(Int, String),
  )
}

pub type Prefilter {
  NotFiltered
  FilteredDirect
  FilteredBulk
}

pub type LargeGuild {
  LargeGuild(member_count: Int, features: List(String))
}

pub type MessageCreate {
  MessageCreate(
    data: MessageData,
    recipients: List(Int),
    guild: Int,
    author: Int,
    channel: Int,
    message: Int,
    guild_default_level: Int,
    guild_name: Option(String),
    channel_name: Option(String),
    user_roles: Dict(Int, List(Int)),
    connected: Set(Int),
    markdown: PreviewContext,
    large: Option(LargeGuild),
    prefilter: Prefilter,
  )
}

pub type ParamsError {
  NoValidUsers
  InvalidChannelId
  InvalidMessageId
  UnpushableMessageType(kind: Int)
  SuppressedNotifications
}

pub fn validate(create: MessageCreate) -> Result(MessageCreate, ParamsError) {
  case create.recipients {
    [] -> Error(NoValidUsers)
    _ -> pushable(create.data) |> result.replace(create)
  }
}

fn pushable(data: MessageData) -> Result(Nil, ParamsError) {
  case data.kind == message_type_default || data.kind == message_type_reply {
    False -> Error(UnpushableMessageType(data.kind))
    True ->
      case suppresses_notifications(data) {
        True -> Error(SuppressedNotifications)
        False -> Ok(Nil)
      }
  }
}

pub fn suppresses_notifications(data: MessageData) -> Bool {
  int.bitwise_and(data.flags, flag_suppress_notifications) != 0
}

pub fn route_key(create: MessageCreate) -> Int {
  case create.recipients {
    [first, ..] -> first
    [] -> create.author
  }
}

pub fn with_recipients(
  create: MessageCreate,
  users: List(Int),
) -> MessageCreate {
  MessageCreate(..create, recipients: users)
}

pub fn is_private_channel(data: MessageData) -> Bool {
  case data.channel_type {
    Some(1) | Some(3) -> True
    _ -> False
  }
}

pub fn is_large(large: Option(LargeGuild)) -> Bool {
  case large {
    Some(LargeGuild(member_count:, features:)) ->
      member_count > 2500 || list.contains(features, "LARGE_GUILD_OVERRIDE")
    None -> False
  }
}

pub fn large_guild(member_count: Int, features: List(String)) -> LargeGuild {
  LargeGuild(member_count:, features: list.take(features, max_guild_features))
}

pub fn direct_mention_ids(data: MessageData) -> Set(Int) {
  list.fold(data.mentions, set.new(), fn(acc, mention) {
    case mention.id {
      Some(user) -> set.insert(acc, user)
      None -> acc
    }
  })
}

pub fn set_thread(
  data: MessageData,
  thread: Option(ThreadPush),
) -> MessageData {
  MessageData(..data, thread:)
}

pub fn mark_age_restricted(data: MessageData) -> MessageData {
  MessageData(..data, age_restricted: True)
}

pub fn private_message(
  data: MessageData,
  recipient: UserId,
  author: UserId,
  markdown: PreviewContext,
) -> Result(MessageCreate, ParamsError) {
  use channel <- result.try(option.to_result(data.channel_id, InvalidChannelId))
  use message <- result.try(option.to_result(data.id, InvalidMessageId))
  MessageCreate(
    data:,
    recipients: [id.user_id_to_int(recipient)],
    guild: 0,
    author: id.user_id_to_int(author),
    channel:,
    message:,
    guild_default_level: 0,
    guild_name: None,
    channel_name: None,
    user_roles: dict.new(),
    connected: set.new(),
    markdown:,
    large: None,
    prefilter: NotFiltered,
  )
  |> validate
}

pub fn empty_data() -> MessageData {
  MessageData(
    id: None,
    channel_id: None,
    kind: message_type_default,
    flags: 0,
    author: Author(None, None, None, None),
    content: "",
    mentions: [],
    mention_roles: [],
    mention_everyone: False,
    mention_here: False,
    channel_type: None,
    age_restricted: False,
    thread: None,
    attachments: [],
    embeds: [],
    stickers: [],
    snapshots: [],
    mention_channels: [],
    nicks: dict.new(),
    id_texts: dict.new(),
  )
}

pub fn decode_data(raw: Dynamic) -> Result(MessageData, Nil) {
  decode.run(raw, data_decoder()) |> result.replace_error(Nil)
}

fn data_decoder() -> Decoder(MessageData) {
  use id <- decode.then(snowflake_field("id"))
  use channel_id <- decode.then(snowflake_field("channel_id"))
  use kind <- decode.then(lenient("type", decode.int, message_type_default))
  use flags <- decode.then(lenient("flags", decode.int, 0))
  use author <- decode.then(lenient(
    "author",
    author_decoder(),
    Author(None, None, None, None),
  ))
  use content <- decode.then(lenient("content", decode.string, ""))
  use mentions <- decode.then(maps_field("mentions", mention_decoder()))
  use mention_roles <- decode.then(snowflakes_field("mention_roles"))
  use mention_everyone <- decode.then(lenient(
    "mention_everyone",
    decode.bool,
    False,
  ))
  use mention_here <- decode.then(lenient("mention_here", decode.bool, False))
  use channel_type <- decode.then(lenient(
    "channel_type",
    decode.map(decode.int, Some),
    None,
  ))
  use push_mature <- decode.then(lenient("__push_mature", decode.bool, False))
  use channel_nsfw <- decode.then(lenient("channel_nsfw", decode.bool, False))
  use attachments <- decode.then(entries_field("attachments", media_decoder()))
  use embeds <- decode.then(entries_field("embeds", embed_decoder()))
  use stickers <- decode.then(stickers_field())
  use snapshots <- decode.then(entries_field(
    "message_snapshots",
    snapshot_decoder(),
  ))
  use mention_channels <- decode.then(maps_field(
    "mention_channels",
    mention_channel_decoder(),
  ))
  use nested_nicks <- decode.then(lenient(
    "channel",
    nick_map("nicks"),
    dict.new(),
  ))
  use channel_nicks <- decode.then(nick_map("channel_nicks"))
  use nicks <- decode.then(nick_map("nicks"))
  use guild_text <- decode.then(id_text_field("guild_id"))
  use channel_text <- decode.then(id_text_field("channel_id"))
  use message_text <- decode.then(id_text_field("id"))
  use author_text <- decode.then(lenient(
    "author",
    id_text_field("id"),
    Error(Nil),
  ))
  decode.success(MessageData(
    id:,
    channel_id:,
    kind:,
    flags:,
    author:,
    content:,
    mentions:,
    mention_roles:,
    mention_everyone:,
    mention_here:,
    channel_type:,
    age_restricted: push_mature || channel_nsfw,
    thread: None,
    attachments:,
    embeds:,
    stickers:,
    snapshots:,
    mention_channels:,
    nicks: nested_nicks |> dict.merge(channel_nicks) |> dict.merge(nicks),
    id_texts: [guild_text, channel_text, message_text, author_text]
      |> result.values
      |> dict.from_list,
  ))
}

fn id_text_field(name: String) -> Decoder(Result(#(Int, String), Nil)) {
  lenient(
    name,
    decode.map(decode.string, fn(text) {
      id.parse(text) |> result.map(fn(value) { #(value, text) })
    }),
    Error(Nil),
  )
}

pub fn id_text(data: MessageData, value: Int) -> String {
  dict.get(data.id_texts, value)
  |> result.lazy_unwrap(fn() { id.write(value) })
}

fn author_decoder() -> Decoder(Author) {
  use id <- decode.then(snowflake_field("id"))
  use username <- decode.then(text_field("username"))
  use global_name <- decode.then(text_field("global_name"))
  use avatar <- decode.then(text_field("avatar"))
  decode.success(Author(id:, username:, global_name:, avatar:))
}

fn mention_decoder() -> Decoder(Mention) {
  use id <- decode.then(snowflake_field("id"))
  use username <- decode.then(text_field("username"))
  use global_name <- decode.then(text_field("global_name"))
  decode.success(Mention(id:, username:, global_name:))
}

fn media_decoder() -> Decoder(Media) {
  use proxy <- decode.then(decode.optional_field(
    "proxy_url",
    None,
    decode.map(optional_text(), Some),
    decode.success,
  ))
  use fallback <- decode.then(text_field("url"))
  use content_type <- decode.then(decode.optional_field(
    "content_type",
    Some(""),
    optional_text(),
    decode.success,
  ))
  use flags <- decode.then(decode.optional_field(
    "flags",
    Some(0),
    decode.one_of(decode.map(bitset(), Some), [decode.success(None)]),
    decode.success,
  ))
  use nsfw <- decode.then(lenient("nsfw", decode.bool, False))
  use filename <- decode.then(text_field("filename"))
  let url = option.unwrap(proxy, fallback)
  decode.success(Media(url:, content_type:, flags:, nsfw:, filename:))
}

fn bitset() -> Decoder(Int) {
  decode.one_of(
    decode.int
      |> decode.then(fn(value) {
        case value >= 0 {
          True -> decode.success(value)
          False -> decode.failure(0, "bitset")
        }
      }),
    [
      decode.string
      |> decode.then(fn(text) {
        case id.parse_decimal(text) {
          Ok(value) if value >= 0 -> decode.success(value)
          _ -> decode.failure(0, "bitset")
        }
      }),
    ],
  )
}

fn embed_decoder() -> Decoder(Embed) {
  use image <- decode.then(optional_map("image", media_decoder()))
  use thumbnail <- decode.then(optional_map("thumbnail", media_decoder()))
  use title <- decode.then(text_field("title"))
  use description <- decode.then(text_field("description"))
  use fields <- decode.then(entries_field("fields", embed_field_decoder()))
  decode.success(Embed(image:, thumbnail:, title:, description:, fields:))
}

fn embed_field_decoder() -> Decoder(EmbedField) {
  use name <- decode.then(text_field("name"))
  use value <- decode.then(text_field("value"))
  decode.success(EmbedField(name:, value:))
}

fn stickers_field() -> Decoder(List(Option(String))) {
  lenient("stickers", decode.list(sticker_decoder()), [])
}

fn sticker_decoder() -> Decoder(Option(String)) {
  decode.one_of(
    decode.dict(decode.dynamic, decode.dynamic)
      |> decode.then(fn(_) {
        lenient("name", decode.string, "") |> decode.map(Some)
      }),
    [decode.success(None)],
  )
}

fn snapshot_decoder() -> Decoder(Snapshot) {
  use content <- decode.then(lenient("content", decode.string, ""))
  use stickers <- decode.then(stickers_field())
  use attachments <- decode.then(entries_field("attachments", media_decoder()))
  use embeds <- decode.then(entries_field("embeds", embed_decoder()))
  decode.success(Snapshot(content:, stickers:, attachments:, embeds:))
}

fn mention_channel_decoder() -> Decoder(MentionChannel) {
  use id <- decode.then(snowflake_field("id"))
  use name <- decode.then(text_field("name"))
  use kind <- decode.then(lenient("type", decode.map(decode.int, Some), None))
  decode.success(MentionChannel(id:, name:, kind:))
}

fn nick_map(name: String) -> Decoder(Dict(Int, String)) {
  lenient(name, nick_entries(), dict.new())
}

fn nick_entries() -> Decoder(Dict(Int, String)) {
  decode.dict(decode.string, decode.dynamic)
  |> decode.map(fn(raw) {
    dict.fold(raw, dict.new(), fn(acc, key, value) {
      case id.parse(key), decode.run(value, decode.string) {
        Ok(user), Ok(nick) if nick != "" -> dict.insert(acc, user, nick)
        _, _ -> acc
      }
    })
  })
}

fn optional_map(name: String, inner: Decoder(a)) -> Decoder(Option(a)) {
  lenient(name, decode.map(inner, Some), None)
}

fn entries_field(name: String, inner: Decoder(a)) -> Decoder(List(Option(a))) {
  lenient(
    name,
    decode.list(decode.one_of(decode.map(inner, Some), [decode.success(None)])),
    [],
  )
}

fn maps_field(name: String, inner: Decoder(a)) -> Decoder(List(a)) {
  lenient(
    name,
    decode.list(decode.one_of(decode.map(inner, Some), [decode.success(None)]))
      |> decode.map(option.values),
    [],
  )
}

fn snowflakes_field(name: String) -> Decoder(List(Int)) {
  lenient(
    name,
    decode.list(
      decode.one_of(decode.map(edge.snowflake(), Some), [decode.success(None)]),
    )
      |> decode.map(option.values),
    [],
  )
}

fn snowflake_field(name: String) -> Decoder(Option(Int)) {
  lenient(name, decode.map(edge.snowflake(), Some), None)
}

fn text_field(name: String) -> Decoder(Option(String)) {
  decode.optional_field(name, None, optional_text(), decode.success)
}

fn optional_text() -> Decoder(Option(String)) {
  decode.one_of(decode.map(decode.string, Some), [decode.success(None)])
}

fn lenient(name: String, inner: Decoder(a), default: a) -> Decoder(a) {
  decode.optional_field(
    name,
    default,
    decode.one_of(inner, [decode.success(default)]),
    decode.success,
  )
}

pub fn nonempty(value: Option(String)) -> Option(String) {
  option.then(value, fn(text) {
    case string.is_empty(text) {
      True -> None
      False -> Some(text)
    }
  })
}
