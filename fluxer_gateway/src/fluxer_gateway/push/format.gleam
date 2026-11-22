// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/config/settings as config
import fluxer_gateway/core/id
import fluxer_gateway/push/markdown.{type PreviewContext, PreviewContext}
import fluxer_gateway/push/message.{
  type Author, type Embed, type Media, type Mention, type MessageData,
  type Snapshot,
}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/json.{type Json}
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/set.{type Set}
import gleam/string

const max_mentions_for_push = 50

const max_preview_bytes = 100

pub type Preview {
  Preview(body: String, fields: List(#(String, Json)))
}

pub fn truncate_bytes(text: String, max_bytes: Int) -> String {
  let bits = bit_array.from_string(text)
  case bit_array.byte_size(bits) > max_bytes {
    False -> text
    True ->
      case bit_array.slice(bits, 0, max_bytes) {
        Ok(prefix) -> valid_prefix(prefix, 3)
        Error(Nil) -> text
      }
  }
}

fn valid_prefix(bits: BitArray, tries: Int) -> String {
  case bit_array.to_string(bits) {
    Ok(text) -> text
    Error(Nil) ->
      case tries > 0, bit_array.byte_size(bits) {
        True, size if size > 0 ->
          case bit_array.slice(bits, 0, size - 1) {
            Ok(shorter) -> valid_prefix(shorter, tries - 1)
            Error(Nil) -> ""
          }
        _, _ -> ""
      }
  }
}

fn truncate_preview(text: String) -> String {
  truncate_bytes(text, max_preview_bytes)
}

pub fn preview(data: MessageData, context: PreviewContext) -> Preview {
  case markdown.preview(data.content, context) {
    "" ->
      Preview(
        body: truncate_preview(fallback_preview(data, context)),
        fields: fallback_fields(data),
      )
    rendered -> Preview(body: truncate_preview(rendered), fields: [])
  }
}

fn fallback_fields(data: MessageData) -> List(#(String, Json)) {
  case data.stickers, data.attachments {
    [Some(sticker)], _ -> with_arg("sticker", sticker)
    [_, _, ..], _ ->
      with_arg("stickers", string.join(sticker_names(data.stickers), "\n"))
    _, [Some(attachment), ..] ->
      with_arg("attachment", option.unwrap(attachment.filename, ""))
    _, _ -> []
  }
}

fn with_arg(kind: String, arg: String) -> List(#(String, Json)) {
  case arg {
    "" -> [#("preview_kind", json.string(kind))]
    _ -> [
      #("preview_kind", json.string(kind)),
      #("preview_arg", json.string(truncate_preview(arg))),
    ]
  }
}

fn sticker_names(stickers: List(Option(String))) -> List(String) {
  stickers
  |> option.values
  |> list.filter(fn(name) { name != "" })
}

fn fallback_preview(data: MessageData, context: PreviewContext) -> String {
  first_nonempty([
    sticker_preview(data.stickers),
    attachment_preview(data.attachments),
    embed_preview(data.embeds),
    snapshot_preview(data.snapshots, context),
  ])
}

fn snapshot_preview(
  snapshots: List(Option(Snapshot)),
  context: PreviewContext,
) -> String {
  case snapshots {
    [Some(snapshot), ..] ->
      first_nonempty([
        markdown.preview(snapshot.content, context),
        sticker_preview(snapshot.stickers),
        attachment_preview(snapshot.attachments),
        embed_preview(snapshot.embeds),
      ])
    _ -> ""
  }
}

fn sticker_preview(stickers: List(Option(String))) -> String {
  case stickers {
    [Some("")] -> "Sticker"
    [Some(name)] -> "Sticker: " <> name
    [_, _, ..] ->
      case sticker_names(stickers) {
        [] -> "Stickers"
        names -> "Stickers: " <> format_name_list(names)
      }
    [None] | [] -> ""
  }
}

fn attachment_preview(attachments: List(Option(Media))) -> String {
  case attachments {
    [Some(attachment), ..] ->
      case message.nonempty(attachment.filename) {
        Some(filename) -> "Attachment: " <> filename
        None -> "Attachment"
      }
    _ -> ""
  }
}

fn embed_preview(embeds: List(Option(Embed))) -> String {
  case embeds {
    [Some(embed), ..] ->
      case message.nonempty(embed.title), message.nonempty(embed.description) {
        Some(title), Some(description) -> title <> ": " <> description
        _, Some(description) -> description
        Some(title), None -> title
        None, None ->
          case embed.fields {
            [Some(field), ..] ->
              case message.nonempty(field.name), message.nonempty(field.value) {
                Some(name), Some(value) -> name <> ": " <> value
                _, _ -> ""
              }
            _ -> ""
          }
      }
    _ -> ""
  }
}

fn format_name_list(names: List(String)) -> String {
  case list.reverse(names) {
    [] -> ""
    [only] -> only
    [last, first] -> first <> " and " <> last
    [last, ..rest] -> string.join(list.reverse(rest), ", ") <> ", and " <> last
  }
}

fn first_nonempty(values: List(String)) -> String {
  list.find(values, fn(value) { value != "" })
  |> option.from_result
  |> option.unwrap("")
}

pub fn markdown_context(
  data: MessageData,
  user_nicknames: Dict(Int, String),
  role_names: Dict(Int, String),
  guild_channel_names: Dict(Int, String),
) -> PreviewContext {
  let mentioned_roles = role_mention_ids(rendered_contents(data))
  PreviewContext(
    preserve_markdown: True,
    hide_spoilers: True,
    include_emoji_names: True,
    include_link_urls: False,
    users: mention_user_names(data.mentions, user_nicknames),
    user_nicknames:,
    roles: dict.fold(role_names, dict.new(), fn(acc, role, name) {
      case name != "" && set.contains(mentioned_roles, role) {
        True -> dict.insert(acc, role, name)
        False -> acc
      }
    }),
    channels: dict.merge(guild_channel_names, mention_channel_names(data)),
  )
}

pub fn private_markdown_context(data: MessageData) -> PreviewContext {
  markdown_context(data, data.nicks, dict.new(), dict.new())
}

fn rendered_contents(data: MessageData) -> List(String) {
  case data.snapshots {
    [Some(snapshot), ..] -> [data.content, snapshot.content]
    _ -> [data.content]
  }
}

fn mention_user_names(
  mentions: List(Mention),
  nicknames: Dict(Int, String),
) -> Dict(Int, String) {
  mentions
  |> list.take(max_mentions_for_push)
  |> list.fold(dict.new(), fn(acc, mention) {
    case mention.id {
      Some(user) -> {
        let name =
          first_present([
            dict.get(nicknames, user) |> option.from_result,
            mention.global_name,
            mention.username,
          ])
        case name {
          Some(name) -> dict.insert(acc, user, name)
          None -> acc
        }
      }
      None -> acc
    }
  })
}

fn mention_channel_names(data: MessageData) -> Dict(Int, String) {
  list.fold(data.mention_channels, dict.new(), fn(acc, channel) {
    let copyable = case channel.kind {
      None -> True
      Some(kind) -> is_copyable_channel_type(kind)
    }
    case copyable, channel.id, message.nonempty(channel.name) {
      True, Some(id), Some(name) -> dict.insert(acc, id, name)
      _, _, _ -> acc
    }
  })
}

pub fn is_copyable_channel_type(kind: Int) -> Bool {
  case kind {
    0 | 2 | 4 | 5 | 998 -> True
    _ -> False
  }
}

pub fn channel_mention_ids(content: String) -> List(Int) {
  mention_scan(content, "<#", set.new())
  |> set.to_list
  |> list.sort(int.compare)
}

fn role_mention_ids(contents: List(String)) -> Set(Int) {
  list.fold(contents, set.new(), fn(acc, content) {
    mention_scan(content, "<@&", acc)
  })
}

fn mention_scan(content: String, opener: String, acc: Set(Int)) -> Set(Int) {
  case string.split_once(content, opener) {
    Error(Nil) -> acc
    Ok(#(_, after)) ->
      case string.split_once(after, ">") {
        Error(Nil) -> acc
        Ok(#(id_text, rest)) ->
          case id.parse(id_text) {
            Ok(found) -> mention_scan(rest, opener, set.insert(acc, found))
            Error(Nil) -> mention_scan(after, opener, acc)
          }
      }
  }
}

fn first_present(values: List(Option(String))) -> Option(String) {
  list.find_map(values, fn(value) {
    message.nonempty(value) |> option.to_result(Nil)
  })
  |> option.from_result
}

pub fn author_name(data: MessageData, context: PreviewContext) -> String {
  let author = data.author
  let fallback = option.unwrap(author.username, "Unknown")
  let nicknames = case dict.is_empty(context.user_nicknames) {
    False -> context.user_nicknames
    True -> data.nicks
  }
  let nick =
    option.then(author.id, fn(user) {
      dict.get(nicknames, user) |> option.from_result
    })
  first_present([nick, author.global_name, author.username, Some(fallback)])
  |> option.unwrap(fallback)
}

pub fn author_avatar_url(
  author: Author,
  user_text: fn(Int) -> String,
) -> String {
  case author.avatar, author.id {
    None, id -> default_avatar_url(id)
    Some(hash), Some(user) -> avatar_url(user_text(user), hash)
    Some(_), None -> default_avatar_url(None)
  }
}

pub fn avatar_url(user: String, hash: String) -> String {
  media_proxy_endpoint() <> "/avatars/" <> user <> "/" <> hash <> ".png"
}

pub fn default_avatar_url(user: Option(Int)) -> String {
  let index = case user {
    Some(value) -> value % 6
    None -> 0
  }
  let directory = case config.get().self_hosted {
    True -> "avatars/neutral/"
    False -> "avatars/"
  }
  static_asset_url(directory <> int.to_string(index) <> ".png")
}

pub fn static_asset_url(path: String) -> String {
  static_cdn_endpoint() <> "/" <> path
}

fn media_proxy_endpoint() -> String {
  strip_trailing_slashes(config.get().media_proxy_endpoint)
}

fn static_cdn_endpoint() -> String {
  strip_trailing_slashes(config.get().static_cdn_endpoint)
}

fn strip_trailing_slashes(text: String) -> String {
  case string.ends_with(text, "/") {
    True -> strip_trailing_slashes(string.drop_end(text, 1))
    False -> text
  }
}

pub fn build_url(guild: Int, channel: Int, message: Int) -> String {
  channel_url(guild, id.write(guild), id.write(channel), id.write(message))
}

pub fn channel_url(
  guild: Int,
  guild_text: String,
  channel_text: String,
  message_text: String,
) -> String {
  let first = case guild {
    0 -> "@me"
    _ -> guild_text
  }
  "/channels/" <> first <> "/" <> channel_text <> "/" <> message_text
}

pub fn channel_label(
  data: MessageData,
  channel_name: Option(String),
) -> Option(String) {
  case channel_name, thread_parent_name(data) {
    Some(name), Some(parent) -> Some(name <> ", #" <> parent)
    other, _ -> other
  }
}

fn thread_parent_name(data: MessageData) -> Option(String) {
  option.then(data.thread, fn(thread) { message.nonempty(thread.parent_name) })
}

pub fn title(
  author_name: String,
  data: MessageData,
  guild: Int,
  guild_name: Option(String),
  channel_name: Option(String),
) -> String {
  case guild {
    0 ->
      case option.unwrap(data.channel_type, 1) {
        3 -> author_name <> " (Group DM)"
        _ -> author_name
      }
    _ ->
      case guild_name, channel_label(data, channel_name) {
        Some(guild_name), Some(label) ->
          author_name <> " (#" <> label <> ", " <> guild_name <> ")"
        _, _ -> author_name
      }
  }
}
