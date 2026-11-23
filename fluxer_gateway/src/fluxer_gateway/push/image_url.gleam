// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/message.{type Embed, type Media, type MessageData}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string

const max_image_url_bytes = 1024

const sensitive_flags = 0x18

type OwnLink {
  OwnLink(channel: String, message: String)
  NoOwnLink
}

pub fn extract(data: MessageData) -> Option(String) {
  let own = own_link(data)
  first_safe(option.values(data.attachments), own)
  |> option.lazy_or(fn() { first_embed_url(option.values(data.embeds), own) })
  |> option.then(fn(text) {
    case string.byte_size(text) <= max_image_url_bytes {
      True -> Some(text)
      False -> None
    }
  })
}

fn own_link(data: MessageData) -> OwnLink {
  case data.channel_id, data.id {
    Some(channel_id), Some(message_id) ->
      OwnLink(
        channel: message.id_text(data, channel_id),
        message: message.id_text(data, message_id),
      )
    _, _ -> NoOwnLink
  }
}

fn first_safe(media: List(Media), own: OwnLink) -> Option(String) {
  list.find_map(media, safe_url(_, own)) |> option.from_result
}

fn first_embed_url(embeds: List(Embed), own: OwnLink) -> Option(String) {
  list.find_map(embeds, fn(embed) {
    [embed.image, embed.thumbnail]
    |> option.values
    |> list.find_map(safe_url(_, own))
  })
  |> option.from_result
}

fn safe_url(media: Media, own: OwnLink) -> Result(String, Nil) {
  case media.url, media.content_type, media.flags {
    Some(url), Some(content_type), Some(flags) -> {
      let image = string.contains(string.lowercase(content_type), "image/")
      let sensitive = media.nsfw || int.bitwise_and(flags, sensitive_flags) != 0
      let https = string.starts_with(string.lowercase(url), "https://")
      case image && !sensitive && https && !is_own_api_link(url, own) {
        True -> Ok(url)
        False -> Error(Nil)
      }
    }
    _, _, _ -> Error(Nil)
  }
}

fn is_own_api_link(url: String, own: OwnLink) -> Bool {
  case own {
    NoOwnLink -> False
    OwnLink(channel:, message:) ->
      case string.split_once(before(string.lowercase(url), "#"), "?") {
        Ok(#(location, query)) ->
          case strip_scheme(location) {
            Ok(rest) ->
              has_attachment_path(rest, channel)
              && list.contains(string.split(query, "&"), "mid=" <> message)
            Error(Nil) -> False
          }
        Error(Nil) -> False
      }
  }
}

fn before(text: String, separator: String) -> String {
  case string.split_once(text, separator) {
    Ok(#(head, _)) -> head
    Error(Nil) -> text
  }
}

fn strip_scheme(url: String) -> Result(String, Nil) {
  case url {
    "https://" <> rest | "http://" <> rest -> Ok(rest)
    _ -> Error(Nil)
  }
}

fn has_attachment_path(location: String, channel: String) -> Bool {
  case string.split_once(location, "/") {
    Ok(#(host, path)) if host != "" ->
      attachment_after(path, "channels/" <> channel <> "/attachments/")
    _ -> False
  }
}

fn attachment_after(path: String, marker: String) -> Bool {
  case string.starts_with(path, marker) {
    True ->
      attachment_tail(string.drop_start(path, string.length(marker)))
      || next_segment(path, marker)
    False -> next_segment(path, marker)
  }
}

fn next_segment(path: String, marker: String) -> Bool {
  case string.split_once(path, "/") {
    Ok(#(_, rest)) -> attachment_after(rest, marker)
    Error(Nil) -> False
  }
}

fn attachment_tail(tail: String) -> Bool {
  case string.split_once(tail, "/") {
    Ok(#(digits, file)) ->
      digits != "" && file != "" && all_digits(bit_array.from_string(digits))
    Error(Nil) -> False
  }
}

fn all_digits(text: BitArray) -> Bool {
  case text {
    <<>> -> True
    <<char, rest:bytes>> if char >= 0x30 && char <= 0x39 -> all_digits(rest)
    _ -> False
  }
}
