// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/list
import gleam/result
import gleam/string

const max_content_bytes = 16_384

const max_context_name_entries = 512

pub type PreviewContext {
  PreviewContext(
    preserve_markdown: Bool,
    hide_spoilers: Bool,
    include_emoji_names: Bool,
    include_link_urls: Bool,
    users: Dict(Int, String),
    user_nicknames: Dict(Int, String),
    roles: Dict(Int, String),
    channels: Dict(Int, String),
  )
}

type Names =
  List(#(Int, String))

@external(erlang, "fluxer_gateway@nif", "md_render_push_preview")
fn render_nif(
  content: String,
  context: #(Bool, Bool, Bool, Bool, Names, Names, Names),
) -> Result(String, Dynamic)

pub fn preview(content: String, context: PreviewContext) -> String {
  case string.byte_size(content) <= max_content_bytes {
    False -> strip_to_safe(content)
    True ->
      render_nif(content, native(context))
      |> result.lazy_unwrap(fn() { strip_to_safe(content) })
  }
}

fn native(
  context: PreviewContext,
) -> #(Bool, Bool, Bool, Bool, Names, Names, Names) {
  #(
    context.preserve_markdown,
    context.hide_spoilers,
    context.include_emoji_names,
    context.include_link_urls,
    bounded_names(context.users),
    bounded_names(context.roles),
    bounded_names(context.channels),
  )
}

fn bounded_names(names: Dict(Int, String)) -> Names {
  case dict.size(names) <= max_context_name_entries {
    True -> dict.to_list(names)
    False -> []
  }
}

fn strip_to_safe(content: String) -> String {
  content
  |> string.to_utf_codepoints
  |> list.filter(fn(codepoint) {
    is_safe_codepoint(string.utf_codepoint_to_int(codepoint))
  })
  |> string.from_utf_codepoints
}

fn is_safe_codepoint(code: Int) -> Bool {
  case code {
    10 | 9 -> True
    _ if code >= 0 && code <= 0x1F -> False
    _ if code >= 0x7F && code <= 0x9F -> False
    _ if code >= 0x200E && code <= 0x200F -> False
    _ if code >= 0x202A && code <= 0x202E -> False
    _ if code >= 0x2066 && code <= 0x2069 -> False
    _ -> True
  }
}
