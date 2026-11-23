// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/push/format
import fluxer_gateway/push/message
import gleam/string

fn preview(content: String) -> String {
  let data = message.MessageData(..message.empty_data(), content:)
  format.preview(data, format.private_markdown_context(data)).body
}

pub fn inline_spoiler_is_hidden_test() {
  assert preview("a ||secret|| b") == "a █████ b"
}

pub fn block_spoiler_is_hidden_test() {
  assert !string.contains(preview("||\nsecret\n||"), "secret")
}
