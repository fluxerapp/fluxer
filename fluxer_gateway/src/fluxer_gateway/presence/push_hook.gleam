// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id.{type UserId}
import fluxer_gateway/core/raw_json.{type RawJson}
import fluxer_gateway/presence/counters
import fluxer_gateway/presence/push_buffer.{type Pending}
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/outbox as push_outbox
import fluxer_gateway/push/read_state
import fluxer_gateway/push/server as push_server
import gleam/dynamic/decode
import gleam/list
import gleam/result

fn create(
  data: RawJson,
  recipient: UserId,
  author: Int,
) -> Result(MessageCreate, Nil) {
  case id.user_id(author), raw_json.read(data, decode.dynamic) {
    Ok(author), Ok(decoded) ->
      push_server.private_message(decoded, recipient, author)
    _, _ -> Error(Nil)
  }
}

pub fn direct_message(data: RawJson, recipient: UserId, author: Int) -> Nil {
  case create(data, recipient, author) {
    Ok(found) -> push_server.handle_message_create(found)
    Error(Nil) -> Nil
  }
}

pub fn send_now(recipient: UserId, pending: Pending) -> Nil {
  direct_message(pending.data, recipient, pending.author)
}

pub fn flush(recipient: UserId, pending: List(Pending)) -> Nil {
  pending
  |> list.filter_map(fn(entry) {
    create(entry.data, recipient, entry.author)
    |> result.map(read_state.Buffered(
      create: _,
      buffered_at_ms: entry.buffered_at,
    ))
  })
  |> push_server.handle_buffered_message_creates
}

pub fn overflow(dropped: Int) -> Nil {
  counters.add(counters.PushBufferOverflow, dropped)
}

pub fn note_session_active(user: UserId) -> Nil {
  push_outbox.note_session_active(user)
}

pub fn invalidate_blocked_ids(user: UserId) -> Nil {
  push_server.invalidate_blocked_ids(user)
}
