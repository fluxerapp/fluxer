// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/presence/push_buffer.{DmMessage, Limits}
import gleam/option.{Some}
import gleeunit/should

fn message(author: Int, channel: Int, id: Int) -> push_buffer.DmMessage {
  DmMessage(
    author:,
    channel: Some(channel),
    message: Some(id),
    suppressed: False,
    data: raw_json.wrap(<<"{}">>),
  )
}

pub fn eligible_sends_now_test() {
  let #(_, action) =
    push_buffer.on_message(
      push_buffer.new(),
      message(2, 10, 100),
      1,
      True,
      5,
      push_buffer.default_limits(),
    )
  case action {
    push_buffer.SendNow([], pending) -> pending.author |> should.equal(2)
    _ -> should.fail()
  }
}

pub fn ineligible_buffers_and_ack_drops_test() {
  let #(buffer, action) =
    push_buffer.on_message(
      push_buffer.new(),
      message(2, 10, 100),
      1,
      False,
      5,
      push_buffer.default_limits(),
    )
  action |> should.equal(push_buffer.Buffered(dropped: 0))
  push_buffer.size(buffer) |> should.equal(1)
  let acked = push_buffer.on_ack(buffer, 10, 100)
  push_buffer.is_empty(acked) |> should.be_true
  let #(_, again) =
    push_buffer.on_message(
      acked,
      message(2, 10, 90),
      1,
      False,
      6,
      push_buffer.default_limits(),
    )
  again |> should.equal(push_buffer.Ignored)
}

pub fn own_message_acks_test() {
  let #(buffer, _) =
    push_buffer.on_message(
      push_buffer.new(),
      message(2, 10, 100),
      1,
      False,
      5,
      push_buffer.default_limits(),
    )
  let #(buffer, action) =
    push_buffer.on_message(
      buffer,
      message(1, 10, 101),
      1,
      False,
      6,
      push_buffer.default_limits(),
    )
  action |> should.equal(push_buffer.Ignored)
  push_buffer.is_empty(buffer) |> should.be_true
}

pub fn entry_cap_test() {
  let limits = Limits(max_entries: 1, max_bytes: 0)
  let #(buffer, _) =
    push_buffer.on_message(
      push_buffer.new(),
      message(2, 10, 1),
      1,
      False,
      1,
      limits,
    )
  let #(buffer, action) =
    push_buffer.on_message(buffer, message(2, 11, 2), 1, False, 2, limits)
  action |> should.equal(push_buffer.Buffered(dropped: 1))
  push_buffer.size(buffer) |> should.equal(1)
}

pub fn eligibility_flush_test() {
  let #(buffer, _) =
    push_buffer.on_message(
      push_buffer.new(),
      message(2, 10, 1),
      1,
      False,
      1,
      push_buffer.default_limits(),
    )
  let #(buffer, pending, announce) = push_buffer.set_eligible(buffer, False)
  pending |> should.equal([])
  announce |> should.be_true
  let #(buffer, pending, announce) = push_buffer.set_eligible(buffer, True)
  announce |> should.be_false
  case pending {
    [only] -> only.buffered_at |> should.equal(1)
    _ -> should.fail()
  }
  push_buffer.is_empty(buffer) |> should.be_true
}
