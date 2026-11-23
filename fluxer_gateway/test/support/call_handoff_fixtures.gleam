// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/id
import fluxer_gateway/core/voice_key
import fluxer_gateway/handoff/codec
import fluxer_gateway/voice/call/handoff/codec as call_codec
import fluxer_gateway/voice/call/state.{Call, Caller, Member, PendingJoin, Slot}
import fluxer_gateway/voice/entry
import fluxer_gateway/voice/geo
import fluxer_gateway/voice/store
import gleam/dict
import gleam/erlang/process.{type Subject}
import gleam/option.{None, Some}
import gleam/set

pub const directory = "test/fixtures/handoff/call"

pub const fixture_epoch = 7

pub fn path(part: String, version: Int) -> String {
  directory <> "/" <> part <> "_v" <> int_text(version) <> ".bin"
}

@external(erlang, "erlang", "integer_to_binary")
fn int_text(value: Int) -> String

@external(erlang, "erlang", "binary_to_atom")
fn name_atom(name: String) -> process.Name(a)

fn inbox() -> Subject(a) {
  process.named_subject(name_atom("undefined"))
}

fn user(value: Int) -> id.UserId {
  let assert Ok(found) = id.user_id(value)
  found
}

fn conn(text: String) -> voice_key.ConnectionId {
  let assert Ok(found) = voice_key.connection_id(text)
  found
}

pub fn expected_tail() -> call_codec.Tail {
  let assert Ok(channel) = id.channel_id(5401)
  let assert Ok(message) = id.message_id(6401)
  let value =
    entry.new(
      user: user(1),
      guild: None,
      channel:,
      connection_id: conn("c1"),
      session_id: Some("s1"),
      flags: entry.Flags(..entry.no_flags(), self_mute: True),
      mute: False,
      deaf: False,
    )
    |> entry.set_routing(Some("us-east"), Some("sfu-1"))
  let assert Ok(found) = entry.key(value)
  let call =
    Call(
      channel:,
      message:,
      region: Some("us-east"),
      ringing: [user(3)],
      pending_ringing: [user(2)],
      recipients: [user(1), user(2), user(3)],
      caller: Some(Caller(id: user(1), name: "one", avatar: Some("a1"))),
      voice_states: store.from_list([#(found, Slot(entry: value, seq: 4))]),
      next_seq: 5,
      sessions: dict.from_list([
        #(
          "s1",
          Member(
            user: user(1),
            inbox: inbox(),
            monitor: None,
            owner: None,
            owner_monitor: None,
          ),
        ),
        #(
          "s2",
          Member(
            user: user(2),
            inbox: inbox(),
            monitor: None,
            owner: None,
            owner_monitor: None,
          ),
        ),
      ]),
      detached: dict.from_list([#("s2", 77)]),
      pending: dict.from_list([
        #(
          conn("c1"),
          PendingJoin(user: user(1), session_id: "s1", joined_at: 55),
        ),
      ]),
      initiator_ready: True,
      ring_timers: dict.new(),
      idle_timer: None,
      created_at: 1000,
      history: set.from_list([user(1), user(2)]),
      last_event: None,
      room_key: Some("room-key"),
    )
  let call =
    Call(
      ..call,
      last_event: Some(state.event_key(call, state.event_states(call))),
    )
  call_codec.Tail(
    call:,
    located: dict.from_list([#(conn("c1"), geo.new(Some("59"), Some("18")))]),
  )
}

pub fn tail_bytes(version: Int) -> BitArray {
  let assert Ok(bytes) =
    codec.encode(call_codec.tail(), expected_tail(), version, fixture_epoch)
  bytes
}
