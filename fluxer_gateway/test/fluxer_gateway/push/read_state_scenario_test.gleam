// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/clock
import fluxer_gateway/push/format
import fluxer_gateway/push/message.{type MessageCreate}
import fluxer_gateway/push/read_state
import fluxer_gateway/push/throttle
import gleam/dict
import gleam/dynamic/decode
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Subject}
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{None, Some}
import gleam/set
import support/push_api_stub as stub
import support/push_permit_probe as probe
import support/push_throttle

type AtomicsRef

@external(erlang, "persistent_term", "get")
fn term_get(key: Atom) -> List(AtomicsRef)

@external(erlang, "atomics", "get")
fn atomics_get(ref: AtomicsRef, index: Int) -> Int

fn slots_in_use() -> Int {
  let assert [ref, ..] =
    term_get(atom.create("fluxer_gateway_push_read_state_slots"))
  atomics_get(ref, 1)
}

fn create(user: Int, channel: Int, message_id: Int) -> MessageCreate {
  let data =
    message.MessageData(
      ..message.empty_data(),
      id: Some(message_id),
      channel_id: Some(channel),
      channel_type: Some(1),
      content: "hello",
    )
  message.MessageCreate(
    data:,
    recipients: [user],
    guild: 0,
    author: 1,
    channel:,
    message: message_id,
    guild_default_level: 0,
    guild_name: None,
    channel_name: None,
    user_roles: dict.new(),
    connected: set.new(),
    markdown: format.private_markdown_context(data),
    large: None,
    prefilter: message.NotFiltered,
  )
}

fn stale(create: MessageCreate) -> read_state.Buffered {
  read_state.Buffered(create:, buffered_at_ms: clock.system_ms() - 301_000)
}

fn fresh(create: MessageCreate) -> read_state.Buffered {
  read_state.Buffered(create:, buffered_at_ms: clock.system_ms())
}

fn read_marks(marks: List(#(Int, Int))) -> fn(stub.Call) -> stub.Reply {
  fn(call: stub.Call) {
    let user =
      decode.run(call.body, decode.at(["user_id"], decode.string))
      |> fn(found) {
        case found {
          Ok(text) -> int.parse(text)
          Error(_) -> Error(Nil)
        }
      }
    let mark = case user {
      Ok(user) -> list.key_find(marks, user)
      Error(Nil) -> Error(Nil)
    }
    case mark {
      Ok(mark) ->
        stub.data(
          json.object([#("last_message_id", json.string(int.to_string(mark)))]),
        )
      Error(Nil) -> stub.data(json.object([]))
    }
  }
}

fn published(into: Subject(Int)) -> fn(MessageCreate) -> Nil {
  fn(create: MessageCreate) { process.send(into, create.message) }
}

fn collect(from: Subject(Int), acc: List(Int)) -> List(Int) {
  case process.receive(from, 300) {
    Ok(found) -> collect(from, [found, ..acc])
    Error(Nil) -> list.sort(acc, int.compare)
  }
}

pub fn stale_messages_already_read_are_suppressed_test() {
  push_throttle.calm(throttle.ReadState)
  let api = stub.start(read_marks([#(9_100_001, 500), #(9_100_002, 10)]))
  let out = process.new_subject()
  read_state.handle_buffered(
    [
      stale(create(9_100_001, 77_001, 400)),
      stale(create(9_100_001, 77_001, 600)),
      stale(create(9_100_002, 77_002, 20)),
      stale(create(9_100_003, 77_003, 30)),
      fresh(create(9_100_001, 77_001, 100)),
    ],
    published(out),
  )
  assert collect(out, []) == [20, 30, 100, 600]
  let assert True = probe.eventually(fn() { slots_in_use() == 0 }, 100)
  stub.stop(api)
}

pub fn a_crashing_publish_does_not_leak_a_fetch_slot_test() {
  push_throttle.calm(throttle.ReadState)
  let api = stub.start(read_marks([]))
  let before = slots_in_use()
  read_state.handle_buffered([stale(create(9_100_011, 77_011, 40))], fn(_) {
    panic as "publish failed"
  })
  process.sleep(300)
  assert slots_in_use() == before
  stub.stop(api)
}
