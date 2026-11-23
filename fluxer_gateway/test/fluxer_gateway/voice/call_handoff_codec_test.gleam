// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import fluxer_gateway/handoff/codec
import fluxer_gateway/handoff/service
import fluxer_gateway/voice/call/handoff/codec as call_codec
import fluxer_gateway/voice/call/handoff/wire_v1
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/erlang/process
import gleam/int
import gleam/list
import gleam/result
import support/call_handoff_fixtures as fixtures

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "filelib", "is_file")
fn is_file(path: String) -> Bool

pub fn every_readable_call_version_has_a_fixture_test() {
  let range = call_codec.range()
  int.range(range.oldest_read, range.max_read + 1, Nil, fn(_, version) {
    assert is_file(fixtures.path("tail", version))
  })
}

pub fn call_tail_fixtures_decode_to_the_frozen_state_test() {
  service.load_wire_atoms()
  let range = call_codec.range()
  int.range(range.oldest_read, range.max_read + 1, Nil, fn(_, version) {
    let assert Ok(bytes) = read_file(fixtures.path("tail", version))
    let assert Ok(#(header, tail)) = codec.decode(call_codec.tail(), bytes)
    assert header.codec_version == version
    assert header.epoch == fixtures.fixture_epoch
    assert tail == fixtures.expected_tail()
  })
}

pub fn call_tail_round_trips_every_field_test() {
  let tail = fixtures.expected_tail()
  let bytes = fixtures.tail_bytes(wire_v1.version)
  let assert Ok(#(header, decoded)) = codec.decode(call_codec.tail(), bytes)
  assert header.part == blob.Tail
  assert decoded == tail
  assert decoded.call.voice_states == tail.call.voice_states
  assert decoded.call.pending == tail.call.pending
  assert decoded.call.room_key == tail.call.room_key
  assert decoded.located == tail.located
}

pub fn call_codec_refuses_versions_it_cannot_write_or_read_test() {
  assert codec.encode(call_codec.tail(), fixtures.expected_tail(), 2, 1)
    == Error(codec.UnwritableVersion(2))
  let assert Ok(bytes) =
    blob.wrap(
      ns: call_codec.range().ns,
      codec_version: 9,
      part: blob.Tail,
      flags: 0,
      epoch: 1,
      body: <<>>,
    )
  assert codec.decode(call_codec.tail(), bytes)
    == Error(codec.UnreadableVersion(9))
}

pub fn corrupt_call_bodies_are_errors_not_crashes_test() {
  let bytes = fixtures.tail_bytes(wire_v1.version)
  let size = bit_array.byte_size(bytes)
  let assert Ok(head) = bit_array.slice(bytes, 0, size - 4)
  assert codec.decode(call_codec.tail(), head) |> result.is_error
  let assert Ok(wrapped) =
    blob.wrap(
      ns: call_codec.range().ns,
      codec_version: wire_v1.version,
      part: blob.Tail,
      flags: 0,
      epoch: 1,
      body: <<131, 100, 0, 3, "bad":utf8>>,
    )
  assert codec.decode(call_codec.tail(), wrapped) |> result.is_error
}

pub fn forwarded_call_messages_and_targets_round_trip_test() {
  let subject: process.Subject(Int) = process.new_subject()
  assert call_codec.decode_target(call_codec.encode_target(subject))
    == Ok(subject)
  let message = #("held", [1, 2, 3], subject)
  let assert Ok(bytes) =
    codec.encode(call_codec.inbox(), message, wire_v1.version, 3)
  let assert Ok(#(header, decoded)) = codec.decode(call_codec.inbox(), bytes)
  assert header.part == blob.Inbox
  assert decoded == message
  assert list.length([decoded]) == 1
}
