// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob
import fluxer_gateway/handoff/codec
import fluxer_gateway/handoff/service
import fluxer_gateway/push/handoff/codec as push_codec
import fluxer_gateway/push/handoff/wire_v1
import fluxer_gateway/push/outbox/state
import gleam/bit_array
import gleam/dynamic.{type Dynamic}
import gleam/int
import gleam/result
import support/push_handoff_fixtures as fixtures

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(BitArray, Dynamic)

@external(erlang, "filelib", "is_file")
fn is_file(path: String) -> Bool

pub fn every_readable_push_version_has_a_fixture_test() {
  let range = push_codec.range()
  int.range(range.oldest_read, range.max_read + 1, Nil, fn(_, version) {
    assert is_file(fixtures.path("tail", version))
  })
}

pub fn push_tail_fixtures_decode_to_the_frozen_state_test() {
  service.load_wire_atoms()
  let range = push_codec.range()
  int.range(range.oldest_read, range.max_read + 1, Nil, fn(_, version) {
    let assert Ok(bytes) = read_file(fixtures.path("tail", version))
    let assert Ok(#(header, tail)) = codec.decode(push_codec.tail(), bytes)
    assert header.codec_version == version
    assert tail == fixtures.expected_tail()
    assert bytes == fixtures.tail_bytes(version)
  })
}

pub fn push_tail_round_trips_jobs_reads_and_throttle_test() {
  let bytes = fixtures.tail_bytes(wire_v1.version)
  let assert Ok(#(header, decoded)) = codec.decode(push_codec.tail(), bytes)
  assert header.part == blob.Tail
  assert decoded == fixtures.expected_tail()
}

pub fn push_targets_round_trip_test() {
  let target = state.Target(node: "push@host", outbox: "outbox_name")
  assert push_codec.decode_target(push_codec.encode_target(target))
    == Ok(target)
  assert push_codec.decode_target(<<1, 2, 3>>) == Error(Nil)
}

pub fn corrupt_push_bodies_are_errors_not_crashes_test() {
  let bytes = fixtures.tail_bytes(wire_v1.version)
  let size = bit_array.byte_size(bytes)
  let assert Ok(head) = bit_array.slice(bytes, 0, size - 4)
  assert codec.decode(push_codec.tail(), head) |> result.is_error
  assert codec.encode(push_codec.tail(), fixtures.expected_tail(), 2, 1)
    == Error(codec.UnwritableVersion(2))
}
