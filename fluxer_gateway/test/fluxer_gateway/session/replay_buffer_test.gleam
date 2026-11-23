// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/session/replay_buffer.{Entry, Kept, Oversized}
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string
import gleeunit/should

fn entry(seq: Int) -> replay_buffer.Entry {
  Entry(seq:, event: "MESSAGE_CREATE", payload: raw_json.wrap(<<"{}">>))
}

fn push_all(seqs: List(Int)) -> replay_buffer.ReplayBuffer {
  list.fold(seqs, replay_buffer.new(), fn(buffer, seq) {
    let assert Kept(buffer, _) = replay_buffer.push(buffer, entry(seq))
    buffer
  })
}

pub fn keeps_entries_in_order_test() {
  push_all([1, 2, 3])
  |> replay_buffer.to_list
  |> list.map(fn(e) { e.seq })
  |> should.equal([1, 2, 3])
}

pub fn ack_drops_acknowledged_entries_test() {
  push_all([1, 2, 3, 4])
  |> replay_buffer.ack(2)
  |> replay_buffer.to_list
  |> list.map(fn(e) { e.seq })
  |> should.equal([3, 4])
}

pub fn after_returns_missed_entries_test() {
  push_all([1, 2, 3])
  |> replay_buffer.after(1)
  |> list.map(fn(e) { e.seq })
  |> should.equal([2, 3])
}

pub fn oversized_entry_is_refused_test() {
  let big = bit_array.from_string(string.repeat("x", 2_100_000))
  replay_buffer.new()
  |> replay_buffer.push(Entry(seq: 1, event: "X", payload: raw_json.wrap(big)))
  |> should.equal(Oversized)
}

pub fn eviction_reports_highest_evicted_seq_test() {
  let full = push_all(int.range(from: 4096, to: 0, with: [], run: list.prepend))
  let assert Kept(buffer, evicted) = replay_buffer.push(full, entry(4097))
  evicted |> should.equal(1)
  replay_buffer.size(buffer) |> should.equal(4096)
}

pub fn transfer_keeps_small_buffers_test() {
  let #(entries, dropped_up_to) = replay_buffer.transfer(push_all([5, 6]))
  list.length(entries) |> should.equal(2)
  dropped_up_to |> should.equal(0)
}
