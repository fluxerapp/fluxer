// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/int
import gleam/list
import gleam/string

pub type Entry {
  Entry(seq: Int, event: String, payload: RawJson)
}

pub opaque type ReplayBuffer {
  ReplayBuffer(front: List(Entry), back: List(Entry), count: Int, bytes: Int)
}

pub type Pushed {
  Kept(buffer: ReplayBuffer, evicted_up_to: Int)
  Oversized
}

const max_entries = 4096

const max_total_bytes = 16_777_216

const max_entry_bytes = 2_097_152

const max_transfer_bytes = 4_194_304

const entry_overhead_bytes = 64

pub fn new() -> ReplayBuffer {
  ReplayBuffer(front: [], back: [], count: 0, bytes: 0)
}

pub fn from_entries(entries: List(Entry)) -> ReplayBuffer {
  list.fold(entries, new(), fn(buffer, entry) {
    case push(buffer, entry) {
      Kept(buffer, _) -> buffer
      Oversized -> buffer
    }
  })
}

pub fn size(buffer: ReplayBuffer) -> Int {
  buffer.count
}

fn entry_bytes(entry: Entry) -> Int {
  raw_json.byte_size(entry.payload)
  + string.byte_size(entry.event)
  + entry_overhead_bytes
}

pub fn push(buffer: ReplayBuffer, entry: Entry) -> Pushed {
  let size = entry_bytes(entry)
  case size > max_entry_bytes {
    True -> Oversized
    False -> {
      let grown =
        ReplayBuffer(
          ..buffer,
          back: [entry, ..buffer.back],
          count: buffer.count + 1,
          bytes: buffer.bytes + size,
        )
      trim(grown, 0)
    }
  }
}

fn trim(buffer: ReplayBuffer, evicted: Int) -> Pushed {
  case buffer.count > max_entries || buffer.bytes > max_total_bytes {
    False -> Kept(buffer, evicted)
    True ->
      case pop_front(buffer) {
        Ok(#(entry, rest)) -> trim(rest, int.max(evicted, entry.seq))
        Error(Nil) -> Kept(buffer, evicted)
      }
  }
}

fn pop_front(buffer: ReplayBuffer) -> Result(#(Entry, ReplayBuffer), Nil) {
  case buffer.front {
    [entry, ..rest] ->
      Ok(#(
        entry,
        ReplayBuffer(
          ..buffer,
          front: rest,
          count: buffer.count - 1,
          bytes: buffer.bytes - entry_bytes(entry),
        ),
      ))
    [] ->
      case buffer.back {
        [] -> Error(Nil)
        back ->
          pop_front(ReplayBuffer(..buffer, front: list.reverse(back), back: []))
      }
  }
}

pub fn to_list(buffer: ReplayBuffer) -> List(Entry) {
  list.append(buffer.front, list.reverse(buffer.back))
}

pub fn ack(buffer: ReplayBuffer, seq: Int) -> ReplayBuffer {
  case pop_front(buffer) {
    Ok(#(entry, rest)) if entry.seq <= seq -> ack(rest, seq)
    _ -> buffer
  }
}

pub fn after(buffer: ReplayBuffer, seq: Int) -> List(Entry) {
  buffer |> to_list |> list.filter(fn(entry) { entry.seq > seq })
}

pub fn transfer(buffer: ReplayBuffer) -> #(List(Entry), Int) {
  let entries = to_list(buffer)
  let total =
    list.fold(entries, 0, fn(sum, entry) {
      sum + raw_json.byte_size(entry.payload)
    })
  trim_transfer(entries, total, 0)
}

fn trim_transfer(
  entries: List(Entry),
  total: Int,
  dropped_up_to: Int,
) -> #(List(Entry), Int) {
  case total <= max_transfer_bytes, entries {
    True, _ -> #(entries, dropped_up_to)
    False, [] -> #([], dropped_up_to)
    False, [dropped, ..rest] ->
      trim_transfer(
        rest,
        int.max(0, total - raw_json.byte_size(dropped.payload)),
        int.max(dropped_up_to, dropped.seq),
      )
  }
}
