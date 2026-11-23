// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json.{type RawJson}
import gleam/dict.{type Dict}
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/set.{type Set}

pub type DmMessage {
  DmMessage(
    author: Int,
    channel: Option(Int),
    message: Option(Int),
    suppressed: Bool,
    data: RawJson,
  )
}

pub type Limits {
  Limits(max_entries: Int, max_bytes: Int)
}

pub type Pending {
  Pending(data: RawJson, author: Int, buffered_at: Int)
}

pub type PushAction {
  SendNow(flushed: List(Pending), message: Pending)
  Buffered(dropped: Int)
  Ignored
}

type Entry {
  Entry(
    channel: Int,
    message: Int,
    data: RawJson,
    author: Int,
    buffered_at: Int,
    bytes: Int,
  )
}

pub opaque type PushBuffer {
  PushBuffer(entries: List(Entry), read_marks: Dict(Int, Int), eligible: Bool)
}

const max_read_marks = 32

const entry_overhead_bytes = 200

pub fn default_limits() -> Limits {
  Limits(max_entries: 128, max_bytes: 1_048_576)
}

pub fn new() -> PushBuffer {
  PushBuffer(entries: [], read_marks: dict.new(), eligible: True)
}

pub fn on_message(
  buffer: PushBuffer,
  message: DmMessage,
  self_user: Int,
  eligible: Bool,
  now_ms: Int,
  limits: Limits,
) -> #(PushBuffer, PushAction) {
  case message.author == self_user {
    True ->
      case message.channel, message.message {
        Some(channel), Some(id) -> #(on_ack(buffer, channel, id), Ignored)
        _, _ -> #(buffer, Ignored)
      }
    False ->
      case is_read(buffer, message) || message.suppressed {
        True -> #(buffer, Ignored)
        False -> route(buffer, message, eligible, now_ms, limits)
      }
  }
}

fn is_read(buffer: PushBuffer, message: DmMessage) -> Bool {
  case message.message {
    None -> False
    Some(id) -> id <= mark(buffer, message.channel)
  }
}

fn mark(buffer: PushBuffer, channel: Option(Int)) -> Int {
  channel
  |> option.to_result(Nil)
  |> result.try(dict.get(buffer.read_marks, _))
  |> result.unwrap(0)
}

fn route(
  buffer: PushBuffer,
  message: DmMessage,
  eligible: Bool,
  now_ms: Int,
  limits: Limits,
) -> #(PushBuffer, PushAction) {
  case eligible, message.channel, message.message {
    True, _, _ -> {
      let #(flushed_buffer, flushed) = flush(buffer)
      #(
        flushed_buffer,
        SendNow(
          flushed:,
          message: Pending(
            data: message.data,
            author: message.author,
            buffered_at: now_ms,
          ),
        ),
      )
    }
    False, Some(channel), Some(id) -> {
      let entry =
        Entry(
          channel:,
          message: id,
          data: message.data,
          author: message.author,
          buffered_at: now_ms,
          bytes: raw_json.byte_size(message.data) + entry_overhead_bytes,
        )
      let grown = [entry, ..buffer.entries]
      let capped = cap(grown, limits)
      #(
        PushBuffer(..buffer, entries: capped),
        Buffered(dropped: list.length(grown) - list.length(capped)),
      )
    }
    False, _, _ -> #(buffer, Ignored)
  }
}

fn cap(entries: List(Entry), limits: Limits) -> List(Entry) {
  let newest = case limits.max_entries {
    0 -> []
    n -> list.take(entries, n)
  }
  case limits.max_bytes {
    0 -> newest
    max -> take_bytes(newest, max, 0, [])
  }
}

fn take_bytes(
  entries: List(Entry),
  max: Int,
  used: Int,
  acc: List(Entry),
) -> List(Entry) {
  case entries {
    [] -> list.reverse(acc)
    [entry, ..rest] ->
      case used + entry.bytes <= max {
        True -> take_bytes(rest, max, used + entry.bytes, [entry, ..acc])
        False -> list.reverse(acc)
      }
  }
}

pub fn on_ack(buffer: PushBuffer, channel: Int, message: Int) -> PushBuffer {
  case channel > 0 && message > 0 {
    False -> buffer
    True -> {
      let entries =
        list.filter(buffer.entries, fn(e) {
          !{ e.channel == channel && e.message <= message }
        })
      let current = mark(buffer, Some(channel))
      let marks =
        buffer.read_marks
        |> dict.insert(channel, int.max(message, current))
        |> cap_marks
      PushBuffer(..buffer, entries:, read_marks: marks)
    }
  }
}

fn cap_marks(marks: Dict(Int, Int)) -> Dict(Int, Int) {
  case dict.size(marks) > max_read_marks {
    False -> marks
    True ->
      case oldest(dict.to_list(marks)) {
        Some(channel) -> dict.delete(marks, channel)
        None -> marks
      }
  }
}

fn oldest(marks: List(#(Int, Int))) -> Option(Int) {
  list.fold(marks, None, fn(best, pair) {
    let #(channel, m) = pair
    case best {
      None -> Some(#(m, channel))
      Some(#(bm, bc)) ->
        case m < bm || { m == bm && channel < bc } {
          True -> Some(#(m, channel))
          False -> best
        }
    }
  })
  |> option.map(fn(pair) { pair.1 })
}

pub fn flush(buffer: PushBuffer) -> #(PushBuffer, List(Pending)) {
  let pending =
    list.reverse(buffer.entries)
    |> list.map(fn(e) {
      Pending(data: e.data, author: e.author, buffered_at: e.buffered_at)
    })
  #(PushBuffer(..buffer, entries: []), pending)
}

pub fn merge(
  into: PushBuffer,
  carried: PushBuffer,
  limits: Limits,
) -> PushBuffer {
  let marks =
    dict.fold(carried.read_marks, into.read_marks, fn(acc, channel, value) {
      dict.upsert(acc, channel, fn(existing) {
        case existing {
          Some(current) -> int.max(current, value)
          None -> value
        }
      })
    })
  let merged = PushBuffer(..into, read_marks: marks)
  let entries =
    list.append(into.entries, carried.entries)
    |> list.filter(fn(e) { e.message > mark(merged, Some(e.channel)) })
    |> list.sort(fn(a, b) {
      case int.compare(b.buffered_at, a.buffered_at) {
        order.Eq -> int.compare(b.message, a.message)
        found -> found
      }
    })
    |> distinct([], set.new())
    |> cap(limits)
  PushBuffer(..merged, entries:, read_marks: cap_all_marks(marks))
}

fn distinct(
  entries: List(Entry),
  acc: List(Entry),
  seen: Set(#(Int, Int)),
) -> List(Entry) {
  case entries {
    [] -> list.reverse(acc)
    [entry, ..rest] -> {
      let key = #(entry.channel, entry.message)
      case set.contains(seen, key) {
        True -> distinct(rest, acc, seen)
        False -> distinct(rest, [entry, ..acc], set.insert(seen, key))
      }
    }
  }
}

fn cap_all_marks(marks: Dict(Int, Int)) -> Dict(Int, Int) {
  case dict.size(marks) > max_read_marks {
    False -> marks
    True -> cap_all_marks(cap_marks(marks))
  }
}

pub fn pending(buffer: PushBuffer) -> List(Pending) {
  flush(buffer).1
}

pub fn set_eligible(
  buffer: PushBuffer,
  eligible: Bool,
) -> #(PushBuffer, List(Pending), Bool) {
  let announce = !eligible && buffer.eligible
  let recorded = PushBuffer(..buffer, eligible:)
  case eligible, recorded.entries {
    True, [_, ..] -> {
      let #(flushed_buffer, pending) = flush(recorded)
      #(flushed_buffer, pending, announce)
    }
    _, _ -> #(recorded, [], announce)
  }
}

pub fn size(buffer: PushBuffer) -> Int {
  list.length(buffer.entries)
}

pub fn is_empty(buffer: PushBuffer) -> Bool {
  buffer.entries == []
}

pub type Kept {
  Kept(channel: Int, message: Int, data: RawJson, author: Int, buffered_at: Int)
}

pub fn kept(buffer: PushBuffer) -> List(Kept) {
  list.map(buffer.entries, fn(e) {
    Kept(
      channel: e.channel,
      message: e.message,
      data: e.data,
      author: e.author,
      buffered_at: e.buffered_at,
    )
  })
}

pub fn read_marks(buffer: PushBuffer) -> List(#(Int, Int)) {
  dict.to_list(buffer.read_marks)
}

pub fn is_eligible(buffer: PushBuffer) -> Bool {
  buffer.eligible
}

pub fn restore(
  kept kept: List(Kept),
  read_marks read_marks: List(#(Int, Int)),
  eligible eligible: Bool,
) -> PushBuffer {
  PushBuffer(
    entries: list.map(kept, fn(k) {
      Entry(
        channel: k.channel,
        message: k.message,
        data: k.data,
        author: k.author,
        buffered_at: k.buffered_at,
        bytes: raw_json.byte_size(k.data) + entry_overhead_bytes,
      )
    }),
    read_marks: dict.from_list(read_marks),
    eligible:,
  )
}
