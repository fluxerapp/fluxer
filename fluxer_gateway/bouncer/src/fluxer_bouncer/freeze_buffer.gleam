// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/list
import gleam/option.{type Option, None, Some}

pub opaque type FreezeBuffer {
  FreezeBuffer(
    newest_first: List(#(Int, BitArray)),
    next_seq: Int,
    bytes: Int,
    limit: Int,
    sealed: Option(Int),
  )
}

pub type BufferError {
  OutOfOrder(expected: Int, got: Int)
  Overflow(limit: Int)
  AlreadySealed(last_seq: Int)
}

pub fn new(limit_bytes: Int) -> FreezeBuffer {
  FreezeBuffer(
    newest_first: [],
    next_seq: 1,
    bytes: 0,
    limit: limit_bytes,
    sealed: None,
  )
}

pub fn push(
  buffer: FreezeBuffer,
  seq: Int,
  bytes: BitArray,
) -> Result(FreezeBuffer, BufferError) {
  let size = bit_array.byte_size(bytes)
  case buffer.sealed {
    Some(last) if seq > last -> Error(AlreadySealed(last))
    _ ->
      case seq == buffer.next_seq, buffer.bytes + size > buffer.limit {
        False, _ -> Error(OutOfOrder(expected: buffer.next_seq, got: seq))
        True, True -> Error(Overflow(buffer.limit))
        True, False ->
          Ok(
            FreezeBuffer(
              ..buffer,
              newest_first: [#(seq, bytes), ..buffer.newest_first],
              next_seq: seq + 1,
              bytes: buffer.bytes + size,
            ),
          )
      }
  }
}

pub fn seal(
  buffer: FreezeBuffer,
  through: Int,
) -> Result(FreezeBuffer, BufferError) {
  case buffer.sealed {
    Some(last) if last == through -> Ok(buffer)
    Some(last) -> Error(AlreadySealed(last))
    None ->
      case through < last_seq(buffer) {
        True -> Error(OutOfOrder(expected: last_seq(buffer), got: through))
        False -> Ok(FreezeBuffer(..buffer, sealed: Some(through)))
      }
  }
}

pub fn last_seq(buffer: FreezeBuffer) -> Int {
  buffer.next_seq - 1
}

pub fn is_complete(buffer: FreezeBuffer) -> Bool {
  buffer.sealed == Some(last_seq(buffer))
}

pub fn entries_from(buffer: FreezeBuffer, seq: Int) -> List(#(Int, BitArray)) {
  list.fold(buffer.newest_first, [], fn(acc, entry) {
    case entry.0 >= seq {
      True -> [entry, ..acc]
      False -> acc
    }
  })
}

pub fn drop_through(buffer: FreezeBuffer, seq: Int) -> FreezeBuffer {
  let #(kept, dropped) =
    list.partition(buffer.newest_first, fn(entry) { entry.0 > seq })
  let freed =
    list.fold(dropped, 0, fn(total, entry) {
      total + bit_array.byte_size(entry.1)
    })
  FreezeBuffer(..buffer, newest_first: kept, bytes: buffer.bytes - freed)
}

pub fn size_bytes(buffer: FreezeBuffer) -> Int {
  buffer.bytes
}
