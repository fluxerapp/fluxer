// SPDX-License-Identifier: AGPL-3.0-or-later

import bouncer_proto/blob.{type Part}
import bouncer_proto/message.{type MoveId}
import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/int
import gleam/list

pub type Blob {
  Blob(
    estimate: Int,
    received: Int,
    admitted_at: Int,
    chunks: Dict(#(Part, Int), BitArray),
    counts: Dict(Part, Int),
  )
}

pub opaque type BlobStore {
  BlobStore(blobs: Dict(MoveId, Blob), used: Int, limit: Int)
}

pub type StoreError {
  OverBudget(limit: Int)
  UnknownMove(move: MoveId)
  ChunkMismatch(part: Part, index: Int)
  Incomplete(part: Part, missing: Int)
}

pub fn new(limit_bytes: Int) -> BlobStore {
  BlobStore(blobs: dict.new(), used: 0, limit: limit_bytes)
}

fn reserved(blob: Blob) -> Int {
  int.max(blob.estimate, blob.received)
}

pub fn admit(
  store: BlobStore,
  move: MoveId,
  bytes_estimate: Int,
  now_ms: Int,
) -> Result(BlobStore, StoreError) {
  case dict.has_key(store.blobs, move) {
    True -> Ok(store)
    False -> {
      let estimate = int.max(bytes_estimate, 0)
      case store.used + estimate > store.limit {
        True -> Error(OverBudget(store.limit))
        False ->
          Ok(
            BlobStore(
              ..store,
              blobs: dict.insert(
                store.blobs,
                move,
                Blob(
                  estimate: estimate,
                  received: 0,
                  admitted_at: now_ms,
                  chunks: dict.new(),
                  counts: dict.new(),
                ),
              ),
              used: store.used + estimate,
            ),
          )
      }
    }
  }
}

pub fn put_chunk(
  store: BlobStore,
  move: MoveId,
  part: Part,
  index: Int,
  count: Int,
  bytes: BitArray,
) -> Result(BlobStore, StoreError) {
  case dict.get(store.blobs, move) {
    Error(Nil) -> Error(UnknownMove(move))
    Ok(blob) -> {
      let known_count = case dict.get(blob.counts, part) {
        Ok(known) -> known
        Error(Nil) -> count
      }
      case
        count < 1 || index < 0 || index >= count || known_count != count,
        dict.get(blob.chunks, #(part, index))
      {
        True, _ -> Error(ChunkMismatch(part, index))
        False, Ok(existing) if existing == bytes -> Ok(store)
        False, Ok(_) -> Error(ChunkMismatch(part, index))
        False, Error(Nil) -> {
          let grown =
            Blob(
              ..blob,
              received: blob.received + bit_array.byte_size(bytes),
              chunks: dict.insert(blob.chunks, #(part, index), bytes),
              counts: dict.insert(blob.counts, part, count),
            )
          let used = store.used - reserved(blob) + reserved(grown)
          case used > store.limit {
            True -> Error(OverBudget(store.limit))
            False ->
              Ok(
                BlobStore(
                  ..store,
                  blobs: dict.insert(store.blobs, move, grown),
                  used: used,
                ),
              )
          }
        }
      }
    }
  }
}

pub fn chunk(
  store: BlobStore,
  move: MoveId,
  part: Part,
  index: Int,
) -> Result(#(BitArray, Int), StoreError) {
  case dict.get(store.blobs, move) {
    Error(Nil) -> Error(UnknownMove(move))
    Ok(blob) ->
      case dict.get(blob.chunks, #(part, index)), dict.get(blob.counts, part) {
        Ok(bytes), Ok(count) -> Ok(#(bytes, count))
        _, _ -> Error(ChunkMismatch(part, index))
      }
  }
}

pub fn chunks(
  store: BlobStore,
  move: MoveId,
  part: Part,
) -> Result(List(#(Int, BitArray)), StoreError) {
  case dict.get(store.blobs, move) {
    Error(Nil) -> Error(UnknownMove(move))
    Ok(blob) -> {
      let present =
        blob.chunks
        |> dict.to_list
        |> list.filter_map(fn(entry) {
          let #(#(chunk_part, index), bytes) = entry
          case chunk_part == part {
            True -> Ok(#(index, bytes))
            False -> Error(Nil)
          }
        })
        |> list.sort(fn(a, b) { int.compare(a.0, b.0) })
      case dict.get(blob.counts, part) {
        Ok(count) ->
          case list.length(present) == count {
            True -> Ok(present)
            False -> Error(Incomplete(part, count - list.length(present)))
          }
        Error(Nil) -> Error(Incomplete(part, 1))
      }
    }
  }
}

pub fn complete(store: BlobStore, move: MoveId, part: Part) -> Bool {
  case chunks(store, move, part) {
    Ok(_) -> True
    Error(_) -> False
  }
}

pub fn drop(store: BlobStore, move: MoveId) -> BlobStore {
  case dict.get(store.blobs, move) {
    Error(Nil) -> store
    Ok(blob) ->
      BlobStore(
        ..store,
        blobs: dict.delete(store.blobs, move),
        used: store.used - reserved(blob),
      )
  }
}

pub fn expire(
  store: BlobStore,
  now_ms: Int,
  ttl_ms: Int,
) -> #(BlobStore, List(MoveId)) {
  let expired =
    store.blobs
    |> dict.to_list
    |> list.filter_map(fn(entry) {
      case now_ms - { entry.1 }.admitted_at > ttl_ms {
        True -> Ok(entry.0)
        False -> Error(Nil)
      }
    })
  #(list.fold(expired, store, drop), expired)
}

pub fn used(store: BlobStore) -> Int {
  store.used
}
