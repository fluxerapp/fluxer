// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result

pub const header_version = 1

pub const header_size = 26

pub const chunk_size = 262_144

pub const chunk_window = 4

pub type Part {
  Tail
  Inbox
}

pub type Header {
  Header(
    ns: Int,
    codec_version: Int,
    part: Part,
    flags: Int,
    epoch: Int,
    body_size: Int,
    checksum: Int,
  )
}

pub type HeaderError {
  BadMagic
  UnknownHeaderVersion(version: Int)
  UnknownPart(code: Int)
  FieldOutOfRange(field: String)
  Truncated
  SizeMismatch(declared: Int, actual: Int)
  ChecksumMismatch
}

pub type ChunkError {
  NoChunks
  MissingChunk(index: Int)
  DuplicateChunk(index: Int)
  ChunkOutOfRange(index: Int, count: Int)
}

@external(erlang, "erlang", "crc32")
fn crc32(data: BitArray) -> Int

pub fn part_code(part: Part) -> Int {
  case part {
    Tail -> 1
    Inbox -> 2
  }
}

pub fn part_from_code(code: Int) -> Result(Part, HeaderError) {
  case code {
    1 -> Ok(Tail)
    2 -> Ok(Inbox)
    other -> Error(UnknownPart(other))
  }
}

fn in_range(value: Int, bits: Int, field: String) -> Result(Nil, HeaderError) {
  case value >= 0 && value < int.bitwise_shift_left(1, bits) {
    True -> Ok(Nil)
    False -> Error(FieldOutOfRange(field))
  }
}

pub fn wrap(
  ns ns: Int,
  codec_version codec_version: Int,
  part part: Part,
  flags flags: Int,
  epoch epoch: Int,
  body body: BitArray,
) -> Result(BitArray, HeaderError) {
  let size = bit_array.byte_size(body)
  use _ <- result.try(in_range(ns, 8, "ns"))
  use _ <- result.try(in_range(codec_version, 16, "codec_version"))
  use _ <- result.try(in_range(flags, 8, "flags"))
  use _ <- result.try(in_range(epoch, 64, "epoch"))
  use _ <- result.try(in_range(size, 32, "body_size"))
  Ok(<<
    "FXHB":utf8,
    header_version:8,
    ns:8,
    codec_version:16,
    part_code(part):8,
    flags:8,
    epoch:64,
    size:32,
    crc32(body):32,
    body:bits,
  >>)
}

pub fn header(bytes: BitArray) -> Result(Header, HeaderError) {
  case bytes {
    <<
      "FXHB":utf8,
      version:8,
      ns:8,
      codec_version:16,
      part:8,
      flags:8,
      epoch:64,
      size:32,
      checksum:32,
      _:bits,
    >> ->
      case version == header_version {
        False -> Error(UnknownHeaderVersion(version))
        True -> {
          use part <- result.map(part_from_code(part))
          Header(
            ns: ns,
            codec_version: codec_version,
            part: part,
            flags: flags,
            epoch: epoch,
            body_size: size,
            checksum: checksum,
          )
        }
      }
    <<"FXHB":utf8, _:bits>> -> Error(Truncated)
    _ -> Error(BadMagic)
  }
}

pub fn unwrap(bytes: BitArray) -> Result(#(Header, BitArray), HeaderError) {
  use found <- result.try(header(bytes))
  let actual = bit_array.byte_size(bytes) - header_size
  case actual == found.body_size {
    False -> Error(SizeMismatch(declared: found.body_size, actual: actual))
    True -> {
      use body <- result.try(
        bit_array.slice(bytes, header_size, actual)
        |> result.replace_error(Truncated),
      )
      case crc32(body) == found.checksum {
        True -> Ok(#(found, body))
        False -> Error(ChecksumMismatch)
      }
    }
  }
}

pub fn split(bytes: BitArray) -> List(BitArray) {
  split_loop(bytes, [])
}

fn split_loop(bytes: BitArray, acc: List(BitArray)) -> List(BitArray) {
  let size = bit_array.byte_size(bytes)
  case size <= chunk_size {
    True -> list.reverse([bytes, ..acc])
    False ->
      case
        bit_array.slice(bytes, 0, chunk_size),
        bit_array.slice(bytes, chunk_size, size - chunk_size)
      {
        Ok(head), Ok(rest) -> split_loop(rest, [head, ..acc])
        _, _ -> list.reverse([bytes, ..acc])
      }
  }
}

pub fn join(
  chunks: List(#(Int, BitArray)),
  count: Int,
) -> Result(BitArray, ChunkError) {
  case count <= 0 {
    True -> Error(NoChunks)
    False -> {
      let sorted = list.sort(chunks, fn(a, b) { int.compare(a.0, b.0) })
      join_loop(sorted, 0, count, [])
    }
  }
}

fn join_loop(
  chunks: List(#(Int, BitArray)),
  expected: Int,
  count: Int,
  acc: List(BitArray),
) -> Result(BitArray, ChunkError) {
  case chunks {
    [] ->
      case expected == count {
        True -> Ok(bit_array.concat(list.reverse(acc)))
        False -> Error(MissingChunk(expected))
      }
    [#(index, _), ..] if index >= count || index < 0 ->
      Error(ChunkOutOfRange(index: index, count: count))
    [#(index, _), ..] if index < expected -> Error(DuplicateChunk(index))
    [#(index, _), ..] if index > expected -> Error(MissingChunk(expected))
    [#(_, bytes), ..rest] ->
      join_loop(rest, expected + 1, count, [bytes, ..acc])
  }
}
