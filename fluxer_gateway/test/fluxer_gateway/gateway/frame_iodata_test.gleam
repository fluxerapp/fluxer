// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/core/raw_json
import fluxer_gateway/gateway/compress
import fluxer_gateway/gateway/encode
import fluxer_gateway/gateway/ws/frame
import fluxer_gateway/gateway/zstd
import gleam/bit_array
import gleam/bytes_tree
import gleam/string

fn body(size: Int) -> BitArray {
  let text = bit_array.from_string(string.repeat("fluxer ", size / 7 + 1))
  let assert Ok(bytes) = bit_array.slice(text, 0, size)
  bytes
}

pub fn dispatch_splices_a_multi_part_payload_test() {
  let payload =
    raw_json.object([
      #("id", raw_json.wrap(<<"\"1\"">>)),
      #("content", raw_json.wrap_string("\"hi\"")),
    ])
  let frame = encode.dispatch(7, "MESSAGE_CREATE", payload)
  assert bytes_tree.to_bit_array(frame)
    == <<
      "{\"op\":0,\"t\":\"MESSAGE_CREATE\",\"s\":7,\"d\":{\"id\":\"1\",\"content\":\"hi\"}}",
    >>
}

pub fn frame_header_counts_every_chunk_test() {
  let small = bytes_tree.from_bit_array(body(60)) |> bytes_tree.append(body(65))
  let assert <<0x81, 125, rest:bits>> =
    bytes_tree.to_bit_array(frame.encode(frame.Text, small))
  assert bit_array.byte_size(rest) == 125
  let medium =
    bytes_tree.from_bit_array(body(100)) |> bytes_tree.append(body(100))
  let assert <<0x82, 126, 200:16, rest:bits>> =
    bytes_tree.to_bit_array(frame.encode(frame.Binary, medium))
  assert rest == bit_array.append(body(100), body(100))
  let large =
    bytes_tree.from_bit_array(body(40_000))
    |> bytes_tree.append(body(40_000))
  let assert <<0x81, 127, 80_000:64, _:bytes-size(80_000)>> =
    bytes_tree.to_bit_array(frame.encode(frame.Text, large))
}

pub fn zstd_compresses_iodata_like_flat_bytes_test() {
  let assert Ok(flat) = zstd.compressor_new(3, 17, 16_384)
  let assert Ok(chunked) = zstd.compressor_new(3, 17, 16_384)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let tree =
    bytes_tree.from_bit_array(<<"{\"d\":\"">>)
    |> bytes_tree.append(body(90_000))
    |> bytes_tree.append_tree(bytes_tree.from_string("\"}"))
    |> bytes_tree.prepend(<<>>)
  let whole = bytes_tree.to_bit_array(tree)
  let assert Error(zstd.NeedsDirty) = zstd.compress(chunked, tree)
  let assert Ok(from_tree) = zstd.compress_dirty(chunked, tree)
  let assert Ok(from_bits) =
    zstd.compress_dirty(flat, bytes_tree.from_bit_array(whole))
  assert from_tree == from_bits
  let assert Ok(back) = zstd.decompress(decompressor, from_tree, 1_000_000)
  assert back == whole
}

pub fn zstd_compresses_an_empty_tree_test() {
  let assert Ok(compressor) = zstd.compressor_new(3, 17, 16_384)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let assert Ok(frame) = zstd.compress(compressor, bytes_tree.new())
  let assert Ok(<<>>) = zstd.decompress(decompressor, frame, 10)
  Nil
}

pub fn stream_context_sends_the_dispatch_bytes_test() {
  let context = compress.new_context(compress.ZstdStream, 17)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let frame = encode.dispatch(3, "TYPING_START", raw_json.wrap(<<"{\"a\":1}">>))
  let assert Ok(#(sent, _)) = compress.compress(context, frame)
  let assert Ok(back) =
    zstd.decompress(decompressor, bytes_tree.to_bit_array(sent), 1000)
  assert back == bytes_tree.to_bit_array(frame)
}

pub fn stream_context_moves_large_frames_to_the_dirty_scheduler_test() {
  let context = compress.new_context(compress.ZstdStream, 17)
  let assert Ok(decompressor) = zstd.decompressor_new(21)
  let content = body(100_000)
  let frame =
    encode.dispatch(
      4,
      "MESSAGE_CREATE",
      raw_json.concat([
        raw_json.wrap(<<"\"">>),
        raw_json.wrap(content),
        raw_json.wrap(<<"\"">>),
      ]),
    )
  let assert Ok(#(sent, _)) = compress.compress(context, frame)
  let assert Ok(back) =
    zstd.decompress(decompressor, bytes_tree.to_bit_array(sent), 1_000_000)
  assert back == bytes_tree.to_bit_array(frame)
}
