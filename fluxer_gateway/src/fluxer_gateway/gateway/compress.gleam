// SPDX-License-Identifier: AGPL-3.0-or-later

import fluxer_gateway/gateway/zstd.{type Compressor, type Decompressor}
import gleam/bytes_tree.{type BytesTree}
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/result

pub type Compression {
  NoCompression
  ZstdStream
}

pub opaque type Context {
  Plain
  Stream(
    window_log: Int,
    compressor: Option(Compressor),
    decompressor: Option(Decompressor),
  )
}

pub type CompressError {
  CompressFailed
}

pub type DecompressError {
  DecompressTooLarge
  DecompressFailed
}

const level = 3

const max_block_size = 16_384

const decompress_window_log_max = 21

pub fn parse_compression(
  value: Option(String),
  stream: Option(String),
) -> Compression {
  case value, stream {
    Some("zstd-stream"), Some("1") | Some("zstd-stream"), Some("true") ->
      ZstdStream
    _, _ -> NoCompression
  }
}

pub fn clamp_window_log(window_log: Int) -> Int {
  int.clamp(window_log, min: 10, max: 21)
}

pub fn new_context(compression: Compression, window_log: Int) -> Context {
  case compression {
    NoCompression -> Plain
    ZstdStream ->
      Stream(
        window_log: clamp_window_log(window_log),
        compressor: None,
        decompressor: None,
      )
  }
}

pub fn compression(context: Context) -> Compression {
  case context {
    Plain -> NoCompression
    Stream(..) -> ZstdStream
  }
}

pub fn compress(
  context: Context,
  data: BytesTree,
) -> Result(#(BytesTree, Context), CompressError) {
  case context {
    Plain -> Ok(#(data, context))
    Stream(compressor: Some(compressor), ..) ->
      run_compress(compressor, data)
      |> wrap(context)
    Stream(window_log:, compressor: None, ..) ->
      case zstd.compressor_new(level, window_log, max_block_size) {
        Ok(compressor) ->
          run_compress(compressor, data)
          |> wrap(Stream(..context, compressor: Some(compressor)))
        Error(_) -> Error(CompressFailed)
      }
  }
}

fn wrap(
  result: Result(BitArray, CompressError),
  context: Context,
) -> Result(#(BytesTree, Context), CompressError) {
  result.map(result, fn(bytes) { #(bytes_tree.from_bit_array(bytes), context) })
}

fn run_compress(
  compressor: Compressor,
  data: BytesTree,
) -> Result(BitArray, CompressError) {
  case zstd.compress(compressor, data) {
    Error(zstd.NeedsDirty) -> zstd.compress_dirty(compressor, data)
    other -> other
  }
  |> result.replace_error(CompressFailed)
}

pub fn decompress(
  context: Context,
  data: BitArray,
  max_size: Int,
) -> Result(#(BitArray, Context), DecompressError) {
  case context {
    Plain -> Ok(#(data, context))
    Stream(decompressor: Some(decompressor), ..) ->
      run_decompress(decompressor, data, max_size, context)
    Stream(decompressor: None, ..) ->
      case zstd.decompressor_new(decompress_window_log_max) {
        Ok(decompressor) ->
          run_decompress(
            decompressor,
            data,
            max_size,
            Stream(..context, decompressor: Some(decompressor)),
          )
        Error(_) -> Error(DecompressFailed)
      }
  }
}

fn run_decompress(
  decompressor: Decompressor,
  data: BitArray,
  max_size: Int,
  context: Context,
) -> Result(#(BitArray, Context), DecompressError) {
  case zstd.decompress(decompressor, data, max_size) {
    Ok(bytes) -> Ok(#(bytes, context))
    Error(zstd.BadInput("too_large")) -> Error(DecompressTooLarge)
    Error(_) -> Error(DecompressFailed)
  }
}
