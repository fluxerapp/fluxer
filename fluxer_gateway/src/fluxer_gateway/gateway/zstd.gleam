// SPDX-License-Identifier: AGPL-3.0-or-later

import gleam/bytes_tree.{type BytesTree}

pub type Compressor

pub type Decompressor

pub type NifError {
  NeedsDirty
  Broken
  Gone
  BadInput(reason: String)
}

@external(erlang, "fluxer_gateway@nif", "zstd_compressor_new")
pub fn compressor_new(
  level: Int,
  window_log: Int,
  max_block_size: Int,
) -> Result(Compressor, NifError)

@external(erlang, "fluxer_gateway@nif", "zstd_compress")
pub fn compress(
  compressor: Compressor,
  data: BytesTree,
) -> Result(BitArray, NifError)

@external(erlang, "fluxer_gateway@nif", "zstd_compress_dirty")
pub fn compress_dirty(
  compressor: Compressor,
  data: BytesTree,
) -> Result(BitArray, NifError)

@external(erlang, "fluxer_gateway@nif", "zstd_decompressor_new")
pub fn decompressor_new(window_log_max: Int) -> Result(Decompressor, NifError)

@external(erlang, "fluxer_gateway@nif", "zstd_decompress")
pub fn decompress(
  decompressor: Decompressor,
  data: BitArray,
  max_size: Int,
) -> Result(BitArray, NifError)
