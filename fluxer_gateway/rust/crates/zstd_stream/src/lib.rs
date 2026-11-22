// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod iodata;
#[cfg(feature = "nif")]
mod resource;
mod stream;

#[cfg(feature = "nif")]
pub mod nifs;

#[cfg(feature = "nif")]
pub use resource::{CompressorResource, DecompressorResource};
pub use stream::{
    Compressor, CompressorConfig, Decompressor, MAX_BLOCK_SIZE, MAX_TABLE_LOG, MAX_WINDOW_LOG,
    MIN_BLOCK_SIZE, MIN_WINDOW_LOG, Output, StreamError, output_bound,
};

#[cfg(feature = "nif")]
pub fn link(env: rustler::Env) -> bool {
    env.register::<CompressorResource>().is_ok() && env.register::<DecompressorResource>().is_ok()
}
