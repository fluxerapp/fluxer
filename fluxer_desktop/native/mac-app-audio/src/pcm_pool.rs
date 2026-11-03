// SPDX-License-Identifier: AGPL-3.0-or-later

pub use fluxer_sample_pool::PCM_SLOT_SAMPLES_MAX;
pub type PcmFramePool = fluxer_sample_pool::SamplePool<f32, true>;
pub type PooledPcmFrame = fluxer_sample_pool::PooledSamples<f32, true>;
pub const PCM_POOL_CAP: usize = 16;
