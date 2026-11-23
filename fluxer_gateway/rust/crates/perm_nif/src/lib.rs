// SPDX-License-Identifier: AGPL-3.0-or-later

#![deny(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::indexing_slicing,
    clippy::arithmetic_side_effects
)]

pub mod decode;
#[cfg(feature = "nif")]
pub mod encode;
#[cfg(feature = "nif")]
pub mod nifs;
#[cfg(feature = "nif")]
mod resource;

#[cfg(feature = "nif")]
pub use resource::{DeltaResource, delta_from_term, link, shared_from_term};
