// SPDX-License-Identifier: AGPL-3.0-or-later

mod aggregate_error;
pub mod asset_hash;
mod asset_size;
pub mod aws_sigv4;
pub mod bunny_ip_gate;
mod byte_budget;
mod byte_cache;
pub mod cli;
mod coalescer;
pub mod codec;
pub mod config;
pub mod constants;
pub mod disposition;
pub mod external_path;
pub mod healthcheck;
pub mod http_client;
pub mod http_headers;
pub mod image_quality;
pub mod image_transform;
mod media_limits;
pub mod media_process;
mod media_type;
pub mod metrics;
pub mod mime;
pub mod native;
pub mod nsfw;
pub mod output_format;
pub mod percent_decode;
pub mod public_net_policy;
pub mod query;
pub mod range;
pub mod request_log;
mod response_body_limit;
mod secret;
pub mod server;
pub mod signing;
pub mod spool;
pub mod storage;
pub mod thumbhash;
pub mod timed_semaphore;
mod transform_cache;
pub mod upload_relay;

#[cfg(test)]
mod component_tests;
#[cfg(test)]
mod policy_tests;
#[cfg(test)]
mod test_fixtures;

pub use server::run;

#[cfg(test)]
mod tests;
