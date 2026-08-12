// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod config;
pub mod content;
pub mod downloads;
pub mod fonts;
pub mod geoip;
pub mod i18n;
pub mod invariant_text;
pub mod pricing;
pub mod rate_limit;
pub mod request_context;
pub mod routes;
pub mod swish;
pub mod templates;

pub use routes::build_router;
