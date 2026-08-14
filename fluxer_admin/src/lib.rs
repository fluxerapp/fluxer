// SPDX-License-Identifier: AGPL-3.0-or-later

pub mod acl;
pub mod admin_flags;
pub mod api;
pub mod config;
pub mod fonts;
pub mod middleware;
pub mod oauth2;
pub mod routes;
pub mod session;
pub mod state;
pub mod templates;
pub mod utils;

pub use routes::build_router;
