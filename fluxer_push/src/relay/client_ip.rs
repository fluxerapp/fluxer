// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::config::RelayConfig;
use axum::http::HeaderMap;
use std::net::{IpAddr, SocketAddr};

fn resolve(cfg: &RelayConfig, peer: SocketAddr, headers: &HeaderMap) -> Option<IpAddr> {
    if !cfg.trust_client_ip_header {
        return Some(peer.ip().to_canonical());
    }
    headers
        .get(&cfg.client_ip_header_name)
        .and_then(|value| value.to_str().ok())
        .and_then(nearest_entry)
        .map(|ip| ip.to_canonical())
}

pub fn for_rate_limit(cfg: &RelayConfig, peer: SocketAddr, headers: &HeaderMap) -> IpAddr {
    resolve(cfg, peer, headers).unwrap_or_else(|| peer.ip().to_canonical())
}

fn nearest_entry(value: &str) -> Option<IpAddr> {
    value
        .rsplit(',')
        .filter_map(|entry| entry.trim().trim_matches(['[', ']']).parse().ok())
        .next()
}
