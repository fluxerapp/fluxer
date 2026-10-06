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
        .and_then(|value| entry_from_right(value, cfg.trusted_proxy_hops))
        .map(|ip| ip.to_canonical())
}

pub fn for_rate_limit(cfg: &RelayConfig, peer: SocketAddr, headers: &HeaderMap) -> IpAddr {
    resolve(cfg, peer, headers).unwrap_or_else(|| peer.ip().to_canonical())
}

fn entry_from_right(value: &str, skip: usize) -> Option<IpAddr> {
    value
        .rsplit(',')
        .map(|entry| entry.trim().trim_matches(['[', ']']))
        .nth(skip)
        .and_then(|entry| entry.parse().ok())
}

#[cfg(test)]
mod tests {
    use super::*;

    const EDGE: &str = "203.0.113.9";
    const INSTANCE: &str = "198.51.100.7";

    #[test]
    fn the_rightmost_entry_is_the_edge_not_the_sending_instance() {
        let chain = format!("{INSTANCE}, {EDGE}");
        assert_eq!(entry_from_right(&chain, 0).unwrap().to_string(), EDGE);
        assert_eq!(entry_from_right(&chain, 1).unwrap().to_string(), INSTANCE);
    }

    #[test]
    fn a_longer_chain_still_names_the_sending_instance() {
        let chain = format!("1.2.3.4, 5.6.7.8, {INSTANCE}, {EDGE}");
        assert_eq!(entry_from_right(&chain, 1).unwrap().to_string(), INSTANCE);
    }

    #[test]
    fn a_short_chain_yields_nothing_rather_than_a_wrong_answer() {
        assert!(entry_from_right(EDGE, 1).is_none());
        assert!(entry_from_right("", 1).is_none());
    }
}
