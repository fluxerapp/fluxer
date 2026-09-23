// SPDX-License-Identifier: AGPL-3.0-or-later

use reqwest::dns::{Addrs, Name, Resolve, Resolving};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr};
use tokio::net::lookup_host;

type ResolveError = Box<dyn std::error::Error + Send + Sync>;

const BLOCKED_V4: &[(Ipv4Addr, u32)] = &[
    (Ipv4Addr::new(0, 0, 0, 0), 8),
    (Ipv4Addr::new(10, 0, 0, 0), 8),
    (Ipv4Addr::new(100, 64, 0, 0), 10),
    (Ipv4Addr::new(127, 0, 0, 0), 8),
    (Ipv4Addr::new(169, 254, 0, 0), 16),
    (Ipv4Addr::new(172, 16, 0, 0), 12),
    (Ipv4Addr::new(192, 0, 0, 0), 24),
    (Ipv4Addr::new(192, 0, 2, 0), 24),
    (Ipv4Addr::new(192, 88, 99, 0), 24),
    (Ipv4Addr::new(192, 168, 0, 0), 16),
    (Ipv4Addr::new(198, 18, 0, 0), 15),
    (Ipv4Addr::new(198, 51, 100, 0), 24),
    (Ipv4Addr::new(203, 0, 113, 0), 24),
    (Ipv4Addr::new(224, 0, 0, 0), 4),
    (Ipv4Addr::new(240, 0, 0, 0), 4),
];

const BLOCKED_V6: &[(Ipv6Addr, u32)] = &[
    (Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 0), 128),
    (Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1), 128),
    (Ipv6Addr::new(0x2001, 0x0db8, 0, 0, 0, 0, 0, 0), 32),
    (Ipv6Addr::new(0xfc00, 0, 0, 0, 0, 0, 0, 0), 7),
    (Ipv6Addr::new(0xfe80, 0, 0, 0, 0, 0, 0, 0), 10),
    (Ipv6Addr::new(0xff00, 0, 0, 0, 0, 0, 0, 0), 8),
];

const NAT64_PREFIX: [u8; 4] = [0x00, 0x64, 0xff, 0x9b];
const SIXTOFOUR_PREFIX: [u8; 2] = [0x20, 0x02];

pub struct PublicOnlyResolver;

impl Resolve for PublicOnlyResolver {
    fn resolve(&self, name: Name) -> Resolving {
        let host = name.as_str().to_owned();
        Box::pin(async move { resolve_public(&host).await })
    }
}

async fn resolve_public(host: &str) -> Result<Addrs, ResolveError> {
    let resolved: Vec<SocketAddr> = lookup_host((host, 0)).await?.collect();
    screen(resolved)
}

fn screen(resolved: Vec<SocketAddr>) -> Result<Addrs, ResolveError> {
    if resolved.is_empty() {
        return Err("host resolved to no addresses".into());
    }
    if resolved.iter().any(|addr| is_blocked(addr.ip())) {
        return Err("host resolved into blocked address space".into());
    }
    Ok(Box::new(resolved.into_iter()))
}

pub fn is_blocked(ip: IpAddr) -> bool {
    match ip {
        IpAddr::V4(ip) => is_blocked_v4(ip),
        IpAddr::V6(ip) => match embedded_v4(ip) {
            Some(embedded) => is_blocked_v4(embedded),
            None => is_blocked_v6(ip),
        },
    }
}

fn is_blocked_v4(ip: Ipv4Addr) -> bool {
    let value = ip.to_bits();
    BLOCKED_V4
        .iter()
        .any(|(network, prefix)| masked_v4(value, *prefix) == masked_v4(network.to_bits(), *prefix))
}

fn is_blocked_v6(ip: Ipv6Addr) -> bool {
    let value = ip.to_bits();
    BLOCKED_V6
        .iter()
        .any(|(network, prefix)| masked_v6(value, *prefix) == masked_v6(network.to_bits(), *prefix))
}

fn masked_v4(value: u32, prefix: u32) -> u32 {
    match prefix {
        0 => 0,
        _ => value & (u32::MAX << (u32::BITS - prefix)),
    }
}

fn masked_v6(value: u128, prefix: u32) -> u128 {
    match prefix {
        0 => 0,
        _ => value & (u128::MAX << (u128::BITS - prefix)),
    }
}

fn embedded_v4(ip: Ipv6Addr) -> Option<Ipv4Addr> {
    let octets = ip.octets();
    let quad = |start: usize| {
        Ipv4Addr::new(
            octets[start],
            octets[start + 1],
            octets[start + 2],
            octets[start + 3],
        )
    };
    if octets[..10] == [0; 10] && octets[10] == 0xff && octets[11] == 0xff {
        return Some(quad(12));
    }
    if octets[..4] == NAT64_PREFIX && octets[4..12] == [0; 8] {
        return Some(quad(12));
    }
    if octets[..12] == [0; 12] {
        return Some(quad(12));
    }
    if octets[..2] == SIXTOFOUR_PREFIX {
        return Some(quad(2));
    }
    None
}
