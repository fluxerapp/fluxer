// SPDX-License-Identifier: AGPL-3.0-or-later

use std::net::{IpAddr, Ipv4Addr, Ipv6Addr};

const NON_PUBLIC_IPV4_SUBNETS: &[(Ipv4Addr, u32)] = &[
    (Ipv4Addr::UNSPECIFIED, 8),
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

const PUBLIC_IPV4_SPECIAL_USE_EXCEPTIONS: &[(Ipv4Addr, u32)] = &[
    (Ipv4Addr::new(192, 0, 0, 9), 32),
    (Ipv4Addr::new(192, 0, 0, 10), 32),
];

const IPV6_GLOBAL_UNICAST_SUBNETS: &[(Ipv6Addr, u32)] =
    &[(Ipv6Addr::new(0x2000, 0, 0, 0, 0, 0, 0, 0), 3)];

const NON_PUBLIC_IPV6_SUBNETS: &[(Ipv6Addr, u32)] = &[
    (Ipv6Addr::UNSPECIFIED, 96),
    (Ipv6Addr::new(0x0064, 0xff9b, 1, 0, 0, 0, 0, 0), 48),
    (Ipv6Addr::new(0x0100, 0, 0, 0, 0, 0, 0, 0), 63),
    (Ipv6Addr::new(0x2001, 0, 0, 0, 0, 0, 0, 0), 23),
    (Ipv6Addr::new(0x2001, 0x0db8, 0, 0, 0, 0, 0, 0), 32),
    (Ipv6Addr::new(0x2002, 0, 0, 0, 0, 0, 0, 0), 16),
    (Ipv6Addr::new(0x3fff, 0, 0, 0, 0, 0, 0, 0), 20),
    (Ipv6Addr::new(0x5f00, 0, 0, 0, 0, 0, 0, 0), 16),
    (Ipv6Addr::new(0xfc00, 0, 0, 0, 0, 0, 0, 0), 7),
    (Ipv6Addr::new(0xfe80, 0, 0, 0, 0, 0, 0, 0), 10),
    (Ipv6Addr::new(0xfec0, 0, 0, 0, 0, 0, 0, 0), 10),
    (Ipv6Addr::new(0xff00, 0, 0, 0, 0, 0, 0, 0), 8),
];

const PUBLIC_IPV6_SPECIAL_USE_EXCEPTIONS: &[(Ipv6Addr, u32)] = &[
    (Ipv6Addr::new(0x2001, 1, 0, 0, 0, 0, 0, 1), 128),
    (Ipv6Addr::new(0x2001, 1, 0, 0, 0, 0, 0, 2), 128),
    (Ipv6Addr::new(0x2001, 1, 0, 0, 0, 0, 0, 3), 128),
    (Ipv6Addr::new(0x2001, 3, 0, 0, 0, 0, 0, 0), 32),
    (Ipv6Addr::new(0x2001, 4, 0x0112, 0, 0, 0, 0, 0), 48),
    (Ipv6Addr::new(0x2001, 0x20, 0, 0, 0, 0, 0, 0), 28),
    (Ipv6Addr::new(0x2001, 0x30, 0, 0, 0, 0, 0, 0), 28),
];

fn ipv4_to_u32(address: Ipv4Addr) -> u32 {
    u32::from_be_bytes(address.octets())
}

fn ipv4_in(address: Ipv4Addr, prefix: Ipv4Addr, bits: u32) -> bool {
    let mask = if bits == 0 {
        0
    } else {
        u32::MAX << (32 - bits)
    };
    (ipv4_to_u32(address) & mask) == (ipv4_to_u32(prefix) & mask)
}

fn ipv6_to_u128(address: Ipv6Addr) -> u128 {
    u128::from_be_bytes(address.octets())
}

fn ipv6_in(address: Ipv6Addr, prefix: Ipv6Addr, bits: u32) -> bool {
    let mask = if bits == 0 {
        0
    } else {
        u128::MAX << (128 - bits)
    };
    (ipv6_to_u128(address) & mask) == (ipv6_to_u128(prefix) & mask)
}

fn is_blocked_ipv4(address: Ipv4Addr) -> bool {
    if PUBLIC_IPV4_SPECIAL_USE_EXCEPTIONS
        .iter()
        .any(|(prefix, bits)| ipv4_in(address, *prefix, *bits))
    {
        return false;
    }
    NON_PUBLIC_IPV4_SUBNETS
        .iter()
        .any(|(prefix, bits)| ipv4_in(address, *prefix, *bits))
}

fn embedded_ipv4(address: Ipv6Addr) -> Option<Ipv4Addr> {
    if !ipv6_in(address, Ipv6Addr::new(0x0064, 0xff9b, 0, 0, 0, 0, 0, 0), 96) {
        return None;
    }
    let octets = address.octets();
    Some(Ipv4Addr::new(
        octets[12], octets[13], octets[14], octets[15],
    ))
}

fn is_blocked_ipv6(address: Ipv6Addr) -> bool {
    if let Some(mapped) = address.to_ipv4_mapped() {
        return is_blocked_ipv4(mapped);
    }
    if let Some(embedded) = embedded_ipv4(address) {
        return is_blocked_ipv4(embedded);
    }
    if PUBLIC_IPV6_SPECIAL_USE_EXCEPTIONS
        .iter()
        .any(|(prefix, bits)| ipv6_in(address, *prefix, *bits))
    {
        return false;
    }
    if !IPV6_GLOBAL_UNICAST_SUBNETS
        .iter()
        .any(|(prefix, bits)| ipv6_in(address, *prefix, *bits))
    {
        return true;
    }
    NON_PUBLIC_IPV6_SUBNETS
        .iter()
        .any(|(prefix, bits)| ipv6_in(address, *prefix, *bits))
}

pub(super) fn is_public_ip(address: IpAddr) -> bool {
    match address {
        IpAddr::V4(address) => !is_blocked_ipv4(address),
        IpAddr::V6(address) => !is_blocked_ipv6(address),
    }
}

pub fn is_blocked_ip_literal(raw: &str) -> bool {
    match raw.parse::<IpAddr>() {
        Ok(address) => !is_public_ip(address),
        Err(_) => true,
    }
}
