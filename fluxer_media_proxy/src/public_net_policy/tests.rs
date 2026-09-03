// SPDX-License-Identifier: AGPL-3.0-or-later

use super::ip_tables::is_public_ip;
use super::resolver::{MAX_PUBLIC_DNS_ADDRESSES, ResolveError, screen_resolved_addresses};
use super::*;
use std::net::SocketAddr;

fn public(literal: &str) -> bool {
    is_public_ip(literal.parse().expect("test address literal parses"))
}

fn socket_addresses(literal: &str, count: usize) -> Vec<SocketAddr> {
    let address = literal.parse().expect("test address literal parses");
    (0..count).map(|_| SocketAddr::new(address, 443)).collect()
}

#[test]
fn blocks_private_and_special_ip_literals() {
    assert!(is_blocked_ip_literal("127.0.0.1"));
    assert!(is_blocked_ip_literal("10.1.2.3"));
    assert!(is_blocked_ip_literal("::1"));
    assert!(is_blocked_ip_literal("::ffff:192.168.1.1"));
    assert!(!is_blocked_ip_literal("8.8.8.8"));
    assert!(!is_blocked_ip_literal("2606:4700:4700::1111"));
}

#[test]
fn validates_public_host_syntax() {
    assert!(is_valid_public_hostname("example.com"));
    assert!(is_valid_public_hostname("xn--bcher-kva.example"));
    assert!(!is_valid_public_hostname("localhost"));
    assert!(!is_valid_public_hostname("example"));
    assert!(!is_valid_public_hostname("bad_name.example"));
    assert!(!is_valid_public_hostname("example.123"));
}

#[test]
fn resolves_relative_redirects() {
    assert_eq!(
        "https://example.com/a/d?y=2",
        resolve_redirect("https://example.com/a/b/c?x=1", "../d?y=2#ignored").unwrap()
    );
    assert_eq!(
        "https://example.com/z",
        resolve_redirect("https://example.com/a/b/c", "/z").unwrap()
    );
}

#[test]
fn blocks_every_documented_ipv4_ssrf_range() {
    for ip in [
        "0.0.0.0",
        "10.0.0.1",
        "100.64.0.1",
        "127.0.0.1",
        "169.254.169.254",
        "172.16.5.5",
        "192.0.0.1",
        "192.0.2.5",
        "192.88.99.5",
        "192.168.1.1",
        "198.18.0.1",
        "198.51.100.5",
        "203.0.113.5",
        "224.0.0.1",
        "240.0.0.1",
        "255.255.255.255",
    ] {
        assert!(is_blocked_ip_literal(ip), "{ip}");
    }
}

#[test]
fn blocks_every_documented_ipv6_ssrf_range() {
    for ip in [
        "::",
        "::1",
        "2001:db8::1",
        "fc00::1",
        "fd00::1",
        "fe80::1",
        "ff00::1",
        "64:ff9b::a9fe:a9fe",
        "::7f00:1",
        "2002:a9fe:a9fe::",
    ] {
        assert!(is_blocked_ip_literal(ip), "{ip}");
    }
}

#[test]
fn rejects_urls_with_userinfo() {
    assert_eq!(
        Err(Error::BlockedUrl),
        validate_url("https://user:pass@example.com/")
    );
}

#[test]
fn rejects_non_http_s_schemes() {
    assert_eq!(Err(Error::BlockedUrl), validate_url("file:///etc/passwd"));
    assert_eq!(
        Err(Error::BlockedUrl),
        validate_url("gopher://example.com/")
    );
}

#[test]
fn redirect_that_returns_to_same_url_is_allowed() {
    assert_eq!(
        "https://example.com/path",
        resolve_redirect("https://example.com/path", "/path").unwrap()
    );
}

#[test]
fn redirect_with_dot_dot_cannot_escape_host() {
    let r = resolve_redirect("https://example.com/a", "../../../etc").unwrap();
    assert!(r.starts_with("https://example.com/"));
}

#[test]
fn logged_urls_keep_only_scheme_host_and_port() {
    assert_eq!(
        "https://cdn.example.com/[redacted]",
        external_url_for_log("https://agent:hunter2@cdn.example.com/rooms/private?token=abc#f")
    );
    assert_eq!(
        "http://cdn.example.com:8080/[redacted]",
        external_url_for_log("http://cdn.example.com:8080/a/b/c")
    );
    assert_eq!("[invalid-url]", external_url_for_log("not a url"));
    assert_eq!("[invalid-url]", external_url_for_log(""));
}

#[test]
fn ipv6_outside_global_unicast_is_blocked_by_default() {
    for literal in [
        "100::1",
        "1fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",
        "2001::1",
        "2002::1",
        "3fff::1",
        "4000::",
        "5f00::1",
        "fec0::1",
        "ff02::1",
    ] {
        assert!(!public(literal), "{literal}");
    }
    for literal in [
        "2000::",
        "2001:200::1",
        "2001:db7:ffff:ffff:ffff:ffff:ffff:ffff",
        "2001:db9::1",
        "2003::1",
        "2606:4700:4700::1111",
        "2a00:1450:4001:800::200e",
        "3fff:1000::1",
    ] {
        assert!(public(literal), "{literal}");
    }
}

#[test]
fn globally_reachable_special_use_addresses_stay_reachable() {
    for literal in ["192.0.0.9", "192.0.0.10", "2001:1::1", "2001:20::1"] {
        assert!(public(literal), "{literal}");
    }
    for literal in ["192.0.0.8", "192.0.0.11", "2001:1::4", "2001:40::1"] {
        assert!(!public(literal), "{literal}");
    }
}

#[test]
fn ipv4_written_in_ipv6_notation_is_revalidated_against_the_v4_table() {
    for literal in [
        "::ffff:127.0.0.1",
        "::ffff:10.0.0.1",
        "::ffff:169.254.169.254",
        "::93.184.216.34",
        "64:ff9b::10.0.0.1",
        "64:ff9b::169.254.169.254",
        "2002:5db8:d822::",
    ] {
        assert!(!public(literal), "{literal}");
    }
    for literal in ["::ffff:93.184.216.34", "64:ff9b::93.184.216.34"] {
        assert!(public(literal), "{literal}");
    }
}

#[test]
fn unparseable_ip_literals_fail_closed() {
    assert!(is_blocked_ip_literal(""));
    assert!(is_blocked_ip_literal("example.com"));
    assert!(is_blocked_ip_literal("127.0.0.1:80"));
    assert!(is_blocked_ip_literal("0x7f000001"));
}

#[test]
fn public_urls_are_restricted_to_the_standard_web_ports() {
    assert_eq!(Ok(()), validate_url("https://example.com/a"));
    assert_eq!(Ok(()), validate_url("https://example.com:443/a"));
    assert_eq!(Ok(()), validate_url("http://example.com:80/a"));
    for url in [
        "https://example.com:8080/a",
        "http://example.com:8080/a",
        "https://example.com:22/a",
        "https://[2606:4700:4700::1111]:8443/a",
    ] {
        assert_eq!(Err(Error::BlockedUrl), validate_url(url), "{url}");
    }
    for url in ["https://example.com:0/a", "https://example.com:99999/a"] {
        assert_eq!(Err(Error::InvalidUrl), validate_url(url), "{url}");
    }
}

#[test]
fn urls_carrying_a_fragment_are_rejected() {
    assert_eq!(
        Err(Error::BlockedUrl),
        validate_url("https://example.com/a#section")
    );
    assert_eq!(
        Err(Error::BlockedUrl),
        validate_url("https://example.com#section")
    );
    assert_eq!(Ok(()), validate_url("https://example.com/a"));
    let next = resolve_redirect("https://example.com/a/b", "/next#anchor").unwrap();
    assert_eq!("https://example.com/next", next);
    assert_eq!(Ok(()), validate_url(&next));
}

#[test]
fn url_validation_accepts_public_syntax_without_resolving_dns() {
    assert_eq!(
        Ok(()),
        validate_url("https://this-host-does-not-exist.invalid/a")
    );
    assert_eq!(
        Err(Error::BlockedUrl),
        validate_url("https://169.254.169.254/latest/meta-data/")
    );
    assert_eq!(Err(Error::BlockedUrl), validate_url("http://localhost/a"));
}

#[test]
fn resolved_address_sets_are_bounded_and_fail_closed() {
    assert_eq!(
        Err(ResolveError::HostResolvedToNoAddress),
        screen_resolved_addresses(std::iter::empty())
    );
    let at_limit = socket_addresses("93.184.216.34", MAX_PUBLIC_DNS_ADDRESSES);
    assert_eq!(
        Ok(at_limit.clone()),
        screen_resolved_addresses(at_limit.into_iter())
    );
    let over_limit = socket_addresses("93.184.216.34", MAX_PUBLIC_DNS_ADDRESSES + 1);
    assert_eq!(
        Err(ResolveError::TooManyAddresses),
        screen_resolved_addresses(over_limit.into_iter())
    );
    let mut mixed = socket_addresses("93.184.216.34", 2);
    mixed.extend(socket_addresses("169.254.169.254", 1));
    assert_eq!(
        Err(ResolveError::BlockedAddress),
        screen_resolved_addresses(mixed.into_iter())
    );
}

#[test]
fn a_resolver_rejection_is_recognisable_however_deeply_the_transport_wraps_it() {
    assert!(is_pinned_dns_failure(&ResolveError::BlockedAddress));
    assert!(is_pinned_dns_failure(&ResolveError::LookupFailed));
    let wrapped = anyhow::Error::new(ResolveError::HostResolvedToNoAddress).context("dns error");
    assert!(is_pinned_dns_failure(wrapped.as_ref()));
    assert!(!is_pinned_dns_failure(&std::io::Error::other(
        "tcp connect error"
    )));
}
