// SPDX-License-Identifier: AGPL-3.0-or-later

use super::env_with;
use crate::config::Config;

const ENDPOINT: &str = "https://chat.example.com/media";

#[test]
fn a_non_default_public_port_reaches_the_public_endpoint() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT", ENDPOINT),
        ("FLUXER_BASE_DOMAIN", "chat.example.com"),
        ("FLUXER_PUBLIC_PORT", "29080"),
    ]))
    .expect("config loads");
    assert_eq!(
        Some("https://chat.example.com:29080/media".to_owned()),
        cfg.public_endpoint
    );
}

#[test]
fn the_public_origin_supplies_the_port() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT", ENDPOINT),
        ("FLUXER_PUBLIC_ORIGIN", "https://chat.example.com:29080"),
        ("FLUXER_BASE_DOMAIN", "chat.example.com"),
        ("FLUXER_PUBLIC_PORT", "443"),
    ]))
    .expect("config loads");
    assert_eq!(
        Some("https://chat.example.com:29080/media".to_owned()),
        cfg.public_endpoint
    );
}

#[test]
fn an_unrelated_host_keeps_its_endpoint() {
    let cfg = Config::load_from_iter(env_with(&[
        (
            "FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT",
            "https://cdn.other.example/media",
        ),
        ("FLUXER_BASE_DOMAIN", "chat.example.com"),
        ("FLUXER_PUBLIC_PORT", "29080"),
    ]))
    .expect("config loads");
    assert_eq!(
        Some("https://cdn.other.example/media".to_owned()),
        cfg.public_endpoint
    );
}

#[test]
fn a_default_public_port_keeps_the_endpoint_portless() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT", ENDPOINT),
        ("FLUXER_BASE_DOMAIN", "chat.example.com"),
        ("FLUXER_PUBLIC_PORT", "443"),
    ]))
    .expect("config loads");
    assert_eq!(Some(ENDPOINT.to_owned()), cfg.public_endpoint);
}

#[test]
fn a_malformed_public_port_is_refused() {
    let error = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_PUBLIC_ENDPOINT", ENDPOINT),
        ("FLUXER_BASE_DOMAIN", "chat.example.com"),
        ("FLUXER_PUBLIC_PORT", "not-a-port"),
    ]))
    .expect_err("a malformed port is refused");
    assert!(error.to_string().contains("FLUXER_PUBLIC_PORT"));
}
