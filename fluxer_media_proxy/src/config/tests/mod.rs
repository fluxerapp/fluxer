// SPDX-License-Identifier: AGPL-3.0-or-later

mod attachment_signature;

use super::*;
use base64::{Engine as _, engine::general_purpose};

fn base_env() -> Vec<(&'static str, &'static str)> {
    vec![("FLUXER_MEDIA_PROXY_SECRET_KEY", "secret")]
}

fn env_with<'a>(extra: &[(&'a str, &'a str)]) -> Vec<(&'a str, &'a str)> {
    let mut env: Vec<(&'a str, &'a str)> = base_env();
    env.extend_from_slice(extra);
    env
}

#[test]
fn debug_output_never_reveals_a_secret() {
    let relay_secret = general_purpose::STANDARD.encode(b"relay-secret-material-0123456789");
    let cfg = Config::load_from_iter([
        ("FLUXER_MEDIA_PROXY_SECRET_KEY", "signing-secret-material"),
        ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        (
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
            relay_secret.as_str(),
        ),
    ])
    .unwrap();

    let rendered = format!("{cfg:?}");
    assert!(!rendered.contains("signing-secret-material"));
    assert!(!rendered.contains("relay-secret-material"));
    assert_eq!(2, rendered.matches("[REDACTED]").count());
    assert_eq!("signing-secret-material", cfg.secret_key.expose());
}

#[test]
fn a_zero_socket_timeout_is_refused() {
    let err = Config::load_from_iter(env_with(&[(
        "FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS",
        "0",
    )]))
    .unwrap_err();
    assert!(
        err.to_string()
            .contains("FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS"),
        "{err}"
    );
}

#[test]
fn the_socket_timeout_keeps_its_configured_value() {
    let cfg = Config::load_from_iter(env_with(&[(
        "FLUXER_MEDIA_PROXY_SOCKET_IO_TIMEOUT_MS",
        "1",
    )]))
    .unwrap();
    assert_eq!(1, cfg.socket_io_timeout_ms.get());
}
