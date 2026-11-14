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
