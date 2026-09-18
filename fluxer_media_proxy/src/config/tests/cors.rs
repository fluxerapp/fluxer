// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{allowed_origins, base_env, env_with};
use crate::config::{Config, DeploymentMode, PolicyMode};
use base64::{Engine as _, engine::general_purpose};

const ORIGINS_KEY: &str = "FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS";

fn enforce_with_origins(raw: &'static str) -> anyhow::Result<Config> {
    Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
        (ORIGINS_KEY, raw),
    ]))
}

#[test]
fn cors_defaults_to_off_with_an_empty_allowlist() {
    let cfg = Config::load_from_iter(base_env()).unwrap();
    assert_eq!(PolicyMode::Off, cfg.cors.mode);
    assert!(cfg.cors.allowed_origins.is_empty());
}

#[test]
fn cors_mode_parses_every_variant_case_insensitively() {
    for (raw, expected) in [("off", PolicyMode::Off), ("OFF", PolicyMode::Off)] {
        let cfg =
            Config::load_from_iter(env_with(&[("FLUXER_MEDIA_PROXY_CORS_MODE", raw)])).unwrap();
        assert_eq!(expected, cfg.cors.mode, "{raw:?}");
        assert!(cfg.cors.allowed_origins.is_empty());
    }

    for (raw, expected) in [
        (" report ", PolicyMode::Report),
        ("Enforce", PolicyMode::Enforce),
    ] {
        let cfg = Config::load_from_iter(env_with(&[
            ("FLUXER_MEDIA_PROXY_CORS_MODE", raw),
            (ORIGINS_KEY, "https://web.fluxer.app"),
        ]))
        .unwrap();
        assert_eq!(expected, cfg.cors.mode, "{raw:?}");
        assert_eq!(vec!["https://web.fluxer.app"], allowed_origins(&cfg));
    }
}

#[test]
fn rejects_an_unknown_cors_mode() {
    for raw in ["strict", "true", "1", ""] {
        let err = Config::load_from_iter(env_with(&[
            ("FLUXER_MEDIA_PROXY_CORS_MODE", raw),
            (ORIGINS_KEY, "https://web.fluxer.app"),
        ]))
        .unwrap_err();
        assert_eq!(
            "FLUXER_MEDIA_PROXY_CORS_MODE must be one of: off, report, enforce",
            err.to_string(),
            "{raw:?}"
        );
    }
}

#[test]
fn allowed_origins_are_normalised_to_their_browser_serialisation() {
    for (raw, expected) in [
        ("https://Web.Fluxer.App", vec!["https://web.fluxer.app"]),
        ("https://web.fluxer.app/", vec!["https://web.fluxer.app"]),
        ("https://web.fluxer.app:443", vec!["https://web.fluxer.app"]),
        ("http://localhost:8088", vec!["http://localhost:8088"]),
        ("http://127.0.0.1:8088", vec!["http://127.0.0.1:8088"]),
        ("http://[::1]:8088", vec!["http://[::1]:8088"]),
        (
            "https://xn--bcher-kva.example",
            vec!["https://xn--bcher-kva.example"],
        ),
        (
            " https://a.example , ,https://b.example,",
            vec!["https://a.example", "https://b.example"],
        ),
        (
            "https://a.example,https://A.example",
            vec!["https://a.example"],
        ),
    ] {
        let cfg = enforce_with_origins(raw).unwrap();
        assert_eq!(expected, allowed_origins(&cfg), "{raw:?}");
    }
}

#[test]
fn rejects_an_allowed_origin_that_is_not_a_bare_http_origin() {
    for entry in [
        "*",
        "null",
        "web.fluxer.app",
        "ftp://web.fluxer.app",
        "file:///tmp/x",
        "fluxer-app://app",
        "https://user@web.fluxer.app",
        "https://user:pw@web.fluxer.app",
        "https://web.fluxer.app/app",
        "https://web.fluxer.app?x=1",
        "https://web.fluxer.app#f",
        "https://*.fluxer.app",
        "https://{web}.fluxer.app",
        "https://*",
        "https://web.*.fluxer.app",
        "https://~",
        "https://$",
        "https://a..b",
        "https://.fluxer.app",
        "https://web.fluxer.app.",
        "https://bücher.example",
        "https://w\u{435}b.fluxer.app",
    ] {
        let err = enforce_with_origins(entry).unwrap_err();
        assert_eq!(
            format!("{ORIGINS_KEY} contains an invalid origin: {entry}"),
            err.to_string()
        );
    }

    let err = enforce_with_origins("https://{web,canary}.fluxer.app").unwrap_err();
    assert_eq!(
        format!("{ORIGINS_KEY} contains an invalid origin: https://{{web"),
        err.to_string()
    );
}

#[test]
fn an_invalid_entry_fails_even_beside_a_valid_one() {
    let err =
        enforce_with_origins("https://web.fluxer.app, https://web.fluxer.app/app").unwrap_err();
    assert_eq!(
        format!("{ORIGINS_KEY} contains an invalid origin: https://web.fluxer.app/app"),
        err.to_string()
    );
}

#[test]
fn report_and_enforce_require_an_allowed_origin() {
    for mode in ["report", "enforce"] {
        let unset = Config::load_from_iter(env_with(&[("FLUXER_MEDIA_PROXY_CORS_MODE", mode)]))
            .unwrap_err();
        let empty = Config::load_from_iter(env_with(&[
            ("FLUXER_MEDIA_PROXY_CORS_MODE", mode),
            (ORIGINS_KEY, ""),
        ]))
        .unwrap_err();
        let separators_only = Config::load_from_iter(env_with(&[
            ("FLUXER_MEDIA_PROXY_CORS_MODE", mode),
            (ORIGINS_KEY, " , "),
        ]))
        .unwrap_err();
        for err in [unset, empty, separators_only] {
            assert_eq!(
                "FLUXER_MEDIA_PROXY_CORS_ALLOWED_ORIGINS is required when FLUXER_MEDIA_PROXY_CORS_MODE is report or enforce",
                err.to_string(),
                "{mode}"
            );
        }
    }
}

#[test]
fn off_mode_keeps_a_staged_allowlist_and_still_rejects_an_invalid_one() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "off"),
        (
            ORIGINS_KEY,
            "https://web.fluxer.app,https://web.canary.fluxer.app",
        ),
    ]))
    .unwrap();
    assert_eq!(PolicyMode::Off, cfg.cors.mode);
    assert_eq!(
        vec!["https://web.fluxer.app", "https://web.canary.fluxer.app"],
        allowed_origins(&cfg)
    );

    let cfg = Config::load_from_iter(env_with(&[(ORIGINS_KEY, "https://web.fluxer.app")])).unwrap();
    assert_eq!(PolicyMode::Off, cfg.cors.mode);
    assert_eq!(vec!["https://web.fluxer.app"], allowed_origins(&cfg));

    let err = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "off"),
        (ORIGINS_KEY, "not an origin"),
    ]))
    .unwrap_err();
    assert_eq!(
        format!("{ORIGINS_KEY} contains an invalid origin: not an origin"),
        err.to_string()
    );
}

#[test]
fn static_mode_ignores_both_cors_keys() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_MODE", "static"),
        ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
        (ORIGINS_KEY, "not an origin"),
    ]))
    .unwrap();
    assert_eq!(DeploymentMode::Static, cfg.mode);
    assert_eq!(PolicyMode::Off, cfg.cors.mode);
    assert!(cfg.cors.allowed_origins.is_empty());
}

#[test]
fn relay_mode_ignores_both_cors_keys() {
    let relay_secret = general_purpose::STANDARD.encode([5u8; 32]);
    for (cors_mode, origins) in [
        ("enforce", "not an origin"),
        ("enforce", "https://*.fluxer.app"),
        ("strict", "https://web.fluxer.app"),
    ] {
        let mut env: Vec<(&str, &str)> = env_with(&[
            ("FLUXER_MEDIA_PROXY_MODE", "relay"),
            ("FLUXER_MEDIA_PROXY_CORS_MODE", cors_mode),
            (ORIGINS_KEY, origins),
        ]);
        env.push((
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
            relay_secret.as_str(),
        ));
        let cfg = Config::load_from_iter(env).unwrap();
        assert_eq!(DeploymentMode::Relay, cfg.mode);
        assert_eq!(PolicyMode::Off, cfg.cors.mode, "{cors_mode} {origins}");
        assert!(cfg.cors.allowed_origins.is_empty(), "{cors_mode} {origins}");
    }
}

#[test]
fn upload_mode_still_applies_the_cors_keys() {
    let relay_secret = general_purpose::STANDARD.encode([5u8; 32]);
    let upload_env = |origins: &'static str| {
        let mut env: Vec<(&str, &str)> = env_with(&[
            ("FLUXER_MEDIA_PROXY_MODE", "upload"),
            ("FLUXER_MEDIA_PROXY_CORS_MODE", "enforce"),
            (ORIGINS_KEY, origins),
        ]);
        env.push((
            "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64",
            relay_secret.as_str(),
        ));
        env
    };

    let cfg = Config::load_from_iter(upload_env("https://web.fluxer.app")).unwrap();
    assert_eq!(DeploymentMode::Upload, cfg.mode);
    assert_eq!(PolicyMode::Enforce, cfg.cors.mode);
    assert_eq!(vec!["https://web.fluxer.app"], allowed_origins(&cfg));

    let err = Config::load_from_iter(upload_env("not an origin")).unwrap_err();
    assert_eq!(
        format!("{ORIGINS_KEY} contains an invalid origin: not an origin"),
        err.to_string()
    );
}
