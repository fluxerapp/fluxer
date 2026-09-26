// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{base_env, env_with};
use crate::config::{Config, DeploymentMode, PolicyMode};

const MODE_KEY: &str = "FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE";
const SECRETS_KEY: &str = "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64";
const RELAY_SECRET_KEY: &str = "FLUXER_MEDIA_PROXY_UPLOAD_RELAY_SECRET_BASE64";
const FIRST: &str = "AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQE=";
const SECOND: &str = "AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgI=";
const TOO_SHORT: &str = "AwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAw==";
const OVERSIZED: &str =
    "BAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBA==";

fn secrets(cfg: &Config) -> Vec<Vec<u8>> {
    cfg.attachment_signature
        .secrets()
        .iter()
        .map(|secret| secret.to_vec())
        .collect()
}

fn enforce_with_secrets(raw: &str) -> anyhow::Result<Config> {
    Config::load_from_iter(env_with(&[(MODE_KEY, "enforce"), (SECRETS_KEY, raw)]))
}

#[test]
fn the_signature_policy_defaults_to_off_with_no_secrets() {
    let cfg = Config::load_from_iter(base_env()).unwrap();
    assert_eq!(PolicyMode::Off, cfg.attachment_signature.mode);
    assert_eq!(0, cfg.attachment_signature.secret_count());
    assert!(secrets(&cfg).is_empty());
}

#[test]
fn the_signature_mode_parses_every_variant_case_insensitively() {
    for raw in ["off", "OFF", " off "] {
        let cfg = Config::load_from_iter(env_with(&[(MODE_KEY, raw)])).unwrap();
        assert_eq!(PolicyMode::Off, cfg.attachment_signature.mode, "{raw:?}");
    }
    for (raw, expected) in [
        ("report", PolicyMode::Report),
        (" REPORT ", PolicyMode::Report),
        ("enforce", PolicyMode::Enforce),
        ("Enforce", PolicyMode::Enforce),
    ] {
        let cfg =
            Config::load_from_iter(env_with(&[(MODE_KEY, raw), (SECRETS_KEY, FIRST)])).unwrap();
        assert_eq!(expected, cfg.attachment_signature.mode, "{raw:?}");
        assert_eq!(vec![vec![1u8; 32]], secrets(&cfg), "{raw:?}");
    }
}

#[test]
fn rejects_an_unknown_signature_mode() {
    for raw in ["strict", "true", "1", "", "deny"] {
        let err =
            Config::load_from_iter(env_with(&[(MODE_KEY, raw), (SECRETS_KEY, FIRST)])).unwrap_err();
        assert_eq!(
            format!("{MODE_KEY} must be one of: off, report, enforce"),
            err.to_string(),
            "{raw:?}"
        );
    }
}

#[test]
fn the_cors_mode_error_still_names_its_own_variable() {
    let err = Config::load_from_iter(env_with(&[("FLUXER_MEDIA_PROXY_CORS_MODE", "strict")]))
        .unwrap_err();
    assert_eq!(
        "FLUXER_MEDIA_PROXY_CORS_MODE must be one of: off, report, enforce",
        err.to_string()
    );
}

#[test]
fn report_and_enforce_require_a_secret() {
    for mode in ["report", "enforce"] {
        let unset = Config::load_from_iter(env_with(&[(MODE_KEY, mode)])).unwrap_err();
        let empty =
            Config::load_from_iter(env_with(&[(MODE_KEY, mode), (SECRETS_KEY, "")])).unwrap_err();
        let separators_only =
            Config::load_from_iter(env_with(&[(MODE_KEY, mode), (SECRETS_KEY, " , , ")]))
                .unwrap_err();
        for err in [unset, empty, separators_only] {
            assert_eq!(
                format!("{SECRETS_KEY} is required when {MODE_KEY} is report or enforce"),
                err.to_string(),
                "{mode}"
            );
        }
    }
}

#[test]
fn rejects_an_entry_that_is_not_standard_base64() {
    for raw in [
        "not base64".to_owned(),
        "AQEB_AQE".to_owned(),
        format!("{FIRST},not base64"),
    ] {
        let err = enforce_with_secrets(&raw).unwrap_err();
        assert_eq!(
            format!("{SECRETS_KEY} entries must be standard base64"),
            err.to_string(),
            "{raw:?}"
        );
    }
}

#[test]
fn rejects_an_entry_shorter_than_thirty_two_bytes() {
    for raw in [TOO_SHORT, "AQ==", "AQEB"] {
        let err = enforce_with_secrets(raw).unwrap_err();
        assert_eq!(
            format!("{SECRETS_KEY} entries must decode to at least 32 bytes"),
            err.to_string(),
            "{raw:?}"
        );
    }
}

#[test]
fn an_error_never_echoes_an_entry() {
    for raw in [TOO_SHORT, "aHVudGVyMg=="] {
        let err = enforce_with_secrets(raw).unwrap_err().to_string();
        assert!(!err.contains(raw), "{err}");
    }
}

#[test]
fn empty_entries_are_skipped_and_duplicates_are_dropped() {
    let cfg = enforce_with_secrets(&format!(" {FIRST} , ,{SECOND},{FIRST}, ")).unwrap();
    assert_eq!(vec![vec![1u8; 32], vec![2u8; 32]], secrets(&cfg));
    assert_eq!(2, cfg.attachment_signature.secret_count());
}

#[test]
fn the_first_entry_stays_first_and_a_longer_key_is_accepted() {
    let cfg = enforce_with_secrets(&format!("{SECOND},{OVERSIZED}")).unwrap();
    assert_eq!(vec![vec![2u8; 32], vec![4u8; 64]], secrets(&cfg));
}

#[test]
fn off_mode_keeps_a_staged_secret_and_still_rejects_an_invalid_one() {
    let cfg = Config::load_from_iter(env_with(&[(MODE_KEY, "off"), (SECRETS_KEY, FIRST)])).unwrap();
    assert_eq!(PolicyMode::Off, cfg.attachment_signature.mode);
    assert_eq!(vec![vec![1u8; 32]], secrets(&cfg));

    let cfg = Config::load_from_iter(env_with(&[(SECRETS_KEY, FIRST)])).unwrap();
    assert_eq!(PolicyMode::Off, cfg.attachment_signature.mode);
    assert_eq!(vec![vec![1u8; 32]], secrets(&cfg));

    let err = Config::load_from_iter(env_with(&[(MODE_KEY, "off"), (SECRETS_KEY, TOO_SHORT)]))
        .unwrap_err();
    assert_eq!(
        format!("{SECRETS_KEY} entries must decode to at least 32 bytes"),
        err.to_string()
    );
}

#[test]
fn static_mode_ignores_both_signature_keys() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_MODE", "static"),
        (MODE_KEY, "enforce"),
        (SECRETS_KEY, "not base64"),
    ]))
    .unwrap();
    assert_eq!(DeploymentMode::Static, cfg.mode);
    assert_eq!(PolicyMode::Off, cfg.attachment_signature.mode);
    assert!(secrets(&cfg).is_empty());
}

#[test]
fn relay_mode_ignores_both_signature_keys() {
    for (mode, raw) in [
        ("enforce", "not base64"),
        ("enforce", TOO_SHORT),
        ("strict", FIRST),
    ] {
        let cfg = Config::load_from_iter(env_with(&[
            ("FLUXER_MEDIA_PROXY_MODE", "relay"),
            (RELAY_SECRET_KEY, FIRST),
            (MODE_KEY, mode),
            (SECRETS_KEY, raw),
        ]))
        .unwrap();
        assert_eq!(DeploymentMode::Relay, cfg.mode);
        assert_eq!(
            PolicyMode::Off,
            cfg.attachment_signature.mode,
            "{mode} {raw}"
        );
        assert!(secrets(&cfg).is_empty(), "{mode} {raw}");
    }
}

#[test]
fn upload_mode_still_applies_both_signature_keys() {
    let cfg = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        (RELAY_SECRET_KEY, FIRST),
        (MODE_KEY, "enforce"),
        (SECRETS_KEY, SECOND),
    ]))
    .unwrap();
    assert_eq!(DeploymentMode::Upload, cfg.mode);
    assert_eq!(PolicyMode::Enforce, cfg.attachment_signature.mode);
    assert_eq!(vec![vec![2u8; 32]], secrets(&cfg));

    let err = Config::load_from_iter(env_with(&[
        ("FLUXER_MEDIA_PROXY_MODE", "upload"),
        (RELAY_SECRET_KEY, FIRST),
        (MODE_KEY, "enforce"),
    ]))
    .unwrap_err();
    assert_eq!(
        format!("{SECRETS_KEY} is required when {MODE_KEY} is report or enforce"),
        err.to_string()
    );
}

#[test]
fn debug_output_never_reveals_a_signing_secret() {
    let cfg = enforce_with_secrets(&format!("{FIRST},{SECOND}")).unwrap();
    let rendered = format!("{cfg:?}");
    assert!(!rendered.contains(FIRST));
    assert!(!rendered.contains(SECOND));
    assert!(!rendered.contains("\u{1}\u{1}"));
    assert_eq!(4, rendered.matches("[REDACTED]").count());
}
