// SPDX-License-Identifier: AGPL-3.0-or-later

use super::env_with;
use crate::config::Config;

const MODE_KEY: &str = "FLUXER_MEDIA_PROXY_ATTACHMENT_SIGNATURE_MODE";
const SECRETS_KEY: &str = "FLUXER_MEDIA_PROXY_ATTACHMENT_URL_SECRETS_BASE64";
const FIRST: &str = "AQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQEBAQE=";
const SECOND: &str = "AgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgI=";
const TOO_SHORT: &str = "AwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAwMDAw==";

fn enforce_with_secrets(raw: &str) -> anyhow::Result<Config> {
    Config::load_from_iter(env_with(&[(MODE_KEY, "enforce"), (SECRETS_KEY, raw)]))
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
fn debug_output_never_reveals_a_signing_secret() {
    let cfg = enforce_with_secrets(&format!("{FIRST},{SECOND}")).unwrap();
    let rendered = format!("{cfg:?}");
    assert!(!rendered.contains(FIRST));
    assert!(!rendered.contains(SECOND));
    assert!(!rendered.contains("\u{1}\u{1}"));
    assert_eq!(4, rendered.matches("[REDACTED]").count());
}
