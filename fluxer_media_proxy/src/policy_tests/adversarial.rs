// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_hash::{self, AssetHash},
    disposition::{self, Decision, PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES},
    query::Query,
    test_fixtures::ADVERSARIAL_TEXT_INPUTS,
    upload_relay::token::token_from_query,
};

const RELAY_TOKEN_BYTES_MAX: usize = 16 * 1024;

#[test]
fn adversarial_text_never_escapes_the_asset_hash_digest_contract() {
    for text in ADVERSARIAL_TEXT_INPUTS {
        let parsed = AssetHash::parse(text);
        assert!(
            text.ends_with(parsed.digest()),
            "the digest is not a suffix of {text}"
        );
        assert_eq!(
            parsed.is_animated(),
            text.starts_with("a_"),
            "the animation flag disagrees with {text}"
        );
        assert_eq!(
            text.len() - parsed.digest().len(),
            if parsed.is_animated() { 2 } else { 0 },
            "the digest dropped bytes from {text}"
        );
        assert_eq!(parsed.digest(), asset_hash::strip_animation_prefix(text));
    }
}

#[test]
fn adversarial_query_text_yields_at_most_one_relay_token() {
    for text in ADVERSARIAL_TEXT_INPUTS {
        let query = Query::parse(text);
        for key in ["t", "token", ""] {
            let Ok(token) = token_from_query(query.get(key)) else {
                continue;
            };
            assert!(!token.is_empty(), "an empty token was accepted from {text}");
            assert!(
                !token.contains('&'),
                "a multi-parameter token was accepted from {text}"
            );
            assert!(
                token.len() <= RELAY_TOKEN_BYTES_MAX,
                "an unbounded token was accepted from {text}"
            );
        }
        if let Ok(token) = token_from_query(Some(text)) {
            assert!(!token.is_empty(), "an empty token was accepted from {text}");
            assert!(token.len() <= RELAY_TOKEN_BYTES_MAX);
        }
    }
}

#[test]
fn adversarial_filenames_never_break_the_content_disposition_header() {
    for text in ADVERSARIAL_TEXT_INPUTS {
        for (decision, directive) in [
            (Decision::Inline, "inline"),
            (Decision::Attachment, "attachment"),
        ] {
            let value = disposition::header(decision, Some(text))
                .expect("disposition header")
                .into_header_value();
            let rendered = value.to_str().expect("the disposition header is ascii");
            assert!(
                rendered.len() <= PUBLIC_MEDIA_MAX_HEADER_VALUE_BYTES,
                "unbounded disposition for {text}"
            );
            assert!(
                !rendered.contains(['\r', '\n', '\0']),
                "the disposition for {text} can split the header block"
            );
            assert!(
                rendered.starts_with(directive),
                "the disposition for {text} lost its directive"
            );
            if text.is_empty() {
                assert_eq!(rendered, directive);
                continue;
            }
            assert_eq!(
                rendered.matches("; filename=\"").count(),
                1,
                "the disposition for {text} repeated the filename parameter"
            );
            let quoted = rendered
                .strip_prefix(directive)
                .and_then(|rest| rest.strip_prefix("; filename=\""))
                .and_then(|rest| rest.split('"').next())
                .expect("a terminated quoted filename");
            assert!(
                !quoted.contains(['\\', '/']),
                "the ascii fallback for {text} kept a path or escape character"
            );
        }
    }
}
