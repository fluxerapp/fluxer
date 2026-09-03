// SPDX-License-Identifier: AGPL-3.0-or-later

use base64::prelude::*;
use hmac::{Hmac, KeyInit, Mac};
use sha2::Sha256;

type HmacSha256 = Hmac<Sha256>;

pub fn create_signature(input: &str, secret: &[u8]) -> String {
    let mut mac = HmacSha256::new_from_slice(secret).expect("HMAC accepts any key length");
    mac.update(input.as_bytes());
    BASE64_URL_SAFE_NO_PAD.encode(mac.finalize().into_bytes())
}

pub fn verify_signature(input: &str, provided: &str, secret: &[u8]) -> bool {
    let expected = create_signature(input, secret);
    if expected.len() != provided.len() {
        return false;
    }
    expected
        .bytes()
        .zip(provided.bytes())
        .fold(0u8, |diff, (a, b)| diff | (a ^ b))
        == 0
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_fixtures::ADVERSARIAL_TEXT_INPUTS;
    use proptest::prelude::*;

    const HMAC_SHA256_BASE64URL_LEN: usize = 43;

    #[test]
    fn signature_matches_node_hmac_base64url_behavior() {
        let sig = create_signature("v2/aHR0cHM6Ly9leGFtcGxlLmNvbS8", b"secret");
        assert_eq!("jkGeYYCiqJ67eS5T6lWJ4eQ77pwIaf6yoXXF5LncsHY", sig);
        assert!(verify_signature(
            "v2/aHR0cHM6Ly9leGFtcGxlLmNvbS8",
            &sig,
            b"secret"
        ));
    }

    #[test]
    fn verify_rejects_tampered_signature() {
        let sig = create_signature("input", b"secret");
        let mut tampered = sig.clone().into_bytes();
        tampered[0] = if tampered[0] == b'A' { b'B' } else { b'A' };
        assert!(!verify_signature(
            "input",
            std::str::from_utf8(&tampered).unwrap(),
            b"secret"
        ));
    }

    #[test]
    fn verify_rejects_wrong_secret() {
        let sig = create_signature("input", b"secret");
        assert!(!verify_signature("input", &sig, b"different"));
    }

    #[test]
    fn verify_rejects_wrong_input() {
        let sig = create_signature("input-a", b"secret");
        assert!(!verify_signature("input-b", &sig, b"secret"));
    }

    #[test]
    fn verify_rejects_different_length_signature_without_panic() {
        assert!(!verify_signature("input", "short", b"secret"));
        assert!(!verify_signature(
            "input",
            "definitely-too-long-of-a-signature-for-hmac-sha256-base64url",
            b"secret"
        ));
    }

    #[test]
    fn signature_is_deterministic() {
        assert_eq!(
            create_signature("input", b"secret"),
            create_signature("input", b"secret")
        );
    }

    #[test]
    fn adversarial_text_signs_and_verifies_without_panicking() {
        for input in ADVERSARIAL_TEXT_INPUTS {
            let signature = create_signature(input, b"secret");
            assert_eq!(HMAC_SHA256_BASE64URL_LEN, signature.len());
            assert!(verify_signature(input, &signature, b"secret"));
            assert!(!verify_signature(input, &signature, b"another-secret"));
            assert!(!verify_signature(input, &signature, b""));
        }
    }

    #[test]
    fn an_empty_provided_signature_is_rejected_by_the_length_check() {
        for input in ADVERSARIAL_TEXT_INPUTS {
            assert!(!verify_signature(input, "", b"secret"));
            assert!(!verify_signature(input, " ", b"secret"));
            let truncated = &create_signature(input, b"secret")[..HMAC_SHA256_BASE64URL_LEN - 1];
            assert!(!verify_signature(input, truncated, b"secret"));
        }
    }

    #[test]
    fn a_signature_from_another_adversarial_input_never_verifies() {
        for input in ADVERSARIAL_TEXT_INPUTS {
            let signature = create_signature(input, b"secret");
            for other in ADVERSARIAL_TEXT_INPUTS {
                assert_eq!(
                    input == other,
                    verify_signature(other, &signature, b"secret"),
                    "{input:?} against {other:?}"
                );
            }
        }
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 256,
            failure_persistence: None,
            ..ProptestConfig::default()
        })]

        #[test]
        fn only_the_signed_input_and_its_own_secret_verify(
            input in ".{0,96}",
            other in ".{0,96}",
            secret in prop::collection::vec(1u8..=u8::MAX, 0..64),
            other_secret in prop::collection::vec(1u8..=u8::MAX, 0..64),
        ) {
            let signature = create_signature(&input, &secret);
            prop_assert_eq!(HMAC_SHA256_BASE64URL_LEN, signature.len());
            prop_assert!(verify_signature(&input, &signature, &secret));
            prop_assert_eq!(input == other, verify_signature(&other, &signature, &secret));
            prop_assert_eq!(
                secret == other_secret,
                verify_signature(&input, &signature, &other_secret)
            );
        }
    }
}
