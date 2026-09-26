// SPDX-License-Identifier: AGPL-3.0-or-later

pub use fluxer_common::external_media_path::{
    ExternalPathError, build_external_media_proxy_path, build_opaque_external_media_proxy_path,
    percent_decode, percent_decode_string, reconstruct_original_url,
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{public_net_policy, signing, test_fixtures::ADVERSARIAL_TEXT_INPUTS};
    use proptest::prelude::*;

    const SECRET: &[u8] = b"external-media-proxy-secret";

    fn without_fragment(url: &str) -> &str {
        url.split_once('#').map_or(url, |(head, _)| head)
    }

    fn signed_round_trip(url: &str) -> String {
        let path = build_external_media_proxy_path(url).expect("the external path builds");
        let signature = signing::create_signature(&path, SECRET);
        assert!(
            signing::verify_signature(&path, &signature, SECRET),
            "{url}"
        );
        assert!(
            !signing::verify_signature(&path, &signature, b"other-secret"),
            "{url}"
        );
        reconstruct_original_url(&path).expect("the original url reconstructs")
    }

    fn adversarial_urls(text: &str) -> [String; 4] {
        [
            format!("https://example.com/{text}"),
            format!("https://example.com/media/{text}/photo.webp"),
            format!("http://example.com:8443/x?v={text}"),
            format!("https://example.com/{text}/{text}?{text}=1"),
        ]
    }

    #[test]
    fn adversarial_text_round_trips_through_the_signed_legacy_path() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            for url in adversarial_urls(text) {
                assert_eq!(without_fragment(&url), signed_round_trip(&url));
            }
        }
    }

    #[test]
    fn adversarial_text_round_trips_through_the_signed_opaque_path() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            for url in adversarial_urls(text) {
                let path = build_opaque_external_media_proxy_path(&url);
                let signature = signing::create_signature(&path, SECRET);
                assert!(
                    signing::verify_signature(&path, &signature, SECRET),
                    "{url}"
                );
                assert_eq!(
                    url,
                    reconstruct_original_url(&path).expect("the opaque path reconstructs")
                );
            }
        }
    }

    #[test]
    fn a_signature_computed_over_a_different_path_never_verifies() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            let path = build_external_media_proxy_path(&format!("https://example.com/{text}"))
                .expect("the external path builds");
            for other in [
                format!("https://example.com/other/{text}"),
                format!("https://example.org/{text}"),
                format!("http://example.com/{text}"),
            ] {
                let other_path =
                    build_external_media_proxy_path(&other).expect("the external path builds");
                assert_ne!(path, other_path, "{other}");
                let signature = signing::create_signature(&other_path, SECRET);
                assert!(
                    !signing::verify_signature(&path, &signature, SECRET),
                    "{other}"
                );
            }
        }
    }

    #[test]
    fn adversarial_text_is_never_a_usable_external_path_on_its_own() {
        for text in ADVERSARIAL_TEXT_INPUTS {
            assert_eq!(
                Err(ExternalPathError::InvalidExternalPath),
                build_external_media_proxy_path(text)
            );
            assert!(reconstruct_original_url(text).is_err(), "{text:?}");
        }
    }

    #[test]
    fn a_signed_path_never_decodes_into_a_url_the_fetch_policy_refuses() {
        let credentialed = "https://reader:secret@cdn.example.com/i.png";
        assert_eq!(
            Err(public_net_policy::Error::BlockedUrl),
            public_net_policy::parse_url(credentialed)
        );

        let path = build_external_media_proxy_path(credentialed).expect("path builds");
        let decoded = reconstruct_original_url(&path).expect("path decodes");

        assert_eq!("https://cdn.example.com/i.png", decoded);
        assert!(public_net_policy::parse_url(&decoded).is_ok());
    }

    #[test]
    fn a_fragment_never_splits_the_path_for_bytes_the_fetch_would_not_see() {
        assert_eq!(
            build_external_media_proxy_path("https://cdn.example.com/i.png").expect("path builds"),
            build_external_media_proxy_path("https://cdn.example.com/i.png#one")
                .expect("path builds")
        );
        assert_eq!(
            public_net_policy::parse_url("https://cdn.example.com/i.png")
                .expect("plain url parses")
                .path_query,
            public_net_policy::parse_url("https://cdn.example.com/i.png#one")
                .expect("anchored url parses")
                .path_query
        );
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 256,
            failure_persistence: None,
            ..ProptestConfig::default()
        })]

        #[test]
        fn every_generated_media_url_survives_the_sign_and_reconstruct_round_trip(
            scheme in prop::sample::select(vec!["http", "https"]),
            host in "[a-z][a-z0-9.-]{0,24}",
            port in prop::option::of(1u16..=65_535),
            path in prop::collection::vec("[^/?#]{0,12}", 1..5),
            query in prop::option::of("[^\u{0}#]{1,24}"),
        ) {
            let authority = match port {
                Some(port) => format!("{host}:{port}"),
                None => host,
            };
            let url = format!(
                "{scheme}://{authority}/{}{}",
                path.join("/"),
                query.map(|query| format!("?{query}")).unwrap_or_default()
            );
            let proxy_path = build_external_media_proxy_path(&url)
                .expect("a generated media url always builds a proxy path");
            let signature = signing::create_signature(&proxy_path, SECRET);
            prop_assert!(signing::verify_signature(&proxy_path, &signature, SECRET));
            prop_assert_eq!(
                url,
                reconstruct_original_url(&proxy_path)
                    .expect("a generated proxy path always reconstructs")
            );
        }
    }
}
