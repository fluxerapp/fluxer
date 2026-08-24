// SPDX-License-Identifier: AGPL-3.0-or-later

pub use fluxer_common::external_media_path::{
    ExternalPathError, build_external_media_proxy_path, build_opaque_external_media_proxy_path,
    percent_decode, percent_decode_string, reconstruct_original_url,
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::public_net_policy;

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
}
