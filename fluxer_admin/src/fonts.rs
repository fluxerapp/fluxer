// SPDX-License-Identifier: AGPL-3.0-or-later

include!(concat!(env!("OUT_DIR"), "/static/fonts.rs"));

pub fn asset(file_name: &str) -> Option<(&'static str, &'static [u8])> {
    ASSETS
        .iter()
        .find(|(name, _, _)| *name == file_name)
        .map(|(_, content_type, bytes)| (*content_type, *bytes))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn stylesheet_is_served_and_content_hashed() {
        let (content_type, bytes) =
            asset(STYLESHEET_FILE_NAME).expect("the generated stylesheet must be servable");
        assert_eq!(content_type, "text/css; charset=utf-8");
        let css = std::str::from_utf8(bytes).expect("stylesheet must be UTF-8");
        assert!(css.contains("font-family: 'Fluxer Sans'"));
        assert!(css.contains("font-family: 'Fluxer Mono'"));
        assert!(
            !css.contains("?v="),
            "content hashing replaces cache-bust tokens"
        );
        assert!(
            !css.contains("fluxerstatic"),
            "fonts must not be fetched from the static CDN"
        );
        assert!(
            STYLESHEET_FILE_NAME.starts_with("fonts.") && STYLESHEET_FILE_NAME.ends_with(".css"),
            "unexpected stylesheet name {STYLESHEET_FILE_NAME}"
        );
    }

    #[test]
    fn every_face_the_stylesheet_references_is_served() {
        let (_, bytes) = asset(STYLESHEET_FILE_NAME).expect("stylesheet");
        let css = std::str::from_utf8(bytes).expect("stylesheet must be UTF-8");
        let mut referenced = 0;
        for fragment in css.split("url('").skip(1) {
            let file_name = fragment.split('\'').next().expect("unterminated url()");
            let (content_type, _) = asset(file_name)
                .unwrap_or_else(|| panic!("stylesheet references unserved font {file_name}"));
            assert_eq!(content_type, "font/woff2");
            referenced += 1;
        }
        assert_eq!(referenced, 16, "expected the 16 bundled Latin-core faces");
    }

    #[test]
    fn ofl_attribution_ships_with_the_binaries() {
        let notice = ASSETS
            .iter()
            .find(|(name, _, _)| name.starts_with("NOTICE.") && name.ends_with(".md"))
            .expect("the OFL modification disclosure must ship with the modified fonts");
        assert!(!notice.2.is_empty());
        assert!(
            ASSETS
                .iter()
                .any(|(name, _, _)| name.starts_with("LICENSE-IBM-PLEX."))
        );
    }

    #[test]
    fn unknown_files_are_not_served() {
        assert!(asset("fonts.css").is_none());
        assert!(asset("../../../etc/passwd").is_none());
    }
}
