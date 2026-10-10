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
}
