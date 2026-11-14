// SPDX-License-Identifier: AGPL-3.0-or-later

include!(concat!(env!("OUT_DIR"), "/static/fonts.rs"));

pub fn asset(file_name: &str) -> Option<(&'static str, &'static [u8])> {
    ASSETS
        .iter()
        .find(|(name, _, _)| *name == file_name)
        .map(|(_, content_type, bytes)| (*content_type, *bytes))
}
