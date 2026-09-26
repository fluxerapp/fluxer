// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_hash::AssetHash,
    constants::AssetExtension,
    disposition::{CONTENT_DISPOSITION_FILENAME_BYTES_MAX, truncate_on_char_boundary},
    image_transform::EncodeEffort,
    media_limits::MediaLimits,
    server::format_policy::is_v1_asset_manual_format,
};
use std::collections::HashMap;

pub(in crate::server) fn bool_param(
    params: &HashMap<String, String>,
    key: &str,
    default_value: bool,
) -> bool {
    params
        .get(key)
        .map(|raw| raw.eq_ignore_ascii_case("true") || raw == "1")
        .unwrap_or(default_value)
}

pub(in crate::server) fn animated_param(
    params: &HashMap<String, String>,
    default_value: bool,
) -> bool {
    params
        .get("animated")
        .map(|raw| raw.eq_ignore_ascii_case("true") || raw == "1")
        .unwrap_or(default_value)
}

pub(in crate::server) fn explicit_output_format(
    params: &HashMap<String, String>,
) -> Result<Option<AssetExtension>, ()> {
    let Some(raw) = params.get("format") else {
        return Ok(None);
    };
    AssetExtension::parse(raw).map(Some).ok_or(())
}

#[cfg(test)]
fn parse_dimension(raw: Option<&str>, limits: &MediaLimits) -> Option<u32> {
    raw.and_then(|v| v.parse::<u32>().ok())
        .filter(|v| *v > 0 && *v <= limits.image_dimension())
}

pub(in crate::server) fn parse_optional_dimension_param(
    params: &HashMap<String, String>,
    key: &str,
    limits: &MediaLimits,
) -> Result<Option<u32>, ()> {
    let Some(raw) = params.get(key) else {
        return Ok(None);
    };
    if raw.is_empty() {
        return Err(());
    }
    let value = raw.parse::<u32>().map_err(|_| ())?;
    if value == 0 || value > limits.image_dimension() {
        return Err(());
    }
    Ok(Some(value))
}

pub(in crate::server) fn parse_effort(params: &HashMap<String, String>) -> Option<EncodeEffort> {
    EncodeEffort::parse_lenient(params.get("effort")?)
}

pub(in crate::server) fn asset_manual_format_override(
    params: &HashMap<String, String>,
    url_ext: AssetExtension,
) -> Option<AssetExtension> {
    let raw = params.get("format").or_else(|| params.get("fmt"));
    if let Some(raw) = raw {
        if raw.eq_ignore_ascii_case("auto") {
            return None;
        }
        if let Some(parsed) = AssetExtension::parse(raw)
            && is_v1_asset_manual_format(parsed)
        {
            return Some(parsed);
        }
    }
    is_v1_asset_manual_format(url_ext).then_some(url_ext)
}

pub(in crate::server) fn asset_wants_animated(
    params: &HashMap<String, String>,
    hash: &str,
) -> bool {
    animated_param(params, AssetHash::parse(hash).is_animated())
}

pub(in crate::server) fn last_segment(value: &str) -> &str {
    value
        .rsplit('/')
        .next()
        .filter(|s| !s.is_empty())
        .unwrap_or("file.bin")
}

pub(in crate::server) fn filename_from_storage_key(key: &str) -> &str {
    last_segment(key)
}

fn strip_query_fragment(value: &str) -> &str {
    let query = value.find('?').unwrap_or(value.len());
    let fragment = value.find('#').unwrap_or(value.len());
    &value[..query.min(fragment)]
}

pub(in crate::server) fn url_filename(url: &str) -> String {
    let filename = last_segment(strip_query_fragment(url));
    truncate_on_char_boundary(filename, CONTENT_DISPOSITION_FILENAME_BYTES_MAX).to_owned()
}

pub(in crate::server) fn extension_of(filename: &str) -> Option<&str> {
    filename.rsplit_once('.').map(|(_, ext)| ext)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn dimensions_are_bounded() {
        let limits = MediaLimits::default_from_config();
        assert_eq!(Some(128), parse_dimension(Some("128"), &limits));
        assert_eq!(None, parse_dimension(Some("0"), &limits));
        assert_eq!(None, parse_dimension(Some("999999999"), &limits));
    }

    #[test]
    fn asset_manual_format_override_is_v1_compatible() {
        assert_eq!(
            Some(AssetExtension::Webp),
            asset_manual_format_override(&HashMap::new(), AssetExtension::Webp)
        );
        assert_eq!(
            None,
            asset_manual_format_override(
                &HashMap::from([("format".to_owned(), "auto".to_owned())]),
                AssetExtension::Webp
            )
        );
        assert_eq!(
            Some(AssetExtension::Png),
            asset_manual_format_override(
                &HashMap::from([("fmt".to_owned(), "png".to_owned())]),
                AssetExtension::Webp
            )
        );
        assert_eq!(
            Some(AssetExtension::Webp),
            asset_manual_format_override(
                &HashMap::from([("format".to_owned(), "svg".to_owned())]),
                AssetExtension::Webp
            )
        );
    }

    #[test]
    fn an_empty_last_url_segment_keeps_the_old_era_file_bin_fallback() {
        assert_eq!("file.bin", url_filename("https://example.test/"));
        assert_eq!("file.bin", url_filename("https://example.test/a/b/"));
        assert_eq!("file.bin", url_filename("https://example.test/a/?x=1"));
        assert_eq!("file.bin", url_filename("https://example.test/a/#frag"));
    }

    #[test]
    fn an_external_url_filename_is_bounded_to_the_disposition_budget() {
        assert_eq!(
            "file.png",
            url_filename("https://example.test/a/file.png?x=1#frag")
        );
        assert_eq!("file.bin", url_filename("https://example.test/"));
        let long = format!(
            "https://example.test/{}",
            "\u{e9}".repeat(CONTENT_DISPOSITION_FILENAME_BYTES_MAX)
        );
        let bounded = url_filename(&long);
        assert!(bounded.len() <= CONTENT_DISPOSITION_FILENAME_BYTES_MAX);
        assert!(bounded.starts_with('\u{e9}'));
        assert!(bounded.ends_with('\u{e9}'));
    }
}
