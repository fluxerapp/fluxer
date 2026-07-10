// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::constants::{self, AssetExtension, AssetKind};

#[derive(Clone, Copy, Debug)]
pub struct Input {
    pub kind: AssetKind,
    pub original: AssetExtension,
    pub requested_size: Option<u32>,
    pub manual_format_override: Option<AssetExtension>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OutputSelection {
    pub format: AssetExtension,
    pub size: Option<u32>,
    pub reason: &'static str,
}

pub fn is_output_format_supported(ext: AssetExtension) -> bool {
    !matches!(
        ext,
        AssetExtension::Avif
            | AssetExtension::Heic
            | AssetExtension::Heif
            | AssetExtension::Jxl
            | AssetExtension::Svg
    )
}

pub fn can_encode_to(ext: AssetExtension) -> bool {
    is_output_format_supported(ext)
}

pub fn coerce_unsupported_format(ext: AssetExtension) -> AssetExtension {
    if can_encode_to(ext) {
        ext
    } else {
        AssetExtension::Webp
    }
}

pub fn select_url_variant(input: Input) -> OutputSelection {
    let requested = input.manual_format_override.unwrap_or(input.original);
    let output = coerce_unsupported_format(requested);
    OutputSelection {
        format: output,
        size: input
            .requested_size
            .map(|size| constants::clamp_size(size, input.kind)),
        reason: if output == requested {
            "url"
        } else {
            "url-coerced"
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn url_extension_drives_output_format() {
        let r = select_url_variant(Input {
            kind: AssetKind::GuildIcon,
            original: AssetExtension::Png,
            requested_size: Some(128),
            manual_format_override: None,
        });
        assert_eq!(AssetExtension::Png, r.format);
        assert_eq!("url", r.reason);
    }

    #[test]
    fn unsupported_url_extension_coerces_to_webp() {
        let r = select_url_variant(Input {
            kind: AssetKind::Avatar,
            original: AssetExtension::Avif,
            requested_size: Some(128),
            manual_format_override: None,
        });
        assert_eq!(AssetExtension::Webp, r.format);
        assert_eq!("url-coerced", r.reason);
    }

    #[test]
    fn svg_url_extension_coerces_to_webp() {
        let r = select_url_variant(Input {
            kind: AssetKind::Avatar,
            original: AssetExtension::Svg,
            requested_size: Some(128),
            manual_format_override: None,
        });
        assert_eq!(AssetExtension::Webp, r.format);
        assert_eq!("url-coerced", r.reason);
    }

    #[test]
    fn manual_query_format_wins_over_url_extension() {
        let r = select_url_variant(Input {
            kind: AssetKind::Avatar,
            original: AssetExtension::Jpeg,
            requested_size: Some(128),
            manual_format_override: Some(AssetExtension::Png),
        });
        assert_eq!(AssetExtension::Png, r.format);
        assert_eq!("url", r.reason);
    }

    #[test]
    fn manual_unsupported_query_format_coerces_to_webp() {
        let r = select_url_variant(Input {
            kind: AssetKind::Avatar,
            original: AssetExtension::Jpeg,
            requested_size: Some(256),
            manual_format_override: Some(AssetExtension::Svg),
        });
        assert_eq!(AssetExtension::Webp, r.format);
        assert_eq!("url-coerced", r.reason);
    }
}
