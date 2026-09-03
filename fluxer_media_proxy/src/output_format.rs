// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    asset_size,
    constants::{AssetExtension, AssetKind},
};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum OutputFormat {
    PNG,
    JPEG,
    WebP,
    GIF,
    APNG,
}

impl OutputFormat {
    pub const fn from_source_extension(extension: AssetExtension) -> Option<Self> {
        match extension {
            AssetExtension::Png => Some(Self::PNG),
            AssetExtension::Jpeg => Some(Self::JPEG),
            AssetExtension::Webp => Some(Self::WebP),
            AssetExtension::Gif => Some(Self::GIF),
            AssetExtension::Apng => Some(Self::APNG),
            AssetExtension::Avif
            | AssetExtension::Heic
            | AssetExtension::Heif
            | AssetExtension::Jxl
            | AssetExtension::Svg => None,
        }
    }

    pub const fn coerce_from_extension(extension: AssetExtension) -> Self {
        match Self::from_source_extension(extension) {
            Some(format) => format,
            None => Self::WebP,
        }
    }

    pub const fn as_asset_extension(self) -> AssetExtension {
        match self {
            Self::PNG => AssetExtension::Png,
            Self::JPEG => AssetExtension::Jpeg,
            Self::WebP => AssetExtension::Webp,
            Self::GIF => AssetExtension::Gif,
            Self::APNG => AssetExtension::Apng,
        }
    }

    pub const fn mime(self) -> &'static str {
        match self {
            Self::PNG => "image/png",
            Self::JPEG => "image/jpeg",
            Self::WebP => "image/webp",
            Self::GIF => "image/gif",
            Self::APNG => "image/apng",
        }
    }

    pub const fn extension(self) -> &'static str {
        match self {
            Self::PNG => "png",
            Self::JPEG => "jpeg",
            Self::WebP => "webp",
            Self::GIF => "gif",
            Self::APNG => "apng",
        }
    }

    pub const fn cache_serialization(self) -> &'static str {
        self.extension()
    }

    pub const fn supports_animation(self) -> bool {
        matches!(self, Self::WebP | Self::GIF | Self::APNG)
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Input {
    pub kind: AssetKind,
    pub original: AssetExtension,
    pub requested_size: Option<u32>,
    pub manual_format_override: Option<AssetExtension>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OutputSelection {
    pub format: OutputFormat,
    pub size: Option<u32>,
    pub reason: &'static str,
}

pub fn is_output_format_supported(ext: AssetExtension) -> bool {
    OutputFormat::from_source_extension(ext).is_some()
}

pub fn coerce_unsupported_format(ext: AssetExtension) -> AssetExtension {
    OutputFormat::coerce_from_extension(ext).as_asset_extension()
}

pub fn select_url_variant(input: Input) -> OutputSelection {
    let requested = input.manual_format_override.unwrap_or(input.original);
    OutputSelection {
        format: OutputFormat::coerce_from_extension(requested),
        size: input
            .requested_size
            .map(|size| asset_size::clamp_size(size, input.kind)),
        reason: if is_output_format_supported(requested) {
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
        assert_eq!(OutputFormat::PNG, r.format);
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
        assert_eq!(OutputFormat::WebP, r.format);
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
        assert_eq!(OutputFormat::WebP, r.format);
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
        assert_eq!(OutputFormat::PNG, r.format);
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
        assert_eq!(OutputFormat::WebP, r.format);
        assert_eq!("url-coerced", r.reason);
    }

    #[test]
    fn encodable_extensions_round_trip_through_the_output_format() {
        for extension in [
            AssetExtension::Png,
            AssetExtension::Jpeg,
            AssetExtension::Webp,
            AssetExtension::Gif,
            AssetExtension::Apng,
        ] {
            let format = OutputFormat::from_source_extension(extension).expect("encodable");
            assert!(is_output_format_supported(extension));
            assert_eq!(extension, format.as_asset_extension());
            assert_eq!(extension.mime(), format.mime());
            assert_eq!(extension.name(), format.extension());
            assert_eq!(format.extension(), format.cache_serialization());
            assert_eq!(extension, coerce_unsupported_format(extension));
        }
        for extension in [
            AssetExtension::Avif,
            AssetExtension::Heic,
            AssetExtension::Heif,
            AssetExtension::Jxl,
            AssetExtension::Svg,
        ] {
            assert!(!is_output_format_supported(extension));
            assert_eq!(None, OutputFormat::from_source_extension(extension));
            assert_eq!(
                OutputFormat::WebP,
                OutputFormat::coerce_from_extension(extension)
            );
            assert_eq!(AssetExtension::Webp, coerce_unsupported_format(extension));
        }
    }

    #[test]
    fn only_the_animation_containers_support_animation() {
        assert!(OutputFormat::WebP.supports_animation());
        assert!(OutputFormat::GIF.supports_animation());
        assert!(OutputFormat::APNG.supports_animation());
        assert!(!OutputFormat::PNG.supports_animation());
        assert!(!OutputFormat::JPEG.supports_animation());
    }
}
