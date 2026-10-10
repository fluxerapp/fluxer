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
}
