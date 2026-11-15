// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::constants::AssetExtension;

struct MediaTypeSpec {
    media_type: MediaType,
    mime: &'static str,
    mime_aliases: &'static [&'static str],
    canonical_extension: &'static str,
    extension_aliases: &'static [&'static str],
}

macro_rules! define_media_types {
    (
        assets {
            $(
                $asset_variant:ident => (
                    AssetExtension::$asset_extension_variant:ident,
                    $asset_mime:literal,
                    [$($asset_mime_alias:literal),* $(,)?],
                    $asset_extension:literal,
                    [$($asset_extension_alias:literal),* $(,)?]
                )
            ),+ $(,)?
        }
        media {
            $(
                $media_variant:ident => (
                    $media_mime:literal,
                    [$($media_mime_alias:literal),* $(,)?],
                    $media_extension:literal,
                    [$($media_extension_alias:literal),* $(,)?]
                )
            ),+ $(,)?
        }
    ) => {
        #[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
        #[expect(clippy::upper_case_acronyms)]
        pub enum MediaType {
            $($asset_variant,)+
            $($media_variant,)+
        }

        const MEDIA_TYPE_SPECS: &[MediaTypeSpec] = &[
            $(
                MediaTypeSpec {
                    media_type: MediaType::$asset_variant,
                    mime: $asset_mime,
                    mime_aliases: &[$($asset_mime_alias),*],
                    canonical_extension: $asset_extension,
                    extension_aliases: &[$($asset_extension_alias),*],
                },
            )+
            $(
                MediaTypeSpec {
                    media_type: MediaType::$media_variant,
                    mime: $media_mime,
                    mime_aliases: &[$($media_mime_alias),*],
                    canonical_extension: $media_extension,
                    extension_aliases: &[$($media_extension_alias),*],
                },
            )+
        ];

        impl MediaType {
            pub fn from_mime(mime: &str) -> Option<Self> {
                MEDIA_TYPE_SPECS
                    .iter()
                    .find(|spec| {
                        spec.mime.eq_ignore_ascii_case(mime)
                            || spec
                                .mime_aliases
                                .iter()
                                .any(|alias| alias.eq_ignore_ascii_case(mime))
                    })
                    .map(|spec| spec.media_type)
            }

            pub fn from_extension(extension: &str) -> Option<Self> {
                MEDIA_TYPE_SPECS
                    .iter()
                    .find(|spec| {
                        spec.canonical_extension.eq_ignore_ascii_case(extension)
                            || spec
                                .extension_aliases
                                .iter()
                                .any(|alias| alias.eq_ignore_ascii_case(extension))
                    })
                    .map(|spec| spec.media_type)
            }

            pub fn mime(self) -> &'static str {
                self.spec().mime
            }

            fn spec(self) -> &'static MediaTypeSpec {
                MEDIA_TYPE_SPECS
                    .iter()
                    .find(|spec| spec.media_type == self)
                    .expect("every media type must have one specification")
            }
        }

        impl From<AssetExtension> for MediaType {
            fn from(value: AssetExtension) -> Self {
                match value {
                    $(AssetExtension::$asset_extension_variant => Self::$asset_variant,)+
                }
            }
        }
    };
}

define_media_types!(
    assets {
        PNG => (AssetExtension::Png, "image/png", [], "png", []),
        JPEG => (AssetExtension::Jpeg, "image/jpeg", [], "jpeg", ["jpg"]),
        WebP => (AssetExtension::Webp, "image/webp", [], "webp", []),
        GIF => (AssetExtension::Gif, "image/gif", [], "gif", []),
        APNG => (AssetExtension::Apng, "image/apng", [], "apng", []),
        AVIF => (AssetExtension::Avif, "image/avif", ["image/avif-sequence"], "avif", []),
        HEIC => (AssetExtension::Heic, "image/heic", ["image/heic-sequence"], "heic", []),
        HEIF => (AssetExtension::Heif, "image/heif", ["image/heif-sequence"], "heif", []),
        JXL => (AssetExtension::Jxl, "image/jxl", [], "jxl", []),
        SVG => (AssetExtension::Svg, "image/svg+xml", [], "svg", []),
    }
    media {
        TIFF => ("image/tiff", [], "tiff", ["tif"]),
        BMP => ("image/bmp", [], "bmp", []),
        MP4Video => ("video/mp4", [], "mp4", ["m4v"]),
        WebMVideo => ("video/webm", [], "webm", []),
        QuickTimeVideo => ("video/quicktime", [], "mov", []),
        ThreeGPPVideo => ("video/3gpp", [], "3gp", []),
        MatroskaVideo => ("video/x-matroska", [], "mkv", []),
        AVIVideo => ("video/x-msvideo", [], "avi", []),
        FLVVideo => ("video/x-flv", [], "flv", []),
        OGGVideo => ("video/ogg", [], "ogv", []),
        MPEGTSVideo => ("video/mp2t", [], "ts", []),
        MPEGVideo => ("video/mpeg", [], "mpeg", ["mpg"]),
        WMVVideo => ("video/x-ms-wmv", [], "wmv", []),
        MPEGAudio => ("audio/mpeg", [], "mp3", []),
        WAVAudio => ("audio/wav", [], "wav", []),
        FLACAudio => ("audio/flac", [], "flac", []),
        OGGAudio => ("audio/ogg", [], "ogg", ["oga", "opus"]),
        AACAudio => ("audio/aac", [], "aac", []),
        MP4Audio => ("audio/mp4", [], "m4a", ["m4b"]),
        WebMAudio => ("audio/webm", [], "weba", []),
        AIFFAudio => ("audio/aiff", [], "aiff", ["aif"]),
    }
);
