// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::{
    constants::AssetExtension, image_quality::ImageQuality, image_transform::EncodeEffort, mime,
    output_format, output_format::OutputFormat, server::params::extension_of,
};

pub(in crate::server) fn effective_animated_image_output_format(
    source_ext: Option<AssetExtension>,
    requested_out_ext: OutputFormat,
    animated: bool,
) -> OutputFormat {
    if animated
        && source_ext == Some(AssetExtension::Apng)
        && requested_out_ext == OutputFormat::PNG
    {
        return OutputFormat::APNG;
    }
    if animated
        && source_ext == Some(AssetExtension::Gif)
        && requested_out_ext == OutputFormat::WebP
    {
        return OutputFormat::GIF;
    }
    requested_out_ext
}

pub(in crate::server) fn default_transform_quality(
    format: OutputFormat,
    animated: bool,
    static_default: ImageQuality,
) -> ImageQuality {
    if animated && format == OutputFormat::WebP {
        ImageQuality::Auto
    } else {
        static_default
    }
}

pub(in crate::server) fn transform_static_quality_default(
    source_ext: Option<AssetExtension>,
) -> ImageQuality {
    if matches!(
        source_ext,
        Some(AssetExtension::Jpeg | AssetExtension::Heic | AssetExtension::Heif)
    ) {
        ImageQuality::High
    } else {
        ImageQuality::Lossless
    }
}

pub(in crate::server) fn is_v1_asset_manual_format(ext: AssetExtension) -> bool {
    matches!(
        ext,
        AssetExtension::Png
            | AssetExtension::Jpeg
            | AssetExtension::Webp
            | AssetExtension::Gif
            | AssetExtension::Apng
            | AssetExtension::Avif
    )
}

fn animated_image_request_can_use_original(
    source_ext: AssetExtension,
    explicit_out_ext: Option<AssetExtension>,
    out_ext: OutputFormat,
    width: Option<u32>,
    height: Option<u32>,
    animated: bool,
) -> bool {
    if !animated || width.is_some() || height.is_some() {
        return false;
    }
    if !matches!(
        source_ext,
        AssetExtension::Gif | AssetExtension::Webp | AssetExtension::Apng
    ) {
        return false;
    }
    let requested = explicit_out_ext.unwrap_or(out_ext.as_asset_extension());
    requested == source_ext
}

fn same_format_image_request_base_allows_original(
    source_ext: AssetExtension,
    explicit_out_ext: Option<AssetExtension>,
    out_ext: OutputFormat,
    has_quality: bool,
    effort: Option<EncodeEffort>,
) -> bool {
    if effort.is_some() {
        return false;
    }
    if !output_format::is_output_format_supported(source_ext) {
        return false;
    }
    if out_ext.as_asset_extension() != source_ext {
        return false;
    }
    let _ = explicit_out_ext;
    if has_quality && source_ext != AssetExtension::Gif {
        return false;
    }
    true
}

#[derive(Clone, Copy)]
pub(in crate::server) struct OriginalImageRequest {
    pub(in crate::server) source_ext: Option<AssetExtension>,
    pub(in crate::server) explicit_out_ext: Option<AssetExtension>,
    pub(in crate::server) out_ext: OutputFormat,
    pub(in crate::server) width: Option<u32>,
    pub(in crate::server) height: Option<u32>,
    pub(in crate::server) has_quality: bool,
    pub(in crate::server) effort: Option<EncodeEffort>,
    pub(in crate::server) animated: bool,
}

pub(in crate::server) fn same_format_loaded_image_request_can_use_original_with_sniff(
    sniffed: mime::SniffInfo,
    request: OriginalImageRequest,
) -> bool {
    let Some(source_ext) = request.source_ext else {
        return false;
    };
    if animated_image_request_can_use_original(
        source_ext,
        request.explicit_out_ext,
        request.out_ext,
        request.width,
        request.height,
        request.animated,
    ) {
        return true;
    }
    if !same_format_image_request_base_allows_original(
        source_ext,
        request.explicit_out_ext,
        request.out_ext,
        request.has_quality,
        request.effort,
    ) {
        return false;
    }
    if !(request.animated
        || request.explicit_out_ext.is_some()
        || request.has_quality
        || request.width.is_some()
        || request.height.is_some())
    {
        return false;
    }
    if sniffed.width == 0 || sniffed.height == 0 {
        return false;
    }
    if let Some(target_w) = request.width
        && target_w < sniffed.width
    {
        return false;
    }
    if let Some(target_h) = request.height
        && target_h < sniffed.height
    {
        return false;
    }
    true
}

pub(in crate::server) fn content_type_is_trustworthy(content_type: &str) -> bool {
    if content_type.is_empty() {
        return false;
    }
    if content_type.eq_ignore_ascii_case("application/octet-stream") {
        return false;
    }
    matches!(
        mime::category(content_type),
        Some(mime::Category::Image | mime::Category::Video | mime::Category::Audio)
    )
}

pub(in crate::server) fn is_svg_content_type(content_type: &str) -> bool {
    mime::normalize(Some(content_type))
        .is_some_and(|value| value.eq_ignore_ascii_case("image/svg+xml"))
}

pub(in crate::server) fn extension_from_mime(content_type: &str) -> Option<AssetExtension> {
    match mime::normalize(Some(content_type))? {
        "image/jpeg" => Some(AssetExtension::Jpeg),
        "image/png" => Some(AssetExtension::Png),
        "image/webp" => Some(AssetExtension::Webp),
        "image/gif" => Some(AssetExtension::Gif),
        "image/apng" => Some(AssetExtension::Apng),
        "image/avif" => Some(AssetExtension::Avif),
        "image/heic" => Some(AssetExtension::Heic),
        "image/heif" => Some(AssetExtension::Heif),
        "image/jxl" => Some(AssetExtension::Jxl),
        "image/svg+xml" => Some(AssetExtension::Svg),
        _ => None,
    }
}

pub(in crate::server) fn image_extension_from_filename(filename: &str) -> Option<AssetExtension> {
    AssetExtension::parse(extension_of(filename)?)
}

pub(in crate::server) fn source_image_format(
    sniffed_mime: &str,
    content_type: &str,
    filename: &str,
) -> Option<AssetExtension> {
    if extension_from_mime(sniffed_mime) == Some(AssetExtension::Apng) {
        return Some(AssetExtension::Apng);
    }
    extension_from_mime(content_type).or_else(|| image_extension_from_filename(filename))
}

pub(in crate::server) fn external_default_output_extension(
    filename: &str,
    content_type: &str,
) -> AssetExtension {
    extension_from_mime(content_type)
        .or_else(|| image_extension_from_filename(filename))
        .unwrap_or(AssetExtension::Webp)
}

pub(in crate::server) fn transform_response_content_type(
    explicit_out_ext: Option<AssetExtension>,
    requested_out_ext: AssetExtension,
    out_ext: OutputFormat,
    fallback_content_type: &str,
) -> &str {
    if explicit_out_ext.is_some()
        || out_ext.as_asset_extension() != requested_out_ext
        || is_svg_content_type(fallback_content_type)
    {
        out_ext.mime()
    } else {
        fallback_content_type
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::server::params::{
        animated_param, explicit_output_format, filename_from_storage_key, url_filename,
    };
    use std::collections::HashMap;

    #[test]
    fn animated_gif_requests_downgrade_webp_output_to_gif() {
        assert_eq!(
            OutputFormat::APNG,
            effective_animated_image_output_format(
                Some(AssetExtension::Apng),
                OutputFormat::PNG,
                true
            )
        );
        assert_eq!(
            OutputFormat::PNG,
            effective_animated_image_output_format(
                Some(AssetExtension::Apng),
                OutputFormat::PNG,
                false
            )
        );
        assert_eq!(
            OutputFormat::GIF,
            effective_animated_image_output_format(
                Some(AssetExtension::Gif),
                OutputFormat::WebP,
                true
            )
        );
        assert_eq!(
            OutputFormat::WebP,
            effective_animated_image_output_format(
                Some(AssetExtension::Gif),
                OutputFormat::WebP,
                false
            )
        );
        assert_eq!(
            OutputFormat::WebP,
            effective_animated_image_output_format(
                Some(AssetExtension::Webp),
                OutputFormat::WebP,
                true
            )
        );
        assert_eq!(
            OutputFormat::PNG,
            effective_animated_image_output_format(
                Some(AssetExtension::Png),
                OutputFormat::PNG,
                true
            )
        );
        assert_eq!(
            OutputFormat::PNG,
            effective_animated_image_output_format(
                Some(AssetExtension::Png),
                OutputFormat::PNG,
                false
            )
        );
        assert!(animated_param(
            &HashMap::from([("animated".to_owned(), "true".to_owned())]),
            false
        ));
        assert!(!animated_param(
            &HashMap::from([("animatd".to_owned(), "true".to_owned())]),
            false
        ));
    }

    #[test]
    fn attachment_and_external_query_helpers_match_v1_edges() {
        for name in [
            "clip.mp4",
            "clip.m4v",
            "clip.webm",
            "clip.mov",
            "clip.ogv",
            "clip.mkv",
            "clip.3gp",
            "clip.avi",
            "clip.flv",
            "clip.ts",
            "clip.mpg",
            "clip.mpeg",
            "clip.wmv",
        ] {
            assert_eq!(None, image_extension_from_filename(name));
        }
        assert_eq!(
            AssetExtension::Gif,
            external_default_output_extension("welcome.png", "image/gif")
        );
        assert_eq!(
            AssetExtension::Png,
            external_default_output_extension("welcome.png", "application/octet-stream")
        );
        assert_eq!(
            "image/gif",
            transform_response_content_type(
                None,
                AssetExtension::Gif,
                OutputFormat::GIF,
                "image/gif"
            )
        );
        assert_eq!(
            "image/webp",
            transform_response_content_type(
                None,
                AssetExtension::Heic,
                OutputFormat::WebP,
                "image/heic"
            )
        );
        assert_eq!(
            "image/webp",
            transform_response_content_type(
                None,
                AssetExtension::Webp,
                OutputFormat::WebP,
                "image/svg+xml; charset=utf-8"
            )
        );
        assert_eq!(
            "image/gif",
            transform_response_content_type(
                Some(AssetExtension::Webp),
                AssetExtension::Webp,
                OutputFormat::GIF,
                "image/gif"
            )
        );
        assert_eq!(
            "file.png",
            url_filename("https://example.test/a/file.png?x=1#frag")
        );
        assert_eq!(
            "photo.png",
            filename_from_storage_key("attachments/123/456/photo.png")
        );
        assert_eq!(
            "file.bin",
            filename_from_storage_key("attachments/123/456/")
        );
        assert!(
            explicit_output_format(&HashMap::from([("format".to_owned(), "auto".to_owned())]))
                .is_err()
        );
    }

    #[test]
    fn unmeasurable_bytes_never_reuse_the_original() {
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(b"GIF89a"),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: false,
                }
            )
        );
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(b"GIF89a"),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Png),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::PNG,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
    }

    #[test]
    fn same_format_gif_noop_requests_use_original_bytes() {
        let gif_header = b"GIF89a\x2c\x01\xe1\x00";
        assert!(
            same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(300),
                    height: Some(225),
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
        assert!(
            same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(301),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(299),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: Some(AssetExtension::Webp),
                    out_ext: OutputFormat::WebP,
                    width: None,
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: Some(AssetExtension::Webp),
                    out_ext: OutputFormat::WebP,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: false,
                }
            )
        );
        assert!(
            !same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: EncodeEffort::parse_lenient("1"),
                    animated: false,
                }
            )
        );
    }

    fn animated_vp8x_webp(width: u32, height: u32) -> Vec<u8> {
        let mut webp = vec![0_u8; 30];
        webp[0..4].copy_from_slice(b"RIFF");
        webp[8..12].copy_from_slice(b"WEBP");
        webp[12..16].copy_from_slice(b"VP8X");
        webp[16..20].copy_from_slice(&10_u32.to_le_bytes());
        webp[20] = 0x02;
        webp[24..27].copy_from_slice(&(width - 1).to_le_bytes()[..3]);
        webp[27..30].copy_from_slice(&(height - 1).to_le_bytes()[..3]);
        webp
    }

    #[test]
    fn still_requests_for_animation_capable_sources_use_original_bytes() {
        let gif_header = b"GIF89a\x2c\x01\xe1\x00";
        assert!(
            same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Gif),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::GIF,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: false,
                }
            )
        );
        let animated_webp = animated_vp8x_webp(300, 225);
        assert!(mime::sniff(&animated_webp).animated);
        assert!(
            same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(&animated_webp),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Webp),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::WebP,
                    width: Some(300),
                    height: Some(225),
                    has_quality: false,
                    effort: None,
                    animated: false,
                }
            )
        );
    }

    #[test]
    fn a_sniffed_mime_disagreeing_with_the_source_extension_still_uses_original_bytes() {
        let gif_header = b"GIF89a\x2c\x01\xe1\x00";
        assert!(
            same_format_loaded_image_request_can_use_original_with_sniff(
                mime::sniff(gif_header),
                OriginalImageRequest {
                    source_ext: Some(AssetExtension::Png),
                    explicit_out_ext: None,
                    out_ext: OutputFormat::PNG,
                    width: Some(300),
                    height: None,
                    has_quality: false,
                    effort: None,
                    animated: true,
                }
            )
        );
    }
}
