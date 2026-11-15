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
        Some(
            AssetExtension::Jpeg
                | AssetExtension::Heic
                | AssetExtension::Heif
                | AssetExtension::Jxl
        )
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
    if sniffed.animated && !request.animated {
        return false;
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

pub(in crate::server) fn is_browser_displayable_image(mime: &str) -> bool {
    matches!(
        mime,
        "image/jpeg" | "image/png" | "image/gif" | "image/webp"
    )
}

pub(in crate::server) fn is_svg_content_type(content_type: &str) -> bool {
    mime::normalize(Some(content_type))
        .is_some_and(|value| value.eq_ignore_ascii_case("image/svg+xml"))
}

pub(in crate::server) fn is_untransformed_fallback_image(
    content_type: &str,
    source_format: Option<AssetExtension>,
) -> bool {
    content_type.starts_with("image/")
        && content_type != "image/heic"
        && content_type != "image/heif"
        && source_format != Some(AssetExtension::Svg)
        && !is_svg_content_type(content_type)
}

pub(in crate::server) fn extension_from_mime(mime: &str) -> Option<AssetExtension> {
    let mime = mime::normalize(Some(mime))?;
    AssetExtension::ALL
        .into_iter()
        .find(|extension| extension.mime() == mime)
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
