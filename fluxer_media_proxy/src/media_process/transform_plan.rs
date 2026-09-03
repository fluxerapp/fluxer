// SPDX-License-Identifier: AGPL-3.0-or-later

use super::{AnimatedProbe, MediaError};
use crate::{
    image_quality::{ImageQuality, ResolvedImageQuality},
    image_transform::{EncodeEffort, ImageOptions},
    mime,
    output_format::OutputFormat,
};
use libc::c_int;
use std::ffi::CString;

const AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_BYTES: usize = 4 * 1024 * 1024;
const AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_PIXELS: usize = 16 * 1024 * 1024;
const VIPS_WEBP_MAX_EFFORT: u8 = 6;

pub(super) fn output_suffix(
    format: OutputFormat,
    quality: ResolvedImageQuality,
    page_height: Option<c_int>,
    effort_override: Option<EncodeEffort>,
) -> Result<CString, MediaError> {
    let q = quality.encoder_quality();
    let animated = page_height.is_some();
    let lossless = if quality.is_lossless() {
        "true"
    } else {
        "false"
    };
    let effort = effort_override
        .map(|effort| effort.get().min(VIPS_WEBP_MAX_EFFORT))
        .unwrap_or_else(|| quality.default_effort(animated));
    let suffix = match format {
        OutputFormat::JPEG => format!(".jpg[Q={q},strip,interlace=true,optimize_coding=true]"),
        OutputFormat::WebP => {
            let effort = effort.min(VIPS_WEBP_MAX_EFFORT);
            match page_height {
                Some(ph) => format!(
                    ".webp[Q={q},lossless={lossless},strip,effort={effort},smart_subsample=true,alpha_q=90,page-height={ph}]"
                ),
                None => format!(
                    ".webp[Q={q},lossless={lossless},strip,effort={effort},smart_subsample=true,alpha_q=90]"
                ),
            }
        }
        OutputFormat::PNG | OutputFormat::APNG => match page_height {
            Some(ph) if format == OutputFormat::APNG => {
                format!(".png[strip,compression=9,filter=all,page-height={ph}]")
            }
            _ => ".png[strip,compression=9,filter=all]".to_owned(),
        },
        OutputFormat::GIF => match page_height {
            Some(ph) => {
                format!(".gif[strip,dither=1.0,effort=7,interframe_maxerror=8.0,page-height={ph}]")
            }
            None => ".gif[strip,dither=1.0,effort=7]".to_owned(),
        },
    };
    CString::new(suffix).map_err(|_| MediaError::MediaEncodeFailed)
}

pub(super) fn output_is_sdr(format: OutputFormat) -> bool {
    matches!(
        format,
        OutputFormat::JPEG
            | OutputFormat::WebP
            | OutputFormat::PNG
            | OutputFormat::GIF
            | OutputFormat::APNG
    )
}

pub(super) fn should_transform_animated_webp_direct(
    sniffed: mime::SniffInfo,
    options: &ImageOptions,
    format: OutputFormat,
) -> bool {
    sniffed.mime == "image/webp"
        && sniffed.animated
        && options.is_animated()
        && !options.wants_cover_crop()
        && format == OutputFormat::WebP
}

#[derive(Clone, Copy)]
pub(super) struct AutoQualitySource<'a> {
    pub(super) format: OutputFormat,
    pub(super) animated: bool,
    pub(super) sniffed_mime: &'a str,
    pub(super) input: &'a [u8],
    pub(super) quality: ImageQuality,
    pub(super) probe: Option<AnimatedProbe>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub(super) struct ResolvedAutoQuality {
    pub(super) quality: ResolvedImageQuality,
    pub(super) effort_override: Option<EncodeEffort>,
}

pub(super) fn resolve_auto_quality(
    source: AutoQualitySource<'_>,
    requested_override: Option<EncodeEffort>,
) -> ResolvedAutoQuality {
    if !source.quality.is_auto() {
        return ResolvedAutoQuality {
            quality: source.quality.resolve_static(),
            effort_override: requested_override,
        };
    }
    let palette_animation = source.animated
        && source.format == OutputFormat::WebP
        && matches!(source.sniffed_mime, "image/gif" | "image/apng");
    let within_pixel_limit = source.probe.is_none_or(|probe| {
        animated_probe_pixels(probe)
            .is_some_and(|pixels| pixels <= AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_PIXELS)
    });
    let lossless = palette_animation
        && source.input.len() <= AUTO_LOSSLESS_PALETTE_ANIMATION_MAX_BYTES
        && within_pixel_limit;
    let quality = if lossless {
        ResolvedImageQuality::Lossless
    } else {
        source.quality.resolve_static()
    };
    let effort_override = if requested_override.is_some() || !palette_animation || lossless {
        requested_override
    } else {
        Some(EncodeEffort::minimum())
    };
    ResolvedAutoQuality {
        quality,
        effort_override,
    }
}

fn animated_probe_pixels(probe: AnimatedProbe) -> Option<usize> {
    if probe.width <= 0 || probe.height <= 0 || probe.pages <= 0 {
        return None;
    }
    (probe.width as usize)
        .checked_mul(probe.height as usize)?
        .checked_mul(probe.pages as usize)
}
