// SPDX-License-Identifier: AGPL-3.0-or-later

use std::sync::atomic::{AtomicU32, AtomicUsize, Ordering};

pub const MAX_MEDIA_PROXY_BYTES: usize = 500 * 1024 * 1024;

pub const OUTBOUND_USER_AGENT: &str =
    "Mozilla/5.0 (compatible; Fluxerbot/1.0; +https://fluxer.app)";
pub const MAX_MEDIA_IMAGE_DIMENSION_DEFAULT: u32 = 16_384;
pub const MAX_MEDIA_IMAGE_PIXELS_DEFAULT: usize =
    MAX_MEDIA_IMAGE_DIMENSION_DEFAULT as usize * MAX_MEDIA_IMAGE_DIMENSION_DEFAULT as usize;
pub const MAX_INTERNAL_REQUEST_BODY_BYTES: usize =
    MAX_MEDIA_PROXY_BYTES.div_ceil(3) * 4 + 1024 * 1024;
pub const MAX_VIDEO_PACKETS_FOR_THUMBNAIL: usize = 512;
pub const MAX_VIDEO_FRAME_BYTES: usize = 128 * 1024 * 1024;
pub const MAX_S3_ATTEMPTS: u8 = 3;
pub const DEFAULT_IMAGE_SIZE: u32 = 128;
pub const MAX_ANIMATED_FRAMES_DEFAULT: u32 = 20_000;
pub const MAX_ANIMATED_TOTAL_PIXELS_DEFAULT: usize = 4 * MAX_MEDIA_IMAGE_PIXELS_DEFAULT;
const _: () = assert!(MAX_ANIMATED_FRAMES_DEFAULT >= 20_000);

static IMAGE_DIMENSION: AtomicU32 = AtomicU32::new(MAX_MEDIA_IMAGE_DIMENSION_DEFAULT);
static IMAGE_PIXELS: AtomicUsize = AtomicUsize::new(MAX_MEDIA_IMAGE_PIXELS_DEFAULT);
static ANIMATED_FRAMES: AtomicU32 = AtomicU32::new(MAX_ANIMATED_FRAMES_DEFAULT);
static ANIMATED_TOTAL_PIXELS: AtomicUsize = AtomicUsize::new(MAX_ANIMATED_TOTAL_PIXELS_DEFAULT);

pub struct Limits;

impl Limits {
    pub fn image_dimension() -> u32 {
        IMAGE_DIMENSION.load(Ordering::Acquire)
    }

    pub fn image_pixels() -> usize {
        IMAGE_PIXELS.load(Ordering::Acquire)
    }

    pub fn animated_frames() -> u32 {
        ANIMATED_FRAMES.load(Ordering::Acquire)
    }

    pub fn animated_total_pixels() -> usize {
        ANIMATED_TOTAL_PIXELS.load(Ordering::Acquire)
    }

    pub fn set_image_dimension(value: u32) {
        let clamped = value.max(16);
        IMAGE_DIMENSION.store(clamped, Ordering::Release);
        IMAGE_PIXELS.store(clamped as usize * clamped as usize, Ordering::Release);
    }

    pub fn set_animated_frames(value: u32) {
        ANIMATED_FRAMES.store(value.max(1), Ordering::Release);
    }

    pub fn set_animated_total_pixels(value: usize) {
        ANIMATED_TOTAL_PIXELS.store(value.max(1024), Ordering::Release);
    }
}

pub const IMAGE_SIZES: &[u32] = &[
    16, 20, 22, 24, 28, 32, 40, 44, 48, 56, 60, 64, 80, 96, 100, 128, 160, 240, 256, 300, 320, 480,
    512, 600, 640, 1024, 1280, 1536, 2048, 3072, 4096, 8192, 16384,
];

pub fn snap_to_image_ladder(value: u32) -> u32 {
    let largest = IMAGE_SIZES[IMAGE_SIZES.len() - 1];
    IMAGE_SIZES
        .iter()
        .copied()
        .find(|rung| value <= *rung)
        .unwrap_or(largest)
}

pub fn parse_image_size(raw: Option<&str>) -> u32 {
    let Some(text) = raw else {
        return DEFAULT_IMAGE_SIZE;
    };
    let Ok(value) = text.parse::<u32>() else {
        return DEFAULT_IMAGE_SIZE;
    };
    snap_to_image_ladder(value)
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum AssetKind {
    Avatar,
    GuildIcon,
    Banner,
    Splash,
    EmbedSplash,
    Emoji,
    Sticker,
    Attachment,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum AssetExtension {
    Png,
    Jpeg,
    Webp,
    Gif,
    Apng,
    Avif,
    Heic,
    Heif,
    Jxl,
    Svg,
}

impl AssetExtension {
    pub fn parse(raw: &str) -> Option<Self> {
        match raw.to_ascii_lowercase().as_str() {
            "png" => Some(Self::Png),
            "jpg" | "jpeg" => Some(Self::Jpeg),
            "webp" => Some(Self::Webp),
            "gif" => Some(Self::Gif),
            "apng" => Some(Self::Apng),
            "avif" => Some(Self::Avif),
            "heic" => Some(Self::Heic),
            "heif" => Some(Self::Heif),
            "jxl" => Some(Self::Jxl),
            "svg" => Some(Self::Svg),
            _ => None,
        }
    }

    pub fn name(self) -> &'static str {
        match self {
            Self::Png => "png",
            Self::Jpeg => "jpeg",
            Self::Webp => "webp",
            Self::Gif => "gif",
            Self::Apng => "apng",
            Self::Avif => "avif",
            Self::Heic => "heic",
            Self::Heif => "heif",
            Self::Jxl => "jxl",
            Self::Svg => "svg",
        }
    }

    pub fn mime(self) -> &'static str {
        match self {
            Self::Png => "image/png",
            Self::Jpeg => "image/jpeg",
            Self::Webp => "image/webp",
            Self::Gif => "image/gif",
            Self::Apng => "image/apng",
            Self::Avif => "image/avif",
            Self::Heic => "image/heic",
            Self::Heif => "image/heif",
            Self::Jxl => "image/jxl",
            Self::Svg => "image/svg+xml",
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Dims {
    pub min: u32,
    pub max: u32,
}

pub fn dims_for(kind: AssetKind) -> Option<Dims> {
    match kind {
        AssetKind::Avatar | AssetKind::GuildIcon => Some(Dims {
            min: 128,
            max: 1024,
        }),
        AssetKind::Banner | AssetKind::Splash | AssetKind::EmbedSplash => Some(Dims {
            min: 480,
            max: 2400,
        }),
        AssetKind::Emoji => Some(Dims { min: 32, max: 512 }),
        AssetKind::Sticker => Some(Dims { min: 128, max: 512 }),
        AssetKind::Attachment => None,
    }
}

pub fn clamp_size(raw_target: u32, kind: AssetKind) -> u32 {
    let value = raw_target.max(1);
    dims_for(kind).map_or(value, |dims| value.clamp(dims.min, dims.max))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn asset_cache_key(raw: Option<&str>, kind: AssetKind) -> u32 {
        clamp_size(parse_image_size(raw), kind)
    }

    #[test]
    fn image_size_snaps_up_to_the_next_rung() {
        assert_eq!(128, parse_image_size(None));
        assert_eq!(640, parse_image_size(Some("640")));
        assert_eq!(1024, parse_image_size(Some("641")));
        assert_eq!(1024, parse_image_size(Some("1000")));
        assert_eq!(16, parse_image_size(Some("0")));
        assert_eq!(16384, parse_image_size(Some("99999")));
        assert_eq!(128, parse_image_size(Some("not-a-number")));
    }

    #[test]
    fn off_ladder_size_never_serves_fewer_pixels_than_requested() {
        for requested in 1..=4096u32 {
            let served = parse_image_size(Some(&requested.to_string()));
            assert!(
                served >= requested,
                "size={requested} served {served}, a silent downscale"
            );
        }
    }

    #[test]
    fn every_rung_snaps_to_itself() {
        for rung in IMAGE_SIZES {
            assert_eq!(*rung, parse_image_size(Some(&rung.to_string())));
        }
    }

    #[test]
    fn snapping_is_idempotent() {
        for raw in [0u32, 1, 17, 641, 1000, 4097, 99999] {
            let once = parse_image_size(Some(&raw.to_string()));
            let twice = parse_image_size(Some(&once.to_string()));
            assert_eq!(once, twice, "size={raw} did not settle");
        }
    }

    #[test]
    fn sub_minimum_sizes_collapse_to_one_avatar_cache_key() {
        let canonical = asset_cache_key(Some("128"), AssetKind::Avatar);
        assert_eq!(128, canonical);
        for below in IMAGE_SIZES.iter().take_while(|rung| **rung < 128) {
            assert_eq!(
                canonical,
                asset_cache_key(Some(&below.to_string()), AssetKind::Avatar),
                "size={below} minted a second avatar cache key"
            );
        }
    }

    #[test]
    fn sub_minimum_sizes_collapse_to_one_banner_cache_key() {
        let canonical = asset_cache_key(Some("480"), AssetKind::Banner);
        assert_eq!(480, canonical);
        for below in IMAGE_SIZES.iter().take_while(|rung| **rung < 480) {
            assert_eq!(
                canonical,
                asset_cache_key(Some(&below.to_string()), AssetKind::Banner),
                "size={below} minted a second banner cache key"
            );
        }
    }

    #[test]
    fn oversize_requests_collapse_onto_the_kind_maximum() {
        assert_eq!(1024, asset_cache_key(Some("99999"), AssetKind::Avatar));
        assert_eq!(1024, asset_cache_key(Some("1024"), AssetKind::Avatar));
        assert_eq!(512, asset_cache_key(Some("99999"), AssetKind::Emoji));
        assert_eq!(512, asset_cache_key(Some("99999"), AssetKind::Sticker));
    }

    #[test]
    fn animated_frame_default_allows_dense_short_clips() {
        assert_eq!(MAX_ANIMATED_FRAMES_DEFAULT, Limits::animated_frames());
    }
}
