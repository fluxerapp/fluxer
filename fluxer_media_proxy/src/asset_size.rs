// SPDX-License-Identifier: AGPL-3.0-or-later

pub use crate::constants::{clamp_size, parse_image_size};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::constants::{AssetKind, DEFAULT_IMAGE_SIZE, IMAGE_SIZES};

    #[test]
    fn unparsable_sizes_fall_back_to_the_default() {
        assert_eq!(DEFAULT_IMAGE_SIZE, parse_image_size(None));
        assert_eq!(DEFAULT_IMAGE_SIZE, parse_image_size(Some("")));
        assert_eq!(DEFAULT_IMAGE_SIZE, parse_image_size(Some("-1")));
        assert_eq!(DEFAULT_IMAGE_SIZE, parse_image_size(Some("99999999999")));
        assert_eq!(DEFAULT_IMAGE_SIZE, parse_image_size(Some("not-a-number")));
    }

    #[test]
    fn parsable_sizes_snap_up_to_the_next_rung() {
        assert_eq!(16, parse_image_size(Some("0")));
        assert_eq!(640, parse_image_size(Some("640")));
        assert_eq!(1024, parse_image_size(Some("641")));
        assert_eq!(1024, parse_image_size(Some("777")));
        assert_eq!(1024, parse_image_size(Some("1000")));
        assert_eq!(16384, parse_image_size(Some("99999")));
        for size in IMAGE_SIZES {
            assert_eq!(*size, parse_image_size(Some(&size.to_string())));
        }
        assert_eq!(16384, parse_image_size(Some("16384")));
    }

    #[test]
    fn an_off_ladder_size_never_serves_fewer_pixels_than_requested() {
        for requested in 1..=4096u32 {
            let served = parse_image_size(Some(&requested.to_string()));
            assert!(
                served >= requested,
                "size={requested} served {served}, a silent downscale"
            );
        }
    }

    #[test]
    fn snapping_is_idempotent() {
        for raw in [0u32, 1, 17, 641, 777, 1000, 4097, 99999] {
            let once = parse_image_size(Some(&raw.to_string()));
            let twice = parse_image_size(Some(&once.to_string()));
            assert_eq!(once, twice, "size={raw} did not settle");
        }
    }

    #[test]
    fn clamping_respects_kind_bounds_and_leaves_attachments_alone() {
        assert_eq!(128, clamp_size(1, AssetKind::Avatar));
        assert_eq!(1024, clamp_size(4096, AssetKind::Avatar));
        assert_eq!(480, clamp_size(16, AssetKind::Banner));
        assert_eq!(512, clamp_size(4096, AssetKind::Emoji));
        assert_eq!(4096, clamp_size(4096, AssetKind::Attachment));
        assert_eq!(1, clamp_size(0, AssetKind::Attachment));
    }
}
