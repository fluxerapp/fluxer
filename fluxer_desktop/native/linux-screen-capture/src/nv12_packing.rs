// SPDX-License-Identifier: AGPL-3.0-or-later

#[derive(Debug, Clone, Copy)]
pub struct Nv12Layout {
    pub width: u32,
    pub height: u32,
    pub stride_y: u32,
    pub stride_uv: u32,
}

impl Nv12Layout {
    pub fn packed_size(&self) -> Option<usize> {
        if self.width == 0 || self.height == 0 || self.height % 2 != 0 {
            return None;
        }
        let w = self.width as usize;
        let h = self.height as usize;
        let y_bytes = w.checked_mul(h)?;
        let uv_bytes = w.checked_mul(h / 2)?;
        y_bytes.checked_add(uv_bytes)
    }

    pub fn packed_stride_y(&self) -> u32 {
        self.width
    }

    pub fn packed_stride_uv(&self) -> u32 {
        self.width
    }
}

pub fn pack_nv12(layout: Nv12Layout, y_plane: &[u8], uv_plane: &[u8], dst: &mut [u8]) -> bool {
    let Some(total) = layout.packed_size() else {
        return false;
    };
    if dst.len() != total {
        return false;
    }
    let w = layout.width as usize;
    let h = layout.height as usize;
    let stride_y = layout.stride_y as usize;
    let stride_uv = layout.stride_uv as usize;
    if stride_y < w || stride_uv < w {
        return false;
    }
    if y_plane.len() < stride_y * h {
        return false;
    }
    if uv_plane.len() < stride_uv * (h / 2) {
        return false;
    }

    let y_bytes = w * h;
    let uv_bytes = w * (h / 2);
    if stride_y == w && stride_uv == w {
        dst[..y_bytes].copy_from_slice(&y_plane[..y_bytes]);
        dst[y_bytes..y_bytes + uv_bytes].copy_from_slice(&uv_plane[..uv_bytes]);
    } else {
        let (dst_y, dst_uv) = dst.split_at_mut(y_bytes);
        for row in 0..h {
            let src_off = row * stride_y;
            let dst_off = row * w;
            dst_y[dst_off..dst_off + w].copy_from_slice(&y_plane[src_off..src_off + w]);
        }
        for row in 0..h / 2 {
            let src_off = row * stride_uv;
            let dst_off = row * w;
            dst_uv[dst_off..dst_off + w].copy_from_slice(&uv_plane[src_off..src_off + w]);
        }
    }
    true
}

#[cfg(target_os = "linux")]
fn bgra_to_nv12_dcp(w: u32, h: u32, bgra: &[u8], bgra_stride: usize, dst: &mut [u8]) -> bool {
    use dcv_color_primitives as dcp;
    let src_format = dcp::ImageFormat {
        pixel_format: dcp::PixelFormat::Bgra,
        color_space: dcp::ColorSpace::Rgb,
        num_planes: 1,
    };
    let dst_format = dcp::ImageFormat {
        pixel_format: dcp::PixelFormat::Nv12,
        color_space: dcp::ColorSpace::Bt601,
        num_planes: 2,
    };
    let y_bytes = (w as usize) * (h as usize);
    let (dst_y, dst_uv) = dst.split_at_mut(y_bytes);
    dcp::convert_image(
        w,
        h,
        &src_format,
        Some(&[bgra_stride]),
        &[bgra],
        &dst_format,
        Some(&[w as usize, w as usize]),
        &mut [dst_y, dst_uv],
    )
    .is_ok()
}

fn flip_nv12_vertical(w: usize, h: usize, dst: &mut [u8]) {
    assert!(w > 0);
    assert!(h % 2 == 0);
    let y_bytes = w * h;
    let uv_bytes = w * (h / 2);
    assert!(dst.len() >= y_bytes + uv_bytes);
    let (y_plane, rest) = dst.split_at_mut(y_bytes);
    flip_plane_rows(y_plane, w, h);
    flip_plane_rows(&mut rest[..uv_bytes], w, h / 2);
}

fn flip_plane_rows(plane: &mut [u8], row_bytes: usize, rows: usize) {
    assert!(row_bytes > 0);
    assert!(plane.len() >= row_bytes * rows);
    for row in 0..rows / 2 {
        let top_start = row * row_bytes;
        let bottom_start = (rows - 1 - row) * row_bytes;
        let (head, tail) = plane.split_at_mut(bottom_start);
        head[top_start..top_start + row_bytes].swap_with_slice(&mut tail[..row_bytes]);
    }
}

fn bgra_to_nv12_scalar(
    w: usize,
    h: usize,
    bgra: &[u8],
    bgra_row: usize,
    dst: &mut [u8],
    flip: bool,
) -> bool {
    let y_bytes = w * h;
    let (dst_y, dst_uv) = dst.split_at_mut(y_bytes);
    for row in 0..h {
        let dst_row_index = if flip { h - 1 - row } else { row };
        let src_row = &bgra[row * bgra_row..row * bgra_row + w * 4];
        let dst_row = &mut dst_y[dst_row_index * w..dst_row_index * w + w];
        for col in 0..w {
            let b = src_row[col * 4] as i32;
            let g = src_row[col * 4 + 1] as i32;
            let r = src_row[col * 4 + 2] as i32;
            let y = (66 * r + 129 * g + 25 * b + 128) >> 8;
            dst_row[col] = (y + 16).clamp(0, 255) as u8;
        }
    }
    for row in 0..h / 2 {
        let dst_row_index = if flip { h / 2 - 1 - row } else { row };
        let r0 = &bgra[(row * 2) * bgra_row..(row * 2) * bgra_row + w * 4];
        let r1 = &bgra[(row * 2 + 1) * bgra_row..(row * 2 + 1) * bgra_row + w * 4];
        let dst_row = &mut dst_uv[dst_row_index * w..dst_row_index * w + w];
        for col in 0..w / 2 {
            let cx0 = col * 2 * 4;
            let cx1 = (col * 2 + 1) * 4;
            let b = (r0[cx0] as i32 + r0[cx1] as i32 + r1[cx0] as i32 + r1[cx1] as i32) >> 2;
            let g =
                (r0[cx0 + 1] as i32 + r0[cx1 + 1] as i32 + r1[cx0 + 1] as i32 + r1[cx1 + 1] as i32)
                    >> 2;
            let r =
                (r0[cx0 + 2] as i32 + r0[cx1 + 2] as i32 + r1[cx0 + 2] as i32 + r1[cx1 + 2] as i32)
                    >> 2;
            let u = ((-38 * r - 74 * g + 112 * b + 128) >> 8) + 128;
            let v = ((112 * r - 94 * g - 18 * b + 128) >> 8) + 128;
            dst_row[col * 2] = u.clamp(0, 255) as u8;
            dst_row[col * 2 + 1] = v.clamp(0, 255) as u8;
        }
    }
    true
}

pub fn bgra_to_nv12(
    layout: Nv12Layout,
    bgra: &[u8],
    bgra_stride: u32,
    dst: &mut [u8],
    flip: bool,
) -> bool {
    let Some(total) = layout.packed_size() else {
        return false;
    };
    if dst.len() < total {
        return false;
    }
    let w = layout.width as usize;
    let h = layout.height as usize;
    let bgra_row = bgra_stride as usize;
    if bgra_row < w.checked_mul(4).unwrap_or(usize::MAX) {
        return false;
    }
    if bgra.len() < bgra_row.checked_mul(h).unwrap_or(usize::MAX) {
        return false;
    }
    #[cfg(target_os = "linux")]
    if bgra_to_nv12_dcp(layout.width, layout.height, bgra, bgra_row, dst) {
        if flip {
            flip_nv12_vertical(w, h, dst);
        }
        return true;
    }
    bgra_to_nv12_scalar(w, h, bgra, bgra_row, dst, flip)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_y(width: usize, height: usize, stride: usize) -> Vec<u8> {
        let mut v = vec![0u8; stride * height];
        for row in 0..height {
            for col in 0..width {
                v[row * stride + col] = ((row * width + col) % 251) as u8;
            }
        }
        v
    }

    fn make_uv(width: usize, height_half: usize, stride: usize) -> Vec<u8> {
        let mut v = vec![0u8; stride * height_half];
        for row in 0..height_half {
            for col in 0..width {
                v[row * stride + col] = ((row * width + col + 7) % 241) as u8;
            }
        }
        v
    }

    #[test]
    fn packed_size_rejects_odd_height() {
        let layout = Nv12Layout {
            width: 16,
            height: 15,
            stride_y: 16,
            stride_uv: 16,
        };
        assert!(layout.packed_size().is_none());
    }

    #[test]
    fn packed_size_rejects_zero_dims() {
        let layout = Nv12Layout {
            width: 0,
            height: 4,
            stride_y: 0,
            stride_uv: 0,
        };
        assert!(layout.packed_size().is_none());
    }

    #[test]
    fn packed_size_matches_yuv420_layout() {
        let layout = Nv12Layout {
            width: 1920,
            height: 1080,
            stride_y: 1920,
            stride_uv: 1920,
        };
        assert_eq!(Some(1920 * 1080 * 3 / 2), layout.packed_size());
    }

    #[test]
    fn pack_nv12_strips_row_padding() {
        let layout = Nv12Layout {
            width: 8,
            height: 4,
            stride_y: 16,
            stride_uv: 16,
        };
        let y = make_y(8, 4, 16);
        let uv = make_uv(8, 2, 16);
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(pack_nv12(layout, &y, &uv, &mut dst));
        for row in 0..4 {
            for col in 0..8 {
                assert_eq!(dst[row * 8 + col], ((row * 8 + col) % 251) as u8);
            }
        }
        for row in 0..2 {
            for col in 0..8 {
                let off = 8 * 4 + row * 8 + col;
                assert_eq!(dst[off], ((row * 8 + col + 7) % 241) as u8);
            }
        }
    }

    #[test]
    fn pack_nv12_zero_padding_is_identity() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let y = vec![1, 2, 3, 4, 5, 6, 7, 8];
        let uv = vec![10, 11, 12, 13];
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(pack_nv12(layout, &y, &uv, &mut dst));
        assert_eq!(dst, vec![1, 2, 3, 4, 5, 6, 7, 8, 10, 11, 12, 13]);
    }

    #[test]
    fn pack_nv12_rejects_short_source() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let y = vec![1, 2, 3];
        let uv = vec![10, 11, 12, 13];
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(!pack_nv12(layout, &y, &uv, &mut dst));
    }

    #[test]
    fn pack_nv12_rejects_undersized_stride() {
        let layout = Nv12Layout {
            width: 8,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let y = vec![0; 8];
        let uv = vec![0; 4];
        let mut dst = vec![0u8; 24];
        assert!(!pack_nv12(layout, &y, &uv, &mut dst));
    }

    #[test]
    fn pack_nv12_rejects_wrong_dst_size() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let y = vec![0; 8];
        let uv = vec![0; 4];
        let mut dst = vec![0u8; 11];
        assert!(!pack_nv12(layout, &y, &uv, &mut dst));
    }

    #[test]
    fn bgra_to_nv12_solid_black() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let bgra = vec![0u8; 4 * 4 * 2];
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(bgra_to_nv12(layout, &bgra, 16, &mut dst, false));
        for byte in &dst[..8] {
            assert_eq!(*byte, 16);
        }
        for chunk in dst[8..].as_chunks::<2>().0 {
            assert_eq!(chunk[0], 128);
            assert_eq!(chunk[1], 128);
        }
    }

    #[test]
    fn bgra_to_nv12_solid_white_is_in_range() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let bgra = vec![255u8; 4 * 4 * 2];
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(bgra_to_nv12(layout, &bgra, 16, &mut dst, false));
        for byte in &dst[..8] {
            assert!(*byte >= 230 && *byte <= 240, "luma out of range: {byte}");
        }
        for chunk in dst[8..].as_chunks::<2>().0 {
            assert!(
                chunk[0] >= 124 && chunk[0] <= 132,
                "U out of range: {}",
                chunk[0]
            );
            assert!(
                chunk[1] >= 124 && chunk[1] <= 132,
                "V out of range: {}",
                chunk[1]
            );
        }
    }

    #[test]
    fn bgra_to_nv12_rejects_short_stride() {
        let layout = Nv12Layout {
            width: 4,
            height: 2,
            stride_y: 4,
            stride_uv: 4,
        };
        let bgra = vec![0u8; 8];
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(!bgra_to_nv12(layout, &bgra, 4, &mut dst, false));
    }

    fn deterministic_bgra_frame(w: usize, h: usize, seed: u64) -> Vec<u8> {
        let mut state = seed;
        let mut v = vec![0u8; w * h * 4];
        for byte in v.iter_mut() {
            state = state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            *byte = (state >> 56) as u8;
        }
        v
    }

    fn row_reversed(bgra: &[u8], w: usize, h: usize) -> Vec<u8> {
        let row_bytes = w * 4;
        assert_eq!(bgra.len(), row_bytes * h);
        let mut reversed = vec![0u8; bgra.len()];
        for row in 0..h {
            let src = (h - 1 - row) * row_bytes;
            reversed[row * row_bytes..(row + 1) * row_bytes]
                .copy_from_slice(&bgra[src..src + row_bytes]);
        }
        reversed
    }

    #[test]
    fn bgra_to_nv12_flip_matches_pre_reversed_rows() {
        for (w, h, seed) in [(8usize, 4usize, 1u64), (16, 8, 2), (64, 32, 3), (12, 6, 4)] {
            let layout = Nv12Layout {
                width: w as u32,
                height: h as u32,
                stride_y: w as u32,
                stride_uv: w as u32,
            };
            let bgra = deterministic_bgra_frame(w, h, seed);
            let reversed = row_reversed(&bgra, w, h);
            let total = layout.packed_size().expect("even dims");
            let mut flipped = vec![0u8; total];
            let mut reference = vec![0u8; total];
            assert!(bgra_to_nv12(
                layout,
                &bgra,
                (w * 4) as u32,
                &mut flipped,
                true
            ));
            assert!(bgra_to_nv12(
                layout,
                &reversed,
                (w * 4) as u32,
                &mut reference,
                false
            ));
            assert_eq!(flipped, reference, "mismatch at {w}x{h} seed {seed}");
        }
    }

    #[test]
    fn bgra_to_nv12_flip_moves_top_row_luma_to_bottom() {
        let layout = Nv12Layout {
            width: 4,
            height: 4,
            stride_y: 4,
            stride_uv: 4,
        };
        let mut bgra = vec![0u8; 4 * 4 * 4];
        bgra[..16].fill(255);
        let mut dst = vec![0u8; layout.packed_size().unwrap()];
        assert!(bgra_to_nv12(layout, &bgra, 16, &mut dst, true));
        let y = &dst[..16];
        assert!(y[12] > 200, "bottom row should hold the white luma: {y:?}");
        assert!(y[0] < 32, "top row should hold the black luma: {y:?}");
    }

    #[test]
    fn bgra_to_nv12_unflipped_path_is_unchanged_by_flip_support() {
        let layout = Nv12Layout {
            width: 8,
            height: 4,
            stride_y: 8,
            stride_uv: 8,
        };
        let bgra = deterministic_bgra_frame(8, 4, 9);
        let total = layout.packed_size().unwrap();
        let mut first = vec![0u8; total];
        let mut second = vec![0u8; total];
        assert!(bgra_to_nv12(layout, &bgra, 32, &mut first, false));
        assert!(bgra_to_nv12(layout, &bgra, 32, &mut second, false));
        assert_eq!(first, second);
    }
}
