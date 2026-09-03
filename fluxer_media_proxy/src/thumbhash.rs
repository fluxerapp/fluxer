// SPDX-License-Identifier: AGPL-3.0-or-later

use std::f64::consts::PI;
use thiserror::Error;

pub const MAX_DIM: u32 = 100;
const OPAQUE_ASPECT_LIMIT: u32 = 7;
const ALPHA_ASPECT_LIMIT: u32 = 5;

#[derive(Clone, Copy, Debug, Error, Eq, PartialEq)]
pub enum EncodeError {
    #[error("invalid dimensions")]
    InvalidDimensions,
}

#[derive(Debug)]
struct ChannelEncoded {
    dc: f64,
    ac: Vec<f64>,
    scale: f64,
}

pub fn encode_rgba(pixels: &[u8], w: u32, h: u32) -> Result<Vec<u8>, EncodeError> {
    if w == 0 || h == 0 || w > MAX_DIM || h > MAX_DIM {
        return Err(EncodeError::InvalidDimensions);
    }
    let total_pixels = w as usize * h as usize;
    if pixels.len() != total_pixels * 4 {
        return Err(EncodeError::InvalidDimensions);
    }

    let mut avg_r = 0.0;
    let mut avg_g = 0.0;
    let mut avg_b = 0.0;
    let mut avg_a = 0.0;
    for i in 0..total_pixels {
        let alpha = pixels[i * 4 + 3] as f64 / 255.0;
        avg_r += alpha / 255.0 * pixels[i * 4] as f64;
        avg_g += alpha / 255.0 * pixels[i * 4 + 1] as f64;
        avg_b += alpha / 255.0 * pixels[i * 4 + 2] as f64;
        avg_a += alpha;
    }
    if avg_a > 0.0 {
        avg_r /= avg_a;
        avg_g /= avg_a;
        avg_b /= avg_a;
    }
    let count = total_pixels as f64;
    let has_alpha = avg_a < count * 0.99;

    let mut l_ch = vec![0.0; total_pixels];
    let mut p_ch = vec![0.0; total_pixels];
    let mut q_ch = vec![0.0; total_pixels];
    let mut a_ch = vec![0.0; total_pixels];
    for i in 0..total_pixels {
        let alpha = pixels[i * 4 + 3] as f64 / 255.0;
        let r = avg_r * (1.0 - alpha) + alpha / 255.0 * pixels[i * 4] as f64;
        let g = avg_g * (1.0 - alpha) + alpha / 255.0 * pixels[i * 4 + 1] as f64;
        let b = avg_b * (1.0 - alpha) + alpha / 255.0 * pixels[i * 4 + 2] as f64;
        l_ch[i] = (r + g + b) / 3.0;
        p_ch[i] = (r + g) / 2.0 - b;
        q_ch[i] = r - g;
        a_ch[i] = alpha;
    }

    let max_wh = w.max(h) as f64;
    let l_limit = if has_alpha { 5.0 } else { 7.0 };
    let lx = 3u32.max((l_limit * w as f64 / max_wh).round().max(1.0) as u32);
    let ly = 3u32.max((l_limit * h as f64 / max_wh).round().max(1.0) as u32);

    let l_enc = encode_channel(&l_ch, w, h, lx, ly)?;
    let p_enc = encode_channel(&p_ch, w, h, 3, 3)?;
    let q_enc = encode_channel(&q_ch, w, h, 3, 3)?;
    let a_enc = if has_alpha {
        encode_channel(&a_ch, w, h, 5, 5)?
    } else {
        ChannelEncoded {
            dc: 1.0,
            ac: Vec::new(),
            scale: 0.0,
        }
    };

    let is_landscape = w > h;
    let l_dc_q = (63.0 * l_enc.dc).round() as u32;
    let p_dc_q = (31.5 + 31.5 * p_enc.dc).round() as u32;
    let q_dc_q = (31.5 + 31.5 * q_enc.dc).round() as u32;
    let l_scale_q = (31.0 * l_enc.scale).round() as u32;
    let header_24 =
        l_dc_q | (p_dc_q << 6) | (q_dc_q << 12) | (l_scale_q << 18) | ((has_alpha as u32) << 23);

    let lx_or_ly = if is_landscape { ly } else { lx };
    let p_scale_q = (63.0 * p_enc.scale).round() as u32;
    let q_scale_q = (63.0 * q_enc.scale).round() as u32;
    let header_16 = lx_or_ly | (p_scale_q << 3) | (q_scale_q << 9) | ((is_landscape as u32) << 15);

    let ac_count_total = l_enc.ac.len() + p_enc.ac.len() + q_enc.ac.len() + a_enc.ac.len();
    let ac_bytes = ac_count_total.div_ceil(2);
    let header_bytes = if has_alpha { 6 } else { 5 };
    let mut out = vec![0u8; header_bytes + ac_bytes];
    out[0] = (header_24 & 0xff) as u8;
    out[1] = ((header_24 >> 8) & 0xff) as u8;
    out[2] = ((header_24 >> 16) & 0xff) as u8;
    out[3] = (header_16 & 0xff) as u8;
    out[4] = ((header_16 >> 8) & 0xff) as u8;
    if has_alpha {
        let a_dc_q = (15.0 * a_enc.dc).round() as u32;
        let a_scale_q = (15.0 * a_enc.scale).round() as u32;
        out[5] = (a_dc_q | (a_scale_q << 4)) as u8;
    }

    let mut idx = 0u32;
    append_acs(&mut out[header_bytes..], &mut idx, &l_enc.ac);
    append_acs(&mut out[header_bytes..], &mut idx, &p_enc.ac);
    append_acs(&mut out[header_bytes..], &mut idx, &q_enc.ac);
    if has_alpha {
        append_acs(&mut out[header_bytes..], &mut idx, &a_enc.ac);
    }
    Ok(out)
}

pub fn represents_aspect_ratio(hash: &[u8], width: u32, height: u32) -> bool {
    if hash.len() < 5 || width == 0 || height == 0 {
        return false;
    }
    let limit = u64::from(if hash[2] & 0x80 != 0 {
        ALPHA_ASPECT_LIMIT
    } else {
        OPAQUE_ASPECT_LIMIT
    });
    let width = u64::from(width);
    let height = u64::from(height);
    width <= height * limit && height <= width * limit
}

fn append_acs(out: &mut [u8], idx: &mut u32, acs: &[f64]) {
    for ac in acs {
        let q = (15.0 * ac).round().clamp(0.0, 15.0) as u32;
        let byte_index = (*idx >> 1) as usize;
        let shift = (*idx & 1) << 2;
        out[byte_index] |= (q << shift) as u8;
        *idx += 1;
    }
}

fn encode_channel(
    channel: &[f64],
    w: u32,
    h: u32,
    nx: u32,
    ny: u32,
) -> Result<ChannelEncoded, EncodeError> {
    const MAX_BASIS: usize = 8;
    if w > MAX_DIM || h > MAX_DIM || nx as usize > MAX_BASIS || ny as usize > MAX_BASIS {
        return Err(EncodeError::InvalidDimensions);
    }
    let mut cos_x = vec![0.0; MAX_BASIS * MAX_DIM as usize];
    let mut cos_y = vec![0.0; MAX_BASIS * MAX_DIM as usize];
    for cx in 0..nx {
        for x in 0..w {
            cos_x[cx as usize * MAX_DIM as usize + x as usize] =
                (PI / w as f64 * cx as f64 * (x as f64 + 0.5)).cos();
        }
    }
    for cy in 0..ny {
        for y in 0..h {
            cos_y[cy as usize * MAX_DIM as usize + y as usize] =
                (PI / h as f64 * cy as f64 * (y as f64 + 0.5)).cos();
        }
    }

    let mut dc = 0.0;
    let mut scale = 0.0;
    let mut ac = Vec::new();
    for cy in 0..ny {
        let mut cx = 0;
        while cx * ny < nx * (ny - cy) {
            let mut f = 0.0;
            for y in 0..h {
                let cyf = cos_y[cy as usize * MAX_DIM as usize + y as usize];
                let row = y as usize * w as usize;
                for x in 0..w {
                    f += channel[row + x as usize]
                        * cos_x[cx as usize * MAX_DIM as usize + x as usize]
                        * cyf;
                }
            }
            f /= w as f64 * h as f64;
            if cx > 0 || cy > 0 {
                ac.push(f);
                if f.abs() > scale {
                    scale = f.abs();
                }
            } else {
                dc = f;
            }
            cx += 1;
        }
    }
    if scale > 0.0 {
        for value in &mut ac {
            *value = 0.5 + 0.5 / scale * *value;
        }
    }
    Ok(ChannelEncoded { dc, ac, scale })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encode_rejects_invalid_dimensions() {
        assert_eq!(Err(EncodeError::InvalidDimensions), encode_rgba(&[], 0, 1));
        assert_eq!(Err(EncodeError::InvalidDimensions), encode_rgba(&[], 1, 1));
        let pixels = [0u8; 101 * 4];
        assert_eq!(
            Err(EncodeError::InvalidDimensions),
            encode_rgba(&pixels, 101, 1)
        );
    }

    #[test]
    fn encode_produces_non_empty_deterministic_output() {
        let mut pixels = [0u8; 16 * 16 * 4];
        for px in pixels.chunks_exact_mut(4) {
            px[0] = 200;
            px[1] = 100;
            px[2] = 50;
            px[3] = 255;
        }
        let out1 = encode_rgba(&pixels, 16, 16).unwrap();
        let out2 = encode_rgba(&pixels, 16, 16).unwrap();
        assert!(out1.len() > 5);
        assert_eq!(out1, out2);
    }

    #[test]
    fn alpha_changes_header_layout() {
        let mut opaque = [0u8; 4 * 4 * 4];
        let mut translucent = [0u8; 4 * 4 * 4];
        for px in opaque.chunks_exact_mut(4) {
            px[3] = 255;
        }
        for px in translucent.chunks_exact_mut(4) {
            px[3] = 64;
        }
        let opaque_out = encode_rgba(&opaque, 4, 4).unwrap();
        let trans_out = encode_rgba(&translucent, 4, 4).unwrap();
        assert_eq!(0, opaque_out[2] & 0x80);
        assert_ne!(0, trans_out[2] & 0x80);
    }

    #[test]
    fn aspect_ratio_representability_follows_the_alpha_limit() {
        let mut opaque = [0u8; 4 * 4 * 4];
        let mut translucent = [0u8; 4 * 4 * 4];
        for px in opaque.chunks_exact_mut(4) {
            px[3] = 255;
        }
        for px in translucent.chunks_exact_mut(4) {
            px[3] = 64;
        }
        let opaque_out = encode_rgba(&opaque, 4, 4).unwrap();
        let trans_out = encode_rgba(&translucent, 4, 4).unwrap();
        assert!(represents_aspect_ratio(&opaque_out, 64, 64));
        assert!(represents_aspect_ratio(&opaque_out, 700, 100));
        assert!(!represents_aspect_ratio(&opaque_out, 800, 100));
        assert!(represents_aspect_ratio(&trans_out, 500, 100));
        assert!(!represents_aspect_ratio(&trans_out, 700, 100));
        assert!(!represents_aspect_ratio(&opaque_out, 64, 0));
        assert!(!represents_aspect_ratio(&opaque_out[..4], 64, 64));
    }

    #[test]
    fn synthetic_gradient_is_wire_stable() {
        let mut pixels = [0u8; 8 * 8 * 4];
        for y in 0..8 {
            for x in 0..8 {
                let i = (y * 8 + x) * 4;
                pixels[i] = (x * 32) as u8;
                pixels[i + 1] = (y * 32) as u8;
                pixels[i + 2] = 50;
                pixels[i + 3] = 255;
            }
        }
        let out = encode_rgba(&pixels, 8, 8).unwrap();
        assert_eq!(0, out[2] & 0x80);
        assert!(out.len() >= 5);
    }
}
