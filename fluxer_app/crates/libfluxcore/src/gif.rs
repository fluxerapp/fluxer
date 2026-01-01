/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

use gif::{ColorOutput, DecodeOptions, DisposalMethod, Encoder as GifEncoder, Frame, Repeat};
use std::borrow::Cow;
use std::collections::HashMap;
use std::io::Cursor;
use wasm_bindgen::prelude::*;

enum EncodeError {
    TooManyColors,
    Js(JsValue),
}

impl From<JsValue> for EncodeError {
    fn from(value: JsValue) -> Self {
        Self::Js(value)
    }
}

#[wasm_bindgen]
pub fn crop_and_rotate_gif(
    input: &[u8],
    x: u32,
    y: u32,
    width: u32,
    height: u32,
    rotation_deg: u32,
    resize_width: Option<u32>,
    resize_height: Option<u32>,
) -> Result<Box<[u8]>, JsValue> {
    match process_gif(
        input,
        x,
        y,
        width,
        height,
        rotation_deg,
        resize_width,
        resize_height,
        EncoderMode::Palette,
    ) {
        Ok(bytes) => Ok(bytes),
        Err(EncodeError::TooManyColors) => process_gif(
            input,
            x,
            y,
            width,
            height,
            rotation_deg,
            resize_width,
            resize_height,
            EncoderMode::Quantized,
        )
        .map_err(|err| match err {
            EncodeError::Js(js) => js,
            EncodeError::TooManyColors => {
                JsValue::from_str("GIF contains more than 256 unique colors")
            }
        }),
        Err(EncodeError::Js(js)) => Err(js),
    }
}

enum EncoderMode {
    Palette,
    Quantized,
}

fn process_gif(
    input: &[u8],
    x: u32,
    y: u32,
    width: u32,
    height: u32,
    rotation_deg: u32,
    resize_width: Option<u32>,
    resize_height: Option<u32>,
    mode: EncoderMode,
) -> Result<Box<[u8]>, EncodeError> {
    let mut decoder = create_decoder(input)?;

    let screen_width = decoder.width() as u32;
    let screen_height = decoder.height() as u32;

    let crop_x = x.min(screen_width);
    let crop_y = y.min(screen_height);
    let crop_w = width.min(screen_width - crop_x);
    let crop_h = height.min(screen_height - crop_y);

    if crop_w == 0 || crop_h == 0 {
        return Err(EncodeError::Js(JsValue::from_str("Crop area is empty")));
    }

    let rotation = rotation_deg.rem_euclid(360);

    let (base_w, base_h) = match rotation {
        90 | 270 => (crop_h, crop_w),
        _ => (crop_w, crop_h),
    };

    let (target_w, target_h) = match (
        resize_width.filter(|w| *w > 0),
        resize_height.filter(|h| *h > 0),
    ) {
        (Some(w), Some(h)) => (w, h),
        _ => (base_w, base_h),
    };

    if target_w == 0 || target_h == 0 {
        return Err(EncodeError::Js(JsValue::from_str(
            "Target dimensions are empty",
        )));
    }

    if crop_x == 0
        && crop_y == 0
        && crop_w == screen_width
        && crop_h == screen_height
        && rotation == 0
        && target_w == screen_width
        && target_h == screen_height
    {
        return Ok(input.to_vec().into_boxed_slice());
    }

    let mut frame_encoder = FrameEncoder::new(mode, target_w as u16, target_h as u16)?;

    let mut canvas = vec![0u8; (screen_width * screen_height * 4) as usize];
    let mut previous_canvas: Option<Vec<u8>> = None;

    let mut processed_any = false;
    const MAX_TOTAL_PIXELS: u64 = 200_000_000;
    let mut processed_pixels: u64 = 0;

    while let Some(frame) = decoder
        .read_next_frame()
        .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("gif read_next_frame: {e}"))))?
    {
        processed_any = true;

        if frame.dispose == DisposalMethod::Previous {
            previous_canvas = Some(canvas.clone());
        }

        draw_frame_on_canvas(
            &mut canvas,
            screen_width,
            frame.left,
            frame.top,
            frame.width,
            frame.height,
            frame.buffer.as_ref(),
        );

        let (cw, ch) = (crop_w as usize, crop_h as usize);
        let cropped = crop_rgba(
            &canvas,
            screen_width as usize,
            screen_height as usize,
            crop_x as usize,
            crop_y as usize,
            cw,
            ch,
        )?;

        let (rotated, rw, rh) = match rotation {
            90 => rotate_rgba_90(&cropped, cw, ch),
            180 => rotate_rgba_180(&cropped, cw, ch),
            270 => rotate_rgba_270(&cropped, cw, ch),
            _ => (cropped, cw, ch),
        };

        let (final_rgba, _fw, _fh) = if target_w as usize != rw || target_h as usize != rh {
            let resized =
                resize_rgba_nearest(&rotated, rw, rh, target_w as usize, target_h as usize);
            (resized, target_w as usize, target_h as usize)
        } else {
            (rotated, rw, rh)
        };

        processed_pixels += (final_rgba.len() / 4) as u64;
        if processed_pixels > MAX_TOTAL_PIXELS {
            return Err(EncodeError::Js(JsValue::from_str(
                "Animated GIF is too large to crop. Try reducing its dimensions or number of frames.",
            )));
        }

        frame_encoder.write_frame(final_rgba, frame.delay)?;

        match frame.dispose {
            DisposalMethod::Background => {
                clear_rect(
                    &mut canvas,
                    screen_width,
                    frame.left,
                    frame.top,
                    frame.width,
                    frame.height,
                );
            }
            DisposalMethod::Previous => {
                if let Some(prev) = previous_canvas.take() {
                    canvas = prev;
                }
            }
            _ => {}
        }
    }

    if !processed_any {
        return Err(EncodeError::Js(JsValue::from_str("GIF has no frames")));
    }

    frame_encoder.finish()
}

fn draw_frame_on_canvas(
    canvas: &mut [u8],
    canvas_width: u32,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    buffer: &[u8],
) {
    let fw = width as usize;
    let fh = height as usize;
    let fx = left as usize;
    let fy = top as usize;
    let cw = canvas_width as usize;

    for row in 0..fh {
        let canvas_y = fy + row;
        let canvas_offset = (canvas_y * cw + fx) * 4;
        let frame_offset = row * fw * 4;

        let frame_row = &buffer[frame_offset..frame_offset + fw * 4];
        let canvas_row = &mut canvas[canvas_offset..canvas_offset + fw * 4];

        for i in 0..fw {
            let pixel_idx = i * 4;
            let alpha = frame_row[pixel_idx + 3];
            if alpha > 0 {
                canvas_row[pixel_idx] = frame_row[pixel_idx];
                canvas_row[pixel_idx + 1] = frame_row[pixel_idx + 1];
                canvas_row[pixel_idx + 2] = frame_row[pixel_idx + 2];
                canvas_row[pixel_idx + 3] = frame_row[pixel_idx + 3];
            }
        }
    }
}

fn clear_rect(canvas: &mut [u8], canvas_width: u32, x: u16, y: u16, w: u16, h: u16) {
    let cw = canvas_width as usize;
    let x = x as usize;
    let y = y as usize;
    let w = w as usize;
    let h = h as usize;

    for row in 0..h {
        let canvas_y = y + row;
        let offset = (canvas_y * cw + x) * 4;
        for i in 0..w {
            let idx = offset + i * 4;
            canvas[idx] = 0;
            canvas[idx + 1] = 0;
            canvas[idx + 2] = 0;
            canvas[idx + 3] = 0;
        }
    }
}

fn crop_rgba(
    src: &[u8],
    src_w: usize,
    src_h: usize,
    x: usize,
    y: usize,
    w: usize,
    h: usize,
) -> Result<Vec<u8>, JsValue> {
    if x + w > src_w || y + h > src_h {
        return Err(JsValue::from_str("Crop rect out of bounds"));
    }

    let mut dst = vec![0u8; w * h * 4];

    for row in 0..h {
        let src_y = y + row;
        let src_offset = (src_y * src_w + x) * 4;
        let dst_offset = row * w * 4;
        dst[dst_offset..dst_offset + w * 4].copy_from_slice(&src[src_offset..src_offset + w * 4]);
    }

    Ok(dst)
}

fn rotate_rgba_90(src: &[u8], src_w: usize, src_h: usize) -> (Vec<u8>, usize, usize) {
    let dst_w = src_h;
    let dst_h = src_w;
    let mut dst = vec![0u8; dst_w * dst_h * 4];

    for y in 0..src_h {
        for x in 0..src_w {
            let src_idx = (y * src_w + x) * 4;
            let dst_x = src_h - 1 - y;
            let dst_y = x;
            let dst_idx = (dst_y * dst_w + dst_x) * 4;
            dst[dst_idx..dst_idx + 4].copy_from_slice(&src[src_idx..src_idx + 4]);
        }
    }

    (dst, dst_w, dst_h)
}

fn rotate_rgba_180(src: &[u8], src_w: usize, src_h: usize) -> (Vec<u8>, usize, usize) {
    let mut dst = vec![0u8; src.len()];
    for y in 0..src_h {
        for x in 0..src_w {
            let src_idx = (y * src_w + x) * 4;
            let dst_x = src_w - 1 - x;
            let dst_y = src_h - 1 - y;
            let dst_idx = (dst_y * src_w + dst_x) * 4;
            dst[dst_idx..dst_idx + 4].copy_from_slice(&src[src_idx..src_idx + 4]);
        }
    }
    (dst, src_w, src_h)
}

fn rotate_rgba_270(src: &[u8], src_w: usize, src_h: usize) -> (Vec<u8>, usize, usize) {
    let dst_w = src_h;
    let dst_h = src_w;
    let mut dst = vec![0u8; dst_w * dst_h * 4];

    for y in 0..src_h {
        for x in 0..src_w {
            let src_idx = (y * src_w + x) * 4;
            let dst_x = y;
            let dst_y = dst_h - 1 - x;
            let dst_idx = (dst_y * dst_w + dst_x) * 4;
            dst[dst_idx..dst_idx + 4].copy_from_slice(&src[src_idx..src_idx + 4]);
        }
    }

    (dst, dst_w, dst_h)
}

fn resize_rgba_nearest(
    src: &[u8],
    src_w: usize,
    src_h: usize,
    dst_w: usize,
    dst_h: usize,
) -> Vec<u8> {
    let mut dst = vec![0u8; dst_w * dst_h * 4];

    for dy in 0..dst_h {
        let sy = dy * src_h / dst_h;
        for dx in 0..dst_w {
            let sx = dx * src_w / dst_w;
            let src_idx = (sy * src_w + sx) * 4;
            let dst_idx = (dy * dst_w + dx) * 4;
            dst[dst_idx..dst_idx + 4].copy_from_slice(&src[src_idx..src_idx + 4]);
        }
    }

    dst
}

fn create_decoder(input: &[u8]) -> Result<gif::Decoder<Cursor<&[u8]>>, EncodeError> {
    let cursor = Cursor::new(input);
    let mut options = DecodeOptions::new();
    options.set_color_output(ColorOutput::RGBA);
    options
        .read_info(cursor)
        .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("gif read_info: {e}"))))
}

enum FrameEncoder {
    Palette(PaletteFrameEncoder),
    Quantized(QuantizedFrameEncoder),
}

impl FrameEncoder {
    fn new(mode: EncoderMode, width: u16, height: u16) -> Result<Self, EncodeError> {
        match mode {
            EncoderMode::Palette => PaletteFrameEncoder::new(width, height).map(Self::Palette),
            EncoderMode::Quantized => {
                QuantizedFrameEncoder::new(width, height).map(Self::Quantized)
            }
        }
    }

    fn write_frame(&mut self, rgba: Vec<u8>, delay: u16) -> Result<(), EncodeError> {
        match self {
            Self::Palette(enc) => enc.write_frame(rgba, delay),
            Self::Quantized(enc) => enc.write_frame(rgba, delay),
        }
    }

    fn finish(self) -> Result<Box<[u8]>, EncodeError> {
        match self {
            Self::Palette(enc) => enc.finish(),
            Self::Quantized(enc) => enc.finish(),
        }
    }
}

struct PaletteFrameEncoder {
    encoder: GifEncoder<Cursor<Vec<u8>>>,
    width: u16,
    height: u16,
}

impl PaletteFrameEncoder {
    fn new(width: u16, height: u16) -> Result<Self, EncodeError> {
        let cursor = Cursor::new(Vec::new());
        let mut encoder = GifEncoder::new(cursor, width, height, &[])
            .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("GifEncoder::new: {e}"))))?;
        encoder
            .set_repeat(Repeat::Infinite)
            .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("set_repeat: {e}"))))?;
        Ok(Self {
            encoder,
            width,
            height,
        })
    }

    fn write_frame(&mut self, rgba: Vec<u8>, delay: u16) -> Result<(), EncodeError> {
        let PaletteFrameData {
            indices,
            palette,
            transparent_index,
        } = PaletteFrameData::from_rgba(&rgba)?;

        let mut frame = Frame::default();
        frame.width = self.width;
        frame.height = self.height;
        frame.delay = delay;
        frame.buffer = Cow::Owned(indices);
        frame.palette = Some(palette);
        frame.transparent = transparent_index;
        self.encoder.write_frame(&frame).map_err(map_encoding_error)
    }

    fn finish(self) -> Result<Box<[u8]>, EncodeError> {
        let cursor = self.encoder.into_inner().map_err(map_io_error)?;
        Ok(cursor.into_inner().into_boxed_slice())
    }
}

struct PaletteFrameData {
    indices: Vec<u8>,
    palette: Vec<u8>,
    transparent_index: Option<u8>,
}

impl PaletteFrameData {
    fn from_rgba(rgba: &[u8]) -> Result<Self, EncodeError> {
        let mut palette = Vec::with_capacity(256 * 3);
        let mut color_to_index = HashMap::with_capacity(256);
        let mut transparent_index = None;
        let mut indices = Vec::with_capacity(rgba.len() / 4);

        for pixel in rgba.chunks_exact(4) {
            let idx = if pixel[3] == 0 {
                if let Some(idx) = transparent_index {
                    idx
                } else {
                    let next_index = palette.len() / 3;
                    if next_index >= 256 {
                        return Err(EncodeError::TooManyColors);
                    }
                    palette.extend_from_slice(&[0, 0, 0]);
                    let idx = next_index as u8;
                    transparent_index = Some(idx);
                    idx
                }
            } else {
                let key = [pixel[0], pixel[1], pixel[2]];
                if let Some(&idx) = color_to_index.get(&key) {
                    idx
                } else {
                    let next_index = palette.len() / 3;
                    if next_index >= 256 {
                        return Err(EncodeError::TooManyColors);
                    }
                    palette.extend_from_slice(&key);
                    let idx = next_index as u8;
                    color_to_index.insert(key, idx);
                    idx
                }
            };
            indices.push(idx);
        }

        if palette.is_empty() {
            palette.extend_from_slice(&[0, 0, 0]);
        }

        Ok(Self {
            indices,
            palette,
            transparent_index,
        })
    }
}

struct QuantizedFrameEncoder {
    encoder: GifEncoder<Cursor<Vec<u8>>>,
    width: u16,
    height: u16,
}

impl QuantizedFrameEncoder {
    fn new(width: u16, height: u16) -> Result<Self, EncodeError> {
        let cursor = Cursor::new(Vec::new());
        let mut encoder = GifEncoder::new(cursor, width, height, &[])
            .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("GifEncoder::new: {e}"))))?;
        encoder
            .set_repeat(Repeat::Infinite)
            .map_err(|e| EncodeError::Js(JsValue::from_str(&format!("set_repeat: {e}"))))?;
        Ok(Self {
            encoder,
            width,
            height,
        })
    }

    fn write_frame(&mut self, mut rgba: Vec<u8>, delay: u16) -> Result<(), EncodeError> {
        let mut frame = Frame::from_rgba_speed(self.width, self.height, &mut rgba, 10);
        frame.delay = delay;
        self.encoder.write_frame(&frame).map_err(map_encoding_error)
    }

    fn finish(self) -> Result<Box<[u8]>, EncodeError> {
        let cursor = self.encoder.into_inner().map_err(map_io_error)?;
        Ok(cursor.into_inner().into_boxed_slice())
    }
}

fn map_encoding_error(err: gif::EncodingError) -> EncodeError {
    EncodeError::Js(JsValue::from_str(&format!("gif encode: {err}")))
}

fn map_io_error(err: std::io::Error) -> EncodeError {
    EncodeError::Js(JsValue::from_str(&format!("gif io: {err}")))
}
