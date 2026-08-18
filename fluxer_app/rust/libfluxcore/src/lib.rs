// SPDX-License-Identifier: AGPL-3.0-or-later

mod formats;
mod rgba;
mod zstd_frame;
mod zstd_stream;

use formats::is_animated_image_bytes;
use rgba::{TransformRequest, crop_rotate_rgba_alloc};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[allow(clippy::too_many_arguments)]
pub fn crop_rotate_rgba_raw(
    input: &[u8],
    src_width: u32,
    src_height: u32,
    x: u32,
    y: u32,
    width: u32,
    height: u32,
    rotation_deg: u32,
    resize_width: Option<u32>,
    resize_height: Option<u32>,
) -> Result<Vec<u8>, JsValue> {
    crop_rotate_rgba_alloc(
        input,
        TransformRequest {
            src_width,
            src_height,
            x,
            y,
            width,
            height,
            rotation_deg,
            resize_width: optional_dimension_to_abi(resize_width),
            resize_height: optional_dimension_to_abi(resize_height),
        },
    )
    .map_err(|error| JsValue::from_str(error.message()))
}

#[wasm_bindgen]
pub fn decompress_zstd_frame(input: &[u8]) -> Result<Vec<u8>, JsValue> {
    zstd_frame::decompress(input).map_err(zstd_error_to_js)
}

#[wasm_bindgen]
pub fn create_zstd_stream_decoder() -> Result<u32, JsValue> {
    let decoder = zstd_stream::ZstdStreamDecoder::new().map_err(zstd_error_to_js)?;
    leak_zstd_stream_decoder(decoder)
        .ok_or_else(|| JsValue::from_str("libfluxcore zstd stream decoder allocation failed"))
}

#[wasm_bindgen]
pub fn free_zstd_stream_decoder(decoder_ptr: u32) {
    if decoder_ptr == 0 {
        return;
    }
    unsafe {
        drop(Box::from_raw(
            decoder_ptr as usize as *mut zstd_stream::ZstdStreamDecoder,
        ));
    }
}

#[wasm_bindgen]
pub fn decompress_zstd_stream_chunk(decoder_ptr: u32, input: &[u8]) -> Result<Vec<u8>, JsValue> {
    if decoder_ptr == 0 {
        return Err(JsValue::from_str("zstd stream decoder is missing"));
    }
    let decoder = unsafe { &mut *(decoder_ptr as usize as *mut zstd_stream::ZstdStreamDecoder) };
    decoder.decompress_chunk(input).map_err(zstd_error_to_js)
}

#[wasm_bindgen]
pub fn create_zstd_stream_encoder(level: i32) -> Result<u32, JsValue> {
    let encoder = zstd_stream::ZstdStreamEncoder::new(level).map_err(zstd_error_to_js)?;
    leak_zstd_stream_encoder(encoder)
        .ok_or_else(|| JsValue::from_str("libfluxcore zstd stream encoder allocation failed"))
}

#[wasm_bindgen]
pub fn free_zstd_stream_encoder(encoder_ptr: u32) {
    if encoder_ptr == 0 {
        return;
    }
    unsafe {
        drop(Box::from_raw(
            encoder_ptr as usize as *mut zstd_stream::ZstdStreamEncoder,
        ));
    }
}

#[wasm_bindgen]
pub fn compress_zstd_stream_chunk(encoder_ptr: u32, input: &[u8]) -> Result<Vec<u8>, JsValue> {
    if encoder_ptr == 0 {
        return Err(JsValue::from_str("zstd stream encoder is missing"));
    }
    let encoder = unsafe { &mut *(encoder_ptr as usize as *mut zstd_stream::ZstdStreamEncoder) };
    encoder.compress_chunk(input).map_err(zstd_error_to_js)
}

#[wasm_bindgen]
pub fn is_animated_image(input: &[u8]) -> bool {
    is_animated_image_bytes(input)
}

fn optional_dimension_to_abi(value: Option<u32>) -> u32 {
    value.filter(|dimension| *dimension > 0).unwrap_or(u32::MAX)
}

fn zstd_error_to_js(error: zstd_frame::ZstdError) -> JsValue {
    JsValue::from_str(error.message())
}

fn leak_zstd_stream_decoder(decoder: zstd_stream::ZstdStreamDecoder) -> Option<u32> {
    let ptr = Box::into_raw(Box::new(decoder));
    match u32::try_from(ptr as usize) {
        Ok(ptr) => Some(ptr),
        Err(_) => {
            unsafe {
                drop(Box::from_raw(ptr));
            }
            None
        }
    }
}

fn leak_zstd_stream_encoder(encoder: zstd_stream::ZstdStreamEncoder) -> Option<u32> {
    let ptr = Box::into_raw(Box::new(encoder));
    match u32::try_from(ptr as usize) {
        Ok(ptr) => Some(ptr),
        Err(_) => {
            unsafe {
                drop(Box::from_raw(ptr));
            }
            None
        }
    }
}
