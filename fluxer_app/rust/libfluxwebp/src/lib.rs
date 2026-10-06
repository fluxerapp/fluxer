// SPDX-License-Identifier: AGPL-3.0-or-later

#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod shim;

use libwebp_sys as ffi;
use std::ffi::CStr;
use std::ptr;
use wasm_bindgen::prelude::*;

const DEMUX_ABI: i32 = ffi::WEBP_DEMUX_ABI_VERSION as i32;
const MUX_ABI: i32 = ffi::WEBP_MUX_ABI_VERSION as i32;
const ENCODER_ABI: i32 = ffi::WEBP_ENCODER_ABI_VERSION as i32;
const DECODER_ABI: i32 = ffi::WEBP_DECODER_ABI_VERSION as i32;
const LOSSLESS_FORMAT: i32 = 2;

fn webp_data(bytes: &[u8]) -> ffi::WebPData {
    ffi::WebPData {
        bytes: bytes.as_ptr(),
        size: bytes.len(),
    }
}

#[wasm_bindgen]
pub fn webp_probe(bytes: &[u8]) -> Result<Box<[u32]>, JsError> {
    let data = webp_data(bytes);
    let dmux = unsafe { ffi::WebPDemuxInternal(&data, 0, ptr::null_mut(), DEMUX_ABI) };
    if dmux.is_null() {
        return Err(JsError::new("webp demux failed"));
    }
    let get = |feature| unsafe { ffi::WebPDemuxGetI(dmux, feature) };
    let width = get(ffi::WebPFormatFeature::WEBP_FF_CANVAS_WIDTH);
    let height = get(ffi::WebPFormatFeature::WEBP_FF_CANVAS_HEIGHT);
    let frame_count = get(ffi::WebPFormatFeature::WEBP_FF_FRAME_COUNT);
    let loop_count = get(ffi::WebPFormatFeature::WEBP_FF_LOOP_COUNT);
    let flags = get(ffi::WebPFormatFeature::WEBP_FF_FORMAT_FLAGS);
    let has_alpha = flags & ffi::WebPFeatureFlags::ALPHA_FLAG as u32 != 0;
    let all_lossless = unsafe { all_frames_lossless(dmux) };
    unsafe { ffi::WebPDemuxDelete(dmux) };
    Ok(Box::new([
        width,
        height,
        frame_count,
        loop_count,
        has_alpha as u32,
        all_lossless as u32,
    ]))
}

unsafe fn all_frames_lossless(dmux: *const ffi::WebPDemuxer) -> bool {
    unsafe {
        let mut iter: ffi::WebPIterator = std::mem::zeroed();
        if ffi::WebPDemuxGetFrame(dmux, 1, &mut iter) == 0 {
            return false;
        }
        let mut lossless = true;
        loop {
            let mut features: ffi::WebPBitstreamFeatures = std::mem::zeroed();
            let status = ffi::WebPGetFeaturesInternal(
                iter.fragment.bytes,
                iter.fragment.size,
                &mut features,
                DECODER_ABI,
            );
            if status != ffi::VP8StatusCode::VP8_STATUS_OK || features.format != LOSSLESS_FORMAT {
                lossless = false;
                break;
            }
            if ffi::WebPDemuxNextFrame(&mut iter) == 0 {
                break;
            }
        }
        ffi::WebPDemuxReleaseIterator(&mut iter);
        lossless
    }
}

#[wasm_bindgen]
pub struct AnimDecoder {
    dec: *mut ffi::WebPAnimDecoder,
    _bytes: Box<[u8]>,
    frame: *mut u8,
    timestamp: i32,
}

#[wasm_bindgen]
impl AnimDecoder {
    #[wasm_bindgen(constructor)]
    pub fn new(bytes: Box<[u8]>) -> Result<AnimDecoder, JsError> {
        let dec = unsafe {
            let mut options: ffi::WebPAnimDecoderOptions = std::mem::zeroed();
            if ffi::WebPAnimDecoderOptionsInitInternal(&mut options, DEMUX_ABI) == 0 {
                return Err(JsError::new("webp decoder options init failed"));
            }
            options.color_mode = ffi::WEBP_CSP_MODE::MODE_RGBA;
            options.use_threads = 0;
            let data = webp_data(&bytes);
            ffi::WebPAnimDecoderNewInternal(&data, &options, DEMUX_ABI)
        };
        if dec.is_null() {
            return Err(JsError::new("webp decoder init failed"));
        }
        Ok(AnimDecoder {
            dec,
            _bytes: bytes,
            frame: ptr::null_mut(),
            timestamp: 0,
        })
    }

    #[wasm_bindgen(js_name = next)]
    pub fn next_frame(&mut self) -> Result<bool, JsError> {
        if unsafe { ffi::WebPAnimDecoderHasMoreFrames(self.dec) } == 0 {
            return Ok(false);
        }
        let mut frame = ptr::null_mut();
        let mut timestamp = 0;
        if unsafe { ffi::WebPAnimDecoderGetNext(self.dec, &mut frame, &mut timestamp) } == 0 {
            return Err(JsError::new("webp frame decode failed"));
        }
        self.frame = frame;
        self.timestamp = timestamp;
        Ok(true)
    }

    #[wasm_bindgen(getter)]
    pub fn frame_ptr(&self) -> u32 {
        self.frame as usize as u32
    }

    #[wasm_bindgen(getter)]
    pub fn end_timestamp(&self) -> i32 {
        self.timestamp
    }
}

impl Drop for AnimDecoder {
    fn drop(&mut self) {
        unsafe { ffi::WebPAnimDecoderDelete(self.dec) };
    }
}

#[wasm_bindgen]
pub struct AnimEncoder {
    enc: *mut ffi::WebPAnimEncoder,
    width: u32,
    height: u32,
    lossless: bool,
}

#[wasm_bindgen]
impl AnimEncoder {
    #[wasm_bindgen(constructor)]
    pub fn new(
        width: u32,
        height: u32,
        loop_count: u32,
        lossless: bool,
    ) -> Result<AnimEncoder, JsError> {
        let (kmin, kmax) = if lossless { (9, 17) } else { (4, 5) };
        let enc = unsafe {
            let mut options: ffi::WebPAnimEncoderOptions = std::mem::zeroed();
            if ffi::WebPAnimEncoderOptionsInitInternal(&mut options, MUX_ABI) == 0 {
                return Err(JsError::new("webp encoder options init failed"));
            }
            options.anim_params.loop_count = loop_count.min(i32::MAX as u32) as i32;
            options.anim_params.bgcolor = 0;
            options.minimize_size = 0;
            options.allow_mixed = 0;
            options.kmin = kmin;
            options.kmax = kmax;
            ffi::WebPAnimEncoderNewInternal(width as i32, height as i32, &options, MUX_ABI)
        };
        if enc.is_null() {
            return Err(JsError::new("webp encoder init failed"));
        }
        Ok(AnimEncoder {
            enc,
            width,
            height,
            lossless,
        })
    }

    pub fn add(
        &mut self,
        rgba: &[u8],
        start_ms: i32,
        quality: f32,
        method: i32,
    ) -> Result<(), JsError> {
        if rgba.len() as u64 != u64::from(self.width) * u64::from(self.height) * 4 {
            return Err(JsError::new("webp frame size mismatch"));
        }
        let added = unsafe {
            let mut config: ffi::WebPConfig = std::mem::zeroed();
            if ffi::WebPConfigInitInternal(
                &mut config,
                ffi::WebPPreset::WEBP_PRESET_DEFAULT,
                quality,
                ENCODER_ABI,
            ) == 0
            {
                return Err(JsError::new("webp config init failed"));
            }
            config.quality = quality;
            config.method = method;
            config.lossless = self.lossless as i32;
            config.thread_level = 0;
            config.alpha_quality = 100;
            config.exact = 0;
            config.use_sharp_yuv = 0;
            let mut picture: ffi::WebPPicture = std::mem::zeroed();
            if ffi::WebPPictureInitInternal(&mut picture, ENCODER_ABI) == 0 {
                return Err(JsError::new("webp picture init failed"));
            }
            picture.width = self.width as i32;
            picture.height = self.height as i32;
            picture.use_argb = 1;
            if ffi::WebPPictureImportRGBA(&mut picture, rgba.as_ptr(), self.width as i32 * 4) == 0 {
                ffi::WebPPictureFree(&mut picture);
                return Err(JsError::new("webp picture import failed"));
            }
            let added = ffi::WebPAnimEncoderAdd(self.enc, &mut picture, start_ms, &config);
            ffi::WebPPictureFree(&mut picture);
            added
        };
        if added == 0 {
            return Err(self.error("webp frame encode failed"));
        }
        Ok(())
    }

    pub fn finish(&mut self, end_ms: i32) -> Result<Vec<u8>, JsError> {
        if unsafe { ffi::WebPAnimEncoderAdd(self.enc, ptr::null_mut(), end_ms, ptr::null()) } == 0 {
            return Err(self.error("webp encoder flush failed"));
        }
        let mut data = ffi::WebPData::default();
        if unsafe { ffi::WebPAnimEncoderAssemble(self.enc, &mut data) } == 0 {
            return Err(self.error("webp assemble failed"));
        }
        let out = unsafe { std::slice::from_raw_parts(data.bytes, data.size) }.to_vec();
        unsafe { ffi::WebPFree(data.bytes as *mut _) };
        Ok(out)
    }

    fn error(&self, what: &str) -> JsError {
        let message = unsafe { ffi::WebPAnimEncoderGetError(self.enc) };
        if message.is_null() {
            return JsError::new(what);
        }
        let detail = unsafe { CStr::from_ptr(message) }.to_string_lossy();
        JsError::new(&format!("{what}: {detail}"))
    }
}

impl Drop for AnimEncoder {
    fn drop(&mut self) {
        unsafe { ffi::WebPAnimEncoderDelete(self.enc) };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn frame(width: u32, height: u32, seed: u8) -> Vec<u8> {
        (0..width * height)
            .flat_map(|index| {
                let value = (index as u8).wrapping_mul(seed);
                [
                    value,
                    seed,
                    value ^ seed,
                    if index % 3 == 0 { 0 } else { 255 },
                ]
            })
            .collect()
    }

    fn encode(lossless: bool) -> Vec<u8> {
        let mut encoder = AnimEncoder::new(16, 8, 3, lossless).expect("encoder");
        let delays = [40, 70, 10];
        let mut start = 0;
        for (index, delay) in delays.iter().enumerate() {
            encoder
                .add(&frame(16, 8, index as u8 + 1), start, 75.0, 4)
                .expect("add");
            start += delay;
        }
        encoder.finish(start).expect("finish")
    }

    #[test]
    fn probe_reports_canvas_frames_loop_alpha_and_lossless() {
        let lossless = encode(true);
        assert_eq!(
            &*webp_probe(&lossless).expect("probe"),
            &[16, 8, 3, 3, 1, 1]
        );
        let lossy = encode(false);
        let probe = webp_probe(&lossy).expect("probe");
        assert_eq!(&probe[..5], &[16, 8, 3, 3, 1]);
        assert_eq!(probe[5], 0);
    }

    #[test]
    fn lossless_round_trip_keeps_pixels_and_end_timestamps() {
        let bytes = encode(true);
        let mut decoder = AnimDecoder::new(bytes.into_boxed_slice()).expect("decoder");
        let mut ends = Vec::new();
        let mut index = 0u8;
        while decoder.next_frame().expect("next") {
            let pixels = unsafe { std::slice::from_raw_parts(decoder.frame, 16 * 8 * 4) };
            let expected = frame(16, 8, index + 1);
            for (got, want) in pixels.chunks(4).zip(expected.chunks(4)) {
                assert_eq!(got[3], want[3]);
                if want[3] != 0 {
                    assert_eq!(got, want);
                }
            }
            ends.push(decoder.end_timestamp());
            index += 1;
        }
        assert_eq!(ends, [40, 110, 120]);
    }
}
