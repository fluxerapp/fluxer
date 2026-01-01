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

use ruzstd::StreamingDecoder;
use std::io::{Cursor, Read};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn decompress_zstd_frame(input: &[u8]) -> Result<Box<[u8]>, JsValue> {
    let mut decoder = StreamingDecoder::new(Cursor::new(input))
        .map_err(|e| JsValue::from_str(&format!("zstd init error: {e}")))?;

    let mut output = Vec::new();
    decoder
        .read_to_end(&mut output)
        .map_err(|e| JsValue::from_str(&format!("zstd read error: {e}")))?;

    Ok(output.into_boxed_slice())
}
