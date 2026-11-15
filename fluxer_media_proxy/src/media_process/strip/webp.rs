// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::MediaError;

const KEPT_CHUNKS: [&[u8; 4]; 7] = [
    b"VP8 ", b"VP8L", b"VP8X", b"ALPH", b"ANIM", b"ANMF", b"ICCP",
];
const VP8X_EXIF_FLAG: u8 = 0x08;
const VP8X_XMP_FLAG: u8 = 0x04;

fn read_u32_le(input: &[u8], offset: usize) -> Option<usize> {
    let bytes = input.get(offset..offset + 4)?;
    Some(u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize)
}

pub(super) fn strip_chunks(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    if input.len() < 12 || &input[..4] != b"RIFF" || &input[8..12] != b"WEBP" {
        return Err(MediaError::MediaDecodeFailed);
    }
    let riff_end = input.len().min(
        read_u32_le(input, 4)
            .ok_or(MediaError::MediaDecodeFailed)?
            .saturating_add(8),
    );
    let mut output = Vec::with_capacity(input.len());
    output.extend_from_slice(&input[..12]);
    let mut offset = 12;
    while offset + 8 <= riff_end {
        let length = read_u32_le(input, offset + 4).ok_or(MediaError::MediaDecodeFailed)?;
        let payload_end = offset.saturating_add(8).saturating_add(length);
        if payload_end > riff_end {
            return Err(MediaError::MediaDecodeFailed);
        }
        let chunk_end = payload_end.saturating_add(length & 1);
        let kind = &input[offset..offset + 4];
        if KEPT_CHUNKS.iter().any(|kept| kept[..] == *kind) {
            let start = output.len();
            output.extend_from_slice(&input[offset..chunk_end.min(riff_end)]);
            if kind == b"VP8X" && length > 0 {
                output[start + 8] &= !(VP8X_EXIF_FLAG | VP8X_XMP_FLAG);
            }
        }
        offset = chunk_end;
    }
    let riff_size = u32::try_from(output.len() - 8).map_err(|_| MediaError::StreamTooLong)?;
    output[4..8].copy_from_slice(&riff_size.to_le_bytes());
    Ok(output)
}
