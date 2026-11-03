// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::{MediaError, PNG_SIGNATURE};

const KEPT_ANCILLARY_CHUNKS: [&[u8; 4]; 9] = [
    b"tRNS", b"gAMA", b"cHRM", b"sRGB", b"iCCP", b"cICP", b"acTL", b"fcTL", b"fdAT",
];

fn keeps_chunk(kind: &[u8]) -> bool {
    kind[0].is_ascii_uppercase() || KEPT_ANCILLARY_CHUNKS.iter().any(|kept| kept[..] == *kind)
}

pub(super) fn strip_chunks(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    if !input.starts_with(PNG_SIGNATURE) {
        return Err(MediaError::MediaDecodeFailed);
    }
    let mut output = Vec::with_capacity(input.len());
    output.extend_from_slice(PNG_SIGNATURE);
    let mut offset = PNG_SIGNATURE.len();
    while let Some(header) = input.get(offset..offset + 8) {
        let length = u32::from_be_bytes([header[0], header[1], header[2], header[3]]) as usize;
        let kind = &header[4..8];
        let end = offset.saturating_add(12).saturating_add(length);
        if keeps_chunk(kind) {
            output.extend_from_slice(
                input
                    .get(offset..end)
                    .ok_or(MediaError::MediaDecodeFailed)?,
            );
        }
        offset = end;
        if kind == b"IEND" {
            break;
        }
    }
    Ok(output)
}
