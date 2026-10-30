// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::MediaError;

const EXTENSION: u8 = 0x21;
const IMAGE: u8 = 0x2C;
const TRAILER: u8 = 0x3B;
const PLAIN_TEXT: u8 = 0x01;
const GRAPHIC_CONTROL: u8 = 0xF9;
const APPLICATION: u8 = 0xFF;
const KEPT_APPLICATIONS: [&[u8; 11]; 2] = [b"NETSCAPE2.0", b"ANIMEXTS1.0"];

fn colour_table_len(flags: u8) -> usize {
    if flags & 0x80 == 0 {
        0
    } else {
        3 << ((flags & 0x07) + 1)
    }
}

fn skip_sub_blocks(input: &[u8], mut offset: usize) -> Result<usize, MediaError> {
    loop {
        let size = usize::from(*input.get(offset).ok_or(MediaError::MediaDecodeFailed)?);
        offset += 1;
        if size == 0 {
            return Ok(offset);
        }
        offset += size;
        if offset > input.len() {
            return Err(MediaError::MediaDecodeFailed);
        }
    }
}

fn keeps_extension(input: &[u8], offset: usize) -> bool {
    match input.get(offset + 1) {
        Some(&GRAPHIC_CONTROL | &PLAIN_TEXT) => true,
        Some(&APPLICATION) => {
            input.get(offset + 2) == Some(&11)
                && input
                    .get(offset + 3..offset + 14)
                    .is_some_and(|id| KEPT_APPLICATIONS.iter().any(|kept| kept[..] == *id))
        }
        _ => false,
    }
}

pub(super) fn strip_blocks(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    if !(input.starts_with(b"GIF87a") || input.starts_with(b"GIF89a")) || input.len() < 13 {
        return Err(MediaError::MediaDecodeFailed);
    }
    let header_end = 13 + colour_table_len(input[10]);
    let mut output = Vec::with_capacity(input.len());
    output.extend_from_slice(
        input
            .get(..header_end)
            .ok_or(MediaError::MediaDecodeFailed)?,
    );
    let mut offset = header_end;
    loop {
        match input.get(offset).copied().unwrap_or(TRAILER) {
            TRAILER => {
                output.push(TRAILER);
                return Ok(output);
            }
            EXTENSION => {
                let end = skip_sub_blocks(input, offset + 2)?;
                if keeps_extension(input, offset) {
                    output.extend_from_slice(&input[offset..end]);
                }
                offset = end;
            }
            IMAGE => {
                let flags = *input.get(offset + 9).ok_or(MediaError::MediaDecodeFailed)?;
                let data_start = offset + 10 + colour_table_len(flags) + 1;
                let end = skip_sub_blocks(input, data_start)?;
                output.extend_from_slice(&input[offset..end]);
                offset = end;
            }
            _ => return Err(MediaError::MediaDecodeFailed),
        }
    }
}
