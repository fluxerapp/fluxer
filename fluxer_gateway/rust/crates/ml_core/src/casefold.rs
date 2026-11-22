// SPDX-License-Identifier: AGPL-3.0-or-later

use crate::casefold_table::{FOLD_BYTES, FOLD_KEYS, FOLD_SPANS};

const HIGH: u64 = 0x8080_8080_8080_8080;
const ADD_GE_A: u64 = 0x3f3f_3f3f_3f3f_3f3f;
const ADD_GT_Z: u64 = 0x2525_2525_2525_2525;

fn lower_word(word: u64) -> u64 {
    let ge_a = word.wrapping_add(ADD_GE_A) & HIGH;
    let gt_z = word.wrapping_add(ADD_GT_Z) & HIGH;
    word | ((ge_a & !gt_z) >> 2)
}

fn fold_ascii(name: &[u8], out: &mut Vec<u8>) {
    let (chunks, rest) = name.as_chunks::<8>();
    for chunk in chunks {
        out.extend_from_slice(&lower_word(u64::from_le_bytes(*chunk)).to_le_bytes());
    }
    out.extend(rest.iter().map(u8::to_ascii_lowercase));
}

pub fn is_ascii(name: &[u8]) -> bool {
    let (chunks, rest) = name.as_chunks::<8>();
    chunks.iter().all(|c| u64::from_le_bytes(*c) & HIGH == 0) && rest.iter().all(|b| *b < 0x80)
}

fn fold_scalar(c: char, out: &mut Vec<u8>) {
    let code = u32::from(c);
    let found = FOLD_KEYS
        .binary_search(&code)
        .ok()
        .and_then(|i| FOLD_SPANS.get(i))
        .and_then(|(off, len)| {
            let start = usize::from(*off);
            FOLD_BYTES.get(start..start.checked_add(usize::from(*len))?)
        });
    match found {
        Some(bytes) => out.extend_from_slice(bytes),
        None => {
            let mut buf = [0u8; 4];
            out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
        }
    }
}

pub fn fold_into(name: &[u8], out: &mut Vec<u8>) {
    out.clear();
    if is_ascii(name) {
        fold_ascii(name, out);
        return;
    }
    match core::str::from_utf8(name) {
        Ok(text) => text.chars().for_each(|c| fold_scalar(c, out)),
        Err(_) => out.extend_from_slice(name),
    }
}

pub fn fold(name: &[u8]) -> Vec<u8> {
    let mut out = Vec::with_capacity(name.len());
    fold_into(name, &mut out);
    out
}

pub fn prefix(key: &[u8]) -> u64 {
    let mut word = [0u8; 8];
    for (cell, byte) in word.iter_mut().zip(key) {
        *cell = *byte;
    }
    u64::from_be_bytes(word)
}
