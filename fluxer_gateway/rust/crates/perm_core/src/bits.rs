// SPDX-License-Identifier: AGPL-3.0-or-later

pub(crate) fn words_for(len: usize) -> usize {
    len.div_ceil(64)
}

pub(crate) fn locate(index: usize) -> (usize, u64) {
    let bit = u32::try_from(index.rem_euclid(64)).unwrap_or(0);
    (index.div_euclid(64), 1u64.checked_shl(bit).unwrap_or(0))
}

pub(crate) fn test(words: &[u64], index: usize) -> bool {
    let (word, mask) = locate(index);
    words.get(word).is_some_and(|w| w & mask != 0)
}

pub(crate) fn set(words: &mut [u64], index: usize) {
    let (word, mask) = locate(index);
    if let Some(w) = words.get_mut(word) {
        *w |= mask;
    }
}

pub(crate) fn assign(words: &mut [u64], index: usize, on: bool) {
    let (word, mask) = locate(index);
    if let Some(w) = words.get_mut(word) {
        if on {
            *w |= mask;
        } else {
            *w &= !mask;
        }
    }
}

pub(crate) fn tail_mask(len: usize, word: usize) -> u64 {
    let start = word.saturating_mul(64);
    if len >= start.saturating_add(64) {
        u64::MAX
    } else if len <= start {
        0
    } else {
        let bits = u32::try_from(len.saturating_sub(start)).unwrap_or(0);
        1u64.checked_shl(bits)
            .map_or(u64::MAX, |v| v.wrapping_sub(1))
    }
}

pub(crate) fn ones(words: &[u64]) -> impl Iterator<Item = usize> + '_ {
    words.iter().enumerate().flat_map(|(w, word)| {
        let base = w.saturating_mul(64);
        BitIter(*word).map(move |bit| base.saturating_add(bit))
    })
}

pub(crate) struct BitIter(pub(crate) u64);

impl Iterator for BitIter {
    type Item = usize;

    fn next(&mut self) -> Option<usize> {
        if self.0 == 0 {
            return None;
        }
        let bit = self.0.trailing_zeros();
        self.0 &= self.0.wrapping_sub(1);
        usize::try_from(bit).ok()
    }
}
