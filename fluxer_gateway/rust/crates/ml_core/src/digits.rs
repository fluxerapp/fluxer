// SPDX-License-Identifier: AGPL-3.0-or-later

pub fn int(out: &mut Vec<u8>, value: u64) {
    let mut buf = [0u8; 20];
    let mut at = buf.len();
    let mut rest = value;
    while let Some(next) = at.checked_sub(1) {
        at = next;
        if let Some(cell) = buf.get_mut(at) {
            *cell = b'0'.wrapping_add((rest % 10) as u8);
        }
        rest /= 10;
        if rest == 0 {
            break;
        }
    }
    out.extend_from_slice(buf.get(at..).unwrap_or_default());
}

pub fn quoted_int(out: &mut Vec<u8>, value: u64) {
    out.push(b'"');
    int(out, value);
    out.push(b'"');
}

#[cfg(test)]
mod tests {
    use super::*;

    fn text(value: u64) -> Vec<u8> {
        let mut out = Vec::new();
        int(&mut out, value);
        out
    }

    #[test]
    fn digits_cover_the_extremes() {
        assert_eq!(text(0), b"0");
        assert_eq!(text(7), b"7");
        assert_eq!(text(1_420_000_000_000_000_001), b"1420000000000000001");
        assert_eq!(text(u64::MAX), b"18446744073709551615");
        let mut quoted = Vec::new();
        quoted_int(&mut quoted, 42);
        assert_eq!(quoted, b"\"42\"");
    }
}
