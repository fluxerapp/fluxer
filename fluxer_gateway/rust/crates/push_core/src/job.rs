// SPDX-License-Identifier: AGPL-3.0-or-later

use std::io::Write;

fn decimal_len(id: u64) -> usize {
    id.checked_ilog10().map_or(1, |log| log as usize + 1)
}

pub fn id_array_len(ids: &[u64]) -> usize {
    let digits: usize = ids.iter().map(|&id| decimal_len(id)).sum();
    let separators = ids.len().saturating_sub(1);
    digits + ids.len() * 2 + separators + 2
}

pub fn body_len(head: &[u8], ids: &[u64], tail: &[u8]) -> usize {
    head.len() + id_array_len(ids) + tail.len()
}

pub fn write_body(
    mut out: &mut [u8],
    head: &[u8],
    ids: &[u64],
    tail: &[u8],
) -> std::io::Result<()> {
    let mut digits = itoa::Buffer::new();
    out.write_all(head)?;
    out.write_all(b"[")?;
    for (index, &id) in ids.iter().enumerate() {
        let lead: &[u8] = if index == 0 { b"\"" } else { b",\"" };
        out.write_all(lead)?;
        out.write_all(digits.format(id).as_bytes())?;
        out.write_all(b"\"")?;
    }
    out.write_all(b"]")?;
    out.write_all(tail)?;
    if out.is_empty() {
        Ok(())
    } else {
        Err(std::io::ErrorKind::InvalidInput.into())
    }
}

pub fn body(head: &[u8], ids: &[u64], tail: &[u8]) -> Vec<u8> {
    let mut out = vec![0; body_len(head, ids, tail)];
    match write_body(&mut out, head, ids, tail) {
        Ok(()) => out,
        Err(_) => Vec::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::{body, body_len, id_array_len};
    use proptest::prelude::*;

    fn reference(head: &[u8], ids: &[u64], tail: &[u8]) -> Vec<u8> {
        let items: Vec<String> = ids.iter().map(|id| format!("\"{id}\"")).collect();
        let mut out = head.to_vec();
        out.extend_from_slice(format!("[{}]", items.join(",")).as_bytes());
        out.extend_from_slice(tail);
        out
    }

    #[test]
    fn writes_quoted_decimal_ids_between_head_and_tail() {
        let written = body(
            b"{\"a\":1,\"user_ids\":",
            &[1_400_000_000_000_000_001, 7],
            b",\"v\":1}",
        );
        assert_eq!(
            written,
            b"{\"a\":1,\"user_ids\":[\"1400000000000000001\",\"7\"],\"v\":1}".to_vec()
        );
    }

    #[test]
    fn writes_an_empty_array() {
        assert_eq!(
            body(b"{\"user_ids\":", &[], b"}"),
            b"{\"user_ids\":[]}".to_vec()
        );
        assert_eq!(id_array_len(&[]), 2);
    }

    #[test]
    fn counts_every_width() {
        let ids = [0, 9, 10, 99, 100, u64::MAX, 1_000_000_000_000_000_000];
        assert_eq!(body_len(b"", &ids, b""), reference(b"", &ids, b"").len());
    }

    proptest! {
        #[test]
        fn matches_the_reference_writer(
            head in proptest::collection::vec(any::<u8>(), 0..32),
            ids in proptest::collection::vec(any::<u64>(), 0..64),
            tail in proptest::collection::vec(any::<u8>(), 0..32),
        ) {
            prop_assert_eq!(body(&head, &ids, &tail), reference(&head, &ids, &tail));
            prop_assert_eq!(body_len(&head, &ids, &tail), reference(&head, &ids, &tail).len());
        }
    }
}
