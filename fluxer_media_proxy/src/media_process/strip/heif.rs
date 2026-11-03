// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::MediaError;
use std::{collections::HashSet, ops::Range};

const FREE_BOX_HEADER: usize = 8;

struct BoxSpan {
    kind: [u8; 4],
    start: usize,
    body: usize,
    end: usize,
}

fn read_uint(input: &[u8], offset: usize, size: usize) -> Result<u64, MediaError> {
    let bytes = input
        .get(offset..offset + size)
        .ok_or(MediaError::MediaDecodeFailed)?;
    Ok(bytes
        .iter()
        .fold(0u64, |value, &byte| (value << 8) | u64::from(byte)))
}

fn read_box(input: &[u8], start: usize, limit: usize) -> Result<BoxSpan, MediaError> {
    let size = read_uint(input, start, 4)? as usize;
    let kind: [u8; 4] = input
        .get(start + 4..start + 8)
        .and_then(|kind| kind.try_into().ok())
        .ok_or(MediaError::MediaDecodeFailed)?;
    let (body, end) = match size {
        0 => (start + 8, limit),
        1 => {
            let large = usize::try_from(read_uint(input, start + 8, 8)?)
                .map_err(|_| MediaError::MediaDecodeFailed)?;
            (
                start + 16,
                start
                    .checked_add(large)
                    .ok_or(MediaError::MediaDecodeFailed)?,
            )
        }
        size => (
            start + 8,
            start
                .checked_add(size)
                .ok_or(MediaError::MediaDecodeFailed)?,
        ),
    };
    if body > end || end > limit {
        return Err(MediaError::MediaDecodeFailed);
    }
    Ok(BoxSpan {
        kind,
        start,
        body,
        end,
    })
}

fn children(input: &[u8], mut offset: usize, end: usize) -> Result<Vec<BoxSpan>, MediaError> {
    let mut boxes = Vec::new();
    while offset < end {
        let span = read_box(input, offset, end)?;
        offset = span.end;
        boxes.push(span);
    }
    Ok(boxes)
}

fn push_box(output: &mut Vec<u8>, kind: &[u8; 4], body: &[u8]) -> Result<(), MediaError> {
    let size = u32::try_from(body.len() + 8).map_err(|_| MediaError::StreamTooLong)?;
    output.extend_from_slice(&size.to_be_bytes());
    output.extend_from_slice(kind);
    output.extend_from_slice(body);
    Ok(())
}

fn metadata_item(input: &[u8], infe: &BoxSpan) -> Result<Option<u32>, MediaError> {
    let version = *input.get(infe.body).ok_or(MediaError::MediaDecodeFailed)?;
    if version < 2 {
        return Err(MediaError::UnsupportedMediaType);
    }
    let id_size = if version == 2 { 2 } else { 4 };
    let id = read_uint(input, infe.body + 4, id_size)? as u32;
    let type_offset = infe.body + 4 + id_size + 2;
    let item_type = input
        .get(type_offset..type_offset + 4)
        .ok_or(MediaError::MediaDecodeFailed)?;
    Ok(matches!(item_type, b"Exif" | b"mime").then_some(id))
}

struct Removal {
    items: HashSet<u32>,
    file_ranges: Vec<Range<usize>>,
    idat_ranges: Vec<Range<usize>>,
}

fn rewrite_iinf(input: &[u8], span: &BoxSpan, removal: &Removal) -> Result<Vec<u8>, MediaError> {
    let version = *input.get(span.body).ok_or(MediaError::MediaDecodeFailed)?;
    let count_size = if version == 0 { 2 } else { 4 };
    let entries_start = span.body + 4 + count_size;
    let mut kept = Vec::new();
    let mut count = 0u32;
    for entry in children(input, entries_start, span.end)? {
        let removed = &entry.kind == b"infe"
            && metadata_item(input, &entry)?.is_some_and(|id| removal.items.contains(&id));
        if !removed {
            kept.extend_from_slice(&input[entry.start..entry.end]);
            count += 1;
        }
    }
    let mut body = input[span.body..span.body + 4].to_vec();
    if version == 0 {
        body.extend_from_slice(
            &u16::try_from(count)
                .map_err(|_| MediaError::MediaDecodeFailed)?
                .to_be_bytes(),
        );
    } else {
        body.extend_from_slice(&count.to_be_bytes());
    }
    body.extend_from_slice(&kept);
    Ok(body)
}

fn rewrite_iloc(
    input: &[u8],
    span: &BoxSpan,
    removal: &mut Removal,
) -> Result<Vec<u8>, MediaError> {
    let version = *input.get(span.body).ok_or(MediaError::MediaDecodeFailed)?;
    let sizes = read_uint(input, span.body + 4, 2)? as usize;
    let offset_size = sizes >> 12;
    let length_size = (sizes >> 8) & 0x0F;
    let base_offset_size = (sizes >> 4) & 0x0F;
    let index_size = if version == 0 { 0 } else { sizes & 0x0F };
    let (count_size, id_size) = if version < 2 { (2, 2) } else { (4, 4) };
    let count = read_uint(input, span.body + 6, count_size)?;
    let mut offset = span.body + 6 + count_size;
    let mut kept = Vec::new();
    let mut kept_count = 0u64;
    for _ in 0..count {
        let entry_start = offset;
        let id = read_uint(input, offset, id_size)? as u32;
        offset += id_size;
        let method = if version == 0 {
            0
        } else {
            let value = read_uint(input, offset, 2)? & 0x0F;
            offset += 2;
            value
        };
        let reference = read_uint(input, offset, 2)?;
        let base = read_uint(input, offset + 2, base_offset_size)? as usize;
        let extents = read_uint(input, offset + 2 + base_offset_size, 2)? as usize;
        offset += 4 + base_offset_size;
        let extent_size = index_size + offset_size + length_size;
        let extents_end = offset + extents * extent_size;
        if extents_end > span.end {
            return Err(MediaError::MediaDecodeFailed);
        }
        if !removal.items.contains(&id) {
            kept.extend_from_slice(&input[entry_start..extents_end]);
            kept_count += 1;
            offset = extents_end;
            continue;
        }
        if reference != 0 || extents == 0 || length_size == 0 {
            return Err(MediaError::UnsupportedMediaType);
        }
        let mut ranges = Vec::with_capacity(extents);
        for _ in 0..extents {
            offset += index_size;
            let start = base.saturating_add(read_uint(input, offset, offset_size)? as usize);
            let length = read_uint(input, offset + offset_size, length_size)? as usize;
            offset += offset_size + length_size;
            ranges.push(start..start.saturating_add(length));
        }
        if ranges.iter().any(Range::is_empty) {
            return Err(MediaError::UnsupportedMediaType);
        }
        match method {
            0 => removal.file_ranges.extend(ranges),
            1 => removal.idat_ranges.extend(ranges),
            _ => return Err(MediaError::UnsupportedMediaType),
        }
    }
    let mut body = input[span.body..span.body + 6].to_vec();
    if count_size == 2 {
        body.extend_from_slice(&(kept_count as u16).to_be_bytes());
    } else {
        body.extend_from_slice(&(kept_count as u32).to_be_bytes());
    }
    body.extend_from_slice(&kept);
    Ok(body)
}

fn rewrite_iref(input: &[u8], span: &BoxSpan, removal: &Removal) -> Result<Vec<u8>, MediaError> {
    let version = *input.get(span.body).ok_or(MediaError::MediaDecodeFailed)?;
    let id_size = if version == 0 { 2 } else { 4 };
    let mut body = input[span.body..span.body + 4].to_vec();
    for reference in children(input, span.body + 4, span.end)? {
        let from = read_uint(input, reference.body, id_size)? as u32;
        if removal.items.contains(&from) {
            continue;
        }
        let count = read_uint(input, reference.body + id_size, 2)? as usize;
        let mut targets = Vec::new();
        for index in 0..count {
            let target_offset = reference.body + id_size + 2 + index * id_size;
            let target = read_uint(input, target_offset, id_size)? as u32;
            if !removal.items.contains(&target) {
                targets.extend_from_slice(&input[target_offset..target_offset + id_size]);
            }
        }
        if targets.is_empty() {
            continue;
        }
        let mut reference_body = input[reference.body..reference.body + id_size].to_vec();
        reference_body.extend_from_slice(&((targets.len() / id_size) as u16).to_be_bytes());
        reference_body.extend_from_slice(&targets);
        push_box(&mut body, &reference.kind, &reference_body)?;
    }
    Ok(body)
}

fn rewrite_ipma(input: &[u8], span: &BoxSpan, removal: &Removal) -> Result<Vec<u8>, MediaError> {
    let version = *input.get(span.body).ok_or(MediaError::MediaDecodeFailed)?;
    let flags = read_uint(input, span.body + 1, 3)?;
    let id_size = if version < 1 { 2 } else { 4 };
    let association_size = if flags & 1 == 1 { 2 } else { 1 };
    let count = read_uint(input, span.body + 4, 4)?;
    let mut offset = span.body + 8;
    let mut kept = Vec::new();
    let mut kept_count = 0u32;
    for _ in 0..count {
        let entry_start = offset;
        let id = read_uint(input, offset, id_size)? as u32;
        let associations = usize::from(
            *input
                .get(offset + id_size)
                .ok_or(MediaError::MediaDecodeFailed)?,
        );
        offset += id_size + 1 + associations * association_size;
        if offset > span.end {
            return Err(MediaError::MediaDecodeFailed);
        }
        if !removal.items.contains(&id) {
            kept.extend_from_slice(&input[entry_start..offset]);
            kept_count += 1;
        }
    }
    let mut body = input[span.body..span.body + 4].to_vec();
    body.extend_from_slice(&kept_count.to_be_bytes());
    body.extend_from_slice(&kept);
    Ok(body)
}

fn zero_ranges(target: &mut [u8], ranges: &[Range<usize>]) -> Result<(), MediaError> {
    for range in ranges {
        target
            .get_mut(range.clone())
            .ok_or(MediaError::MediaDecodeFailed)?
            .fill(0);
    }
    Ok(())
}

fn rewrite_meta(
    input: &[u8],
    meta: &BoxSpan,
    removal: &mut Removal,
) -> Result<Vec<u8>, MediaError> {
    let parts = children(input, meta.body + 4, meta.end)?;
    for infe in parts
        .iter()
        .filter(|part| &part.kind == b"iinf")
        .map(|iinf| {
            let version = input.get(iinf.body).copied().unwrap_or(0);
            let count_size = if version == 0 { 2 } else { 4 };
            children(input, iinf.body + 4 + count_size, iinf.end)
        })
        .collect::<Result<Vec<_>, _>>()?
        .into_iter()
        .flatten()
        .filter(|entry| &entry.kind == b"infe")
    {
        if let Some(id) = metadata_item(input, &infe)? {
            removal.items.insert(id);
        }
    }
    if removal.items.is_empty() {
        return Ok(input[meta.start..meta.end].to_vec());
    }
    let mut body = input[meta.body..meta.body + 4].to_vec();
    for part in &parts {
        let rewritten = match &part.kind {
            b"iinf" => rewrite_iinf(input, part, removal)?,
            b"iloc" => rewrite_iloc(input, part, removal)?,
            b"iref" => rewrite_iref(input, part, removal)?,
            b"iprp" => {
                let mut iprp = Vec::new();
                for child in children(input, part.body, part.end)? {
                    if &child.kind == b"ipma" {
                        let ipma = rewrite_ipma(input, &child, removal)?;
                        push_box(&mut iprp, &child.kind, &ipma)?;
                    } else {
                        iprp.extend_from_slice(&input[child.start..child.end]);
                    }
                }
                iprp
            }
            _ => {
                body.extend_from_slice(&input[part.start..part.end]);
                continue;
            }
        };
        push_box(&mut body, &part.kind, &rewritten)?;
    }
    let mut output = Vec::new();
    push_box(&mut output, b"meta", &body)?;
    Ok(output)
}

pub(super) fn strip_items(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    let top = children(input, 0, input.len())?;
    let meta = top
        .iter()
        .find(|span| &span.kind == b"meta")
        .ok_or(MediaError::MediaDecodeFailed)?;
    let mut removal = Removal {
        items: HashSet::new(),
        file_ranges: Vec::new(),
        idat_ranges: Vec::new(),
    };
    let mut rewritten = rewrite_meta(input, meta, &mut removal)?;
    if removal.items.is_empty() {
        return Ok(input.to_vec());
    }
    if !removal.idat_ranges.is_empty() {
        zero_idat(&mut rewritten, &removal.idat_ranges)?;
    }
    let original_len = meta.end - meta.start;
    let padding = original_len
        .checked_sub(rewritten.len())
        .filter(|padding| *padding >= FREE_BOX_HEADER)
        .ok_or(MediaError::UnsupportedMediaType)?;
    if removal
        .file_ranges
        .iter()
        .any(|range| range.start < meta.end && range.end > meta.start)
    {
        return Err(MediaError::UnsupportedMediaType);
    }
    let mut output = Vec::with_capacity(input.len());
    output.extend_from_slice(&input[..meta.start]);
    output.extend_from_slice(&rewritten);
    push_box(&mut output, b"free", &vec![0; padding - FREE_BOX_HEADER])?;
    output.extend_from_slice(&input[meta.end..]);
    zero_ranges(&mut output, &removal.file_ranges)?;
    Ok(output)
}

fn zero_idat(meta: &mut [u8], ranges: &[Range<usize>]) -> Result<(), MediaError> {
    let parts = children(meta, 12, meta.len())?;
    let idat = parts
        .iter()
        .find(|part| &part.kind == b"idat")
        .ok_or(MediaError::MediaDecodeFailed)?;
    let (body, end) = (idat.body, idat.end);
    zero_ranges(&mut meta[body..end], ranges)
}
