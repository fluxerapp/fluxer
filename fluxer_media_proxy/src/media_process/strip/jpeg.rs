// SPDX-License-Identifier: AGPL-3.0-or-later

use super::super::image_probe::load_image;
use super::super::loaded_image::validate_vips_image;
use super::super::native_runtime::clear_vips_error;
use super::super::streaming_encoder::write_vips_image_to_vec;
use super::super::{MediaError, native_status_error};
use crate::{
    constants,
    media_limits::MediaLimits,
    native::{self, NativeStatus, VipsImageHandle},
};
use std::{ffi::CString, ptr};

const SOI: u8 = 0xD8;
const EOI: u8 = 0xD9;
const SOS: u8 = 0xDA;
const DQT: u8 = 0xDB;
const APP0: u8 = 0xE0;
const APP2: u8 = 0xE2;
const APP14: u8 = 0xEE;
const APP15: u8 = 0xEF;
const COM: u8 = 0xFE;
const DEFAULT_REENCODE_QUALITY: u8 = 95;

struct Segment<'a> {
    marker: u8,
    bytes: &'a [u8],
    payload: &'a [u8],
}

struct JpegLayout<'a> {
    header: Vec<Segment<'a>>,
    scan_start: usize,
    scan_end: usize,
}

fn is_standalone(marker: u8) -> bool {
    matches!(marker, 0x01 | 0xD0..=0xD7)
}

fn read_segment(input: &[u8], offset: usize) -> Result<Segment<'_>, MediaError> {
    let header = input
        .get(offset..offset + 4)
        .ok_or(MediaError::MediaDecodeFailed)?;
    if header[0] != 0xFF {
        return Err(MediaError::MediaDecodeFailed);
    }
    let length = usize::from(u16::from_be_bytes([header[2], header[3]]));
    if length < 2 {
        return Err(MediaError::MediaDecodeFailed);
    }
    let bytes = input
        .get(offset..offset + 2 + length)
        .ok_or(MediaError::MediaDecodeFailed)?;
    Ok(Segment {
        marker: header[1],
        bytes,
        payload: &bytes[4..],
    })
}

fn skip_fill_bytes(input: &[u8], mut offset: usize) -> usize {
    while input.get(offset) == Some(&0xFF) && input.get(offset + 1) == Some(&0xFF) {
        offset += 1;
    }
    offset
}

fn find_end_of_image(input: &[u8], mut offset: usize) -> Result<usize, MediaError> {
    loop {
        let Some(found) = input[offset..].iter().position(|&byte| byte == 0xFF) else {
            return Ok(input.len());
        };
        offset = skip_fill_bytes(input, offset + found);
        let Some(&marker) = input.get(offset + 1) else {
            return Ok(input.len());
        };
        if marker == 0x00 || is_standalone(marker) {
            offset += 2;
            continue;
        }
        if marker == EOI {
            return Ok(offset + 2);
        }
        offset += read_segment(input, offset)?.bytes.len();
    }
}

fn parse_layout(input: &[u8]) -> Result<JpegLayout<'_>, MediaError> {
    if input.get(..2) != Some(&[0xFF, SOI]) {
        return Err(MediaError::MediaDecodeFailed);
    }
    let mut header = Vec::new();
    let mut offset = 2;
    loop {
        offset = skip_fill_bytes(input, offset);
        let marker = *input.get(offset + 1).ok_or(MediaError::MediaDecodeFailed)?;
        if is_standalone(marker) {
            offset += 2;
            continue;
        }
        if marker == EOI {
            return Err(MediaError::MediaDecodeFailed);
        }
        if marker == SOS {
            let scan_end = find_end_of_image(input, offset)?;
            return Ok(JpegLayout {
                header,
                scan_start: offset,
                scan_end,
            });
        }
        let segment = read_segment(input, offset)?;
        offset += segment.bytes.len();
        header.push(segment);
    }
}

fn is_icc(segment: &Segment<'_>) -> bool {
    segment.marker == APP2 && segment.payload.starts_with(b"ICC_PROFILE\0")
}

fn keeps_segment(segment: &Segment<'_>) -> bool {
    match segment.marker {
        APP0 => segment.payload.starts_with(b"JFIF\0") || segment.payload.starts_with(b"JFXX\0"),
        APP2 => is_icc(segment),
        APP14 => segment.payload.starts_with(b"Adobe"),
        APP0..=APP15 | COM => false,
        _ => true,
    }
}

pub(super) fn strip_segments(input: &[u8]) -> Result<Vec<u8>, MediaError> {
    let layout = parse_layout(input)?;
    let mut output = Vec::with_capacity(input.len());
    output.extend_from_slice(&[0xFF, SOI]);
    for segment in layout
        .header
        .iter()
        .filter(|segment| keeps_segment(segment))
    {
        output.extend_from_slice(segment.bytes);
    }
    output.extend_from_slice(&input[layout.scan_start..layout.scan_end]);
    Ok(output)
}

const COLOR_HASH: [u16; 100] = [
    1020, 1015, 932, 848, 780, 735, 702, 679, 660, 645, 632, 623, 613, 607, 600, 594, 589, 585,
    581, 571, 555, 542, 529, 514, 494, 474, 457, 439, 424, 410, 397, 386, 373, 364, 351, 341, 334,
    324, 317, 309, 299, 294, 287, 279, 274, 267, 262, 257, 251, 247, 243, 237, 232, 227, 222, 217,
    213, 207, 202, 198, 192, 188, 183, 177, 173, 168, 163, 157, 153, 148, 143, 139, 132, 128, 125,
    119, 115, 108, 104, 99, 94, 90, 84, 79, 74, 70, 64, 59, 55, 49, 45, 40, 34, 30, 25, 20, 15, 11,
    6, 4,
];
const COLOR_SUMS: [u32; 100] = [
    32640, 32635, 32266, 31495, 30665, 29804, 29146, 28599, 28104, 27670, 27225, 26725, 26210,
    25716, 25240, 24789, 24373, 23946, 23572, 22846, 21801, 20842, 19949, 19121, 18386, 17651,
    16998, 16349, 15800, 15247, 14783, 14321, 13859, 13535, 13081, 12702, 12423, 12056, 11779,
    11513, 11135, 10955, 10676, 10392, 10208, 9928, 9747, 9564, 9369, 9193, 9017, 8822, 8639, 8458,
    8270, 8084, 7896, 7710, 7527, 7347, 7156, 6977, 6788, 6607, 6422, 6236, 6054, 5867, 5684, 5495,
    5305, 5128, 4945, 4751, 4638, 4442, 4248, 4065, 3888, 3698, 3509, 3326, 3139, 2957, 2775, 2586,
    2405, 2216, 2037, 1846, 1666, 1483, 1297, 1109, 927, 735, 554, 375, 201, 128,
];
const GREY_HASH: [u16; 100] = [
    510, 505, 422, 380, 355, 338, 326, 318, 311, 305, 300, 297, 293, 291, 288, 286, 284, 283, 281,
    280, 279, 278, 277, 273, 262, 251, 243, 233, 225, 218, 211, 205, 198, 193, 186, 181, 177, 172,
    168, 164, 158, 156, 152, 148, 145, 142, 139, 136, 133, 131, 129, 126, 123, 120, 118, 115, 113,
    110, 107, 105, 102, 100, 97, 94, 92, 89, 87, 83, 81, 79, 76, 74, 70, 68, 66, 63, 61, 57, 55,
    52, 50, 48, 44, 42, 39, 37, 34, 31, 29, 26, 24, 21, 18, 16, 13, 11, 8, 6, 3, 2,
];
const GREY_SUMS: [u32; 100] = [
    16320, 16315, 15946, 15277, 14655, 14073, 13623, 13230, 12859, 12560, 12240, 11861, 11456,
    11081, 10714, 10360, 10027, 9679, 9368, 9056, 8680, 8331, 7995, 7668, 7376, 7084, 6823, 6562,
    6345, 6125, 5939, 5756, 5571, 5421, 5240, 5086, 4976, 4829, 4719, 4616, 4463, 4393, 4280, 4166,
    4092, 3980, 3909, 3835, 3755, 3688, 3621, 3541, 3467, 3396, 3323, 3247, 3170, 3096, 3021, 2952,
    2874, 2804, 2727, 2657, 2583, 2509, 2437, 2362, 2290, 2211, 2136, 2068, 1996, 1915, 1858, 1773,
    1692, 1620, 1552, 1477, 1398, 1326, 1251, 1179, 1109, 1031, 961, 884, 814, 736, 667, 592, 518,
    441, 369, 292, 221, 151, 86, 64,
];

fn estimate_quality(header: &[Segment<'_>]) -> Option<u8> {
    let mut by_index: [Option<&[u8]>; 4] = [None; 4];
    for segment in header.iter().filter(|segment| segment.marker == DQT) {
        let Some(&first) = segment.payload.first() else {
            continue;
        };
        if let Some(slot) = by_index.get_mut(usize::from(first & 0x0F)) {
            *slot = Some(segment.payload);
        }
    }
    let mut tables: Vec<&[u8]> = Vec::with_capacity(4);
    let mut sum: u32 = 0;
    'segments: for payload in by_index.into_iter().flatten() {
        let mut offset = 1;
        while offset + 64 <= payload.len() {
            let table = &payload[offset..offset + 64];
            sum += table.iter().map(|&value| u32::from(value)).sum::<u32>();
            tables.push(table);
            if tables.len() >= 4 {
                break 'segments;
            }
            offset += 65;
        }
    }
    let first = tables.first()?;
    let mut qval = u32::from(first[2]) + u32::from(first[53]);
    let (hash, sums): (&[u16; 100], &[u32; 100]) = match tables.get(1) {
        Some(second) => {
            qval += u32::from(second[0]) + u32::from(second[63]);
            (&COLOR_HASH, &COLOR_SUMS)
        }
        None => (&GREY_HASH, &GREY_SUMS),
    };
    for i in 0..100 {
        let hash = u32::from(hash[i]);
        if qval < hash && sum < sums[i] {
            continue;
        }
        return ((qval <= hash && sum <= sums[i]) || i >= 50).then(|| (i + 1) as u8);
    }
    None
}

struct FrameFacts {
    components: usize,
    full_chroma: bool,
}

fn frame_facts(header: &[Segment<'_>]) -> Result<FrameFacts, MediaError> {
    let frame = header
        .iter()
        .find(|segment| {
            matches!(segment.marker, 0xC0..=0xCF) && !matches!(segment.marker, 0xC4 | 0xC8 | 0xCC)
        })
        .ok_or(MediaError::MediaDecodeFailed)?;
    let components = usize::from(*frame.payload.get(5).ok_or(MediaError::MediaDecodeFailed)?);
    let sampling = |index: usize| frame.payload.get(6 + index * 3 + 1).copied();
    let full_chroma = components == 3 && sampling(0).is_some() && sampling(0) == sampling(1);
    Ok(FrameFacts {
        components,
        full_chroma,
    })
}

fn vips_status(rc: libc::c_int) -> Result<(), MediaError> {
    match native_status_error(
        NativeStatus::from_code(rc),
        MediaError::MediaTransformFailed,
    ) {
        Some(error) => {
            clear_vips_error();
            Err(error)
        }
        None => Ok(()),
    }
}

fn derive<'source>(
    image: &VipsImageHandle<'source>,
    raw: *mut native::VipsImage,
    rc: libc::c_int,
) -> Result<VipsImageHandle<'source>, MediaError> {
    let derived = unsafe { image.adopt_derived_raw(raw) };
    vips_status(rc)?;
    derived.ok_or(MediaError::MediaTransformFailed)
}

fn insert_after_jfif(encoded: Vec<u8>, segments: &[&Segment<'_>]) -> Result<Vec<u8>, MediaError> {
    let layout = parse_layout(&encoded)?;
    let mut output =
        Vec::with_capacity(encoded.len() + segments.iter().map(|s| s.bytes.len()).sum::<usize>());
    output.extend_from_slice(&[0xFF, SOI]);
    let jfif = layout
        .header
        .iter()
        .take_while(|segment| segment.marker == APP0)
        .count();
    for segment in &layout.header[..jfif] {
        output.extend_from_slice(segment.bytes);
    }
    for segment in segments {
        output.extend_from_slice(segment.bytes);
    }
    for segment in &layout.header[jfif..] {
        output.extend_from_slice(segment.bytes);
    }
    output.extend_from_slice(&encoded[layout.scan_start..layout.scan_end]);
    Ok(output)
}

pub(super) fn srgb_without_alpha(
    image: VipsImageHandle<'_>,
) -> Result<VipsImageHandle<'_>, MediaError> {
    let mut raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_srgb_without_alpha(image.as_ptr(), &mut raw) };
    derive(&image, raw, rc)
}

pub(super) fn orientation(image: &VipsImageHandle<'_>) -> i32 {
    let field = c"orientation";
    let mut value = 1;
    let present =
        unsafe { native::fluxer_vips_image_has_field(image.as_ptr(), field.as_ptr()) } != 0;
    if present
        && unsafe { native::fluxer_vips_image_get_int(image.as_ptr(), field.as_ptr(), &mut value) }
            == 0
    {
        value
    } else {
        1
    }
}

pub(super) fn reencode_upright(
    input: &[u8],
    media_limits: &MediaLimits,
    deadline_ms: Option<i64>,
) -> Result<Vec<u8>, MediaError> {
    let layout = parse_layout(input)?;
    let facts = frame_facts(&layout.header)?;
    let quality = estimate_quality(&layout.header).unwrap_or(DEFAULT_REENCODE_QUALITY);
    let image = load_image(input, "")?;
    validate_vips_image(media_limits, &image)?;
    let mut raw = ptr::null_mut();
    let rc = unsafe { native::fluxer_vips_autorot(image.as_ptr(), 0, &mut raw) };
    let mut upright = derive(&image, raw, rc)?;
    let cmyk = facts.components == 4;
    if cmyk {
        upright = srgb_without_alpha(upright)?;
    }
    let subsample = if facts.full_chroma { "off" } else { "on" };
    let suffix = CString::new(format!(
        ".jpg[Q={quality},strip,optimize_coding=true,subsample_mode={subsample}]"
    ))
    .map_err(|_| MediaError::MediaEncodeFailed)?;
    let encoded = write_vips_image_to_vec(
        &upright,
        &suffix,
        constants::MAX_MEDIA_PROXY_BYTES,
        deadline_ms,
    )?;
    let icc: Vec<&Segment<'_>> = if cmyk {
        Vec::new()
    } else {
        layout
            .header
            .iter()
            .filter(|segment| is_icc(segment))
            .collect()
    };
    if icc.is_empty() {
        return Ok(encoded);
    }
    insert_after_jfif(encoded, &icc)
}
