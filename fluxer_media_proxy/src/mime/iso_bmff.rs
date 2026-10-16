// SPDX-License-Identifier: AGPL-3.0-or-later

use super::SniffInfo;
use crate::media_type::MediaType;

const ISO_BMFF_BRAND_SCAN_BYTES: usize = 128;
const ISO_BMFF_BASE_HEADER_BYTES: usize = 8;
const ISO_BMFF_EXTENDED_HEADER_BYTES: usize = 16;
const ISO_BMFF_FTYP_FIELDS_BYTES: usize = 8;

struct FTYPLayout {
    major_brand_offset: usize,
    compatible_brands_offset: usize,
    scan_end: usize,
}

#[derive(Default)]
struct ISOBMFFBrandSet {
    avif: bool,
    animated_avif: bool,
    heic: bool,
    heif: bool,
    audio_mp4: bool,
    mp4: bool,
    quicktime: bool,
    three_gpp: bool,
}

impl ISOBMFFBrandSet {
    fn record(&mut self, brand: &[u8]) {
        if matches!(brand, b"avif" | b"avis" | b"avio") {
            self.avif = true;
            if brand == b"avis" {
                self.animated_avif = true;
            }
        }
        if matches!(
            brand,
            b"heic" | b"heix" | b"heif" | b"heim" | b"heis" | b"hevc" | b"hevx" | b"hevm" | b"hevs"
        ) {
            self.heic = true;
        }
        if matches!(brand, b"mif1" | b"msf1") {
            self.heif = true;
        }
        if matches!(
            brand,
            b"mp41"
                | b"mp42"
                | b"isom"
                | b"iso2"
                | b"iso3"
                | b"iso4"
                | b"iso5"
                | b"iso6"
                | b"M4V "
                | b"M4P "
                | b"dash"
                | b"msdh"
                | b"msix"
                | b"mj2s"
        ) {
            self.mp4 = true;
        }
        if matches!(brand, b"M4A " | b"M4B " | b"M4P ") {
            self.audio_mp4 = true;
        }
        if brand == b"qt  " {
            self.quicktime = true;
        }
        if brand.starts_with(b"3gp") {
            self.three_gpp = true;
        }
        if brand.starts_with(b"3g2") {
            self.three_gpp = true;
        }
    }

    fn resolve(self) -> Option<SniffInfo> {
        if self.avif {
            return Some(SniffInfo {
                mime: MediaType::AVIF.mime(),
                animated: self.animated_avif,
                ..Default::default()
            });
        }
        if self.heic {
            return Some(SniffInfo {
                mime: MediaType::HEIC.mime(),
                ..Default::default()
            });
        }
        if self.heif {
            return Some(SniffInfo {
                mime: MediaType::HEIF.mime(),
                ..Default::default()
            });
        }
        if self.quicktime {
            return Some(SniffInfo {
                mime: MediaType::QuickTimeVideo.mime(),
                ..Default::default()
            });
        }
        if self.three_gpp {
            return Some(SniffInfo {
                mime: MediaType::ThreeGPPVideo.mime(),
                ..Default::default()
            });
        }
        if self.audio_mp4 {
            return Some(SniffInfo {
                mime: MediaType::MP4Audio.mime(),
                ..Default::default()
            });
        }
        if self.mp4 {
            return Some(SniffInfo {
                mime: MediaType::MP4Video.mime(),
                ..Default::default()
            });
        }
        None
    }
}

fn ftyp_layout(data: &[u8]) -> Option<FTYPLayout> {
    if data.len() < ISO_BMFF_BASE_HEADER_BYTES {
        return None;
    }
    if &data[4..8] != b"ftyp" {
        return None;
    }
    let size32 = u32::from_be_bytes(
        data[0..4]
            .try_into()
            .expect("validated ISO BMFF box size slice"),
    );
    let (header_bytes, declared_box_size) = if size32 == 1 {
        if data.len() < ISO_BMFF_EXTENDED_HEADER_BYTES {
            return None;
        }
        let size64 = u64::from_be_bytes(
            data[8..16]
                .try_into()
                .expect("validated extended ISO BMFF box size slice"),
        );
        let box_size = usize::try_from(size64).ok()?;
        (ISO_BMFF_EXTENDED_HEADER_BYTES, Some(box_size))
    } else if size32 == 0 {
        (ISO_BMFF_BASE_HEADER_BYTES, None)
    } else {
        let box_size = usize::try_from(size32).expect("u32 ISO BMFF box size must fit usize");
        (ISO_BMFF_BASE_HEADER_BYTES, Some(box_size))
    };
    let box_size = declared_box_size.unwrap_or(data.len());
    let minimum_size = header_bytes.checked_add(ISO_BMFF_FTYP_FIELDS_BYTES)?;
    if box_size < minimum_size {
        return None;
    }
    let major_brand_end = header_bytes.checked_add(4)?;
    let scan_end = match declared_box_size {
        Some(declared) if declared <= data.len() => declared,
        _ => data.len().min(ISO_BMFF_BRAND_SCAN_BYTES),
    };
    if major_brand_end > scan_end {
        return None;
    }
    Some(FTYPLayout {
        major_brand_offset: header_bytes,
        compatible_brands_offset: minimum_size,
        scan_end,
    })
}

pub(super) fn iso_bmff_sniff(data: &[u8]) -> Option<SniffInfo> {
    let layout = ftyp_layout(data)?;
    let mut brands = ISOBMFFBrandSet::default();
    brands.record(&data[layout.major_brand_offset..layout.major_brand_offset + 4]);
    let mut offset = layout.compatible_brands_offset;
    loop {
        let end = offset
            .checked_add(4)
            .expect("bounded ISO BMFF brand offset must fit usize");
        if end > layout.scan_end {
            break;
        }
        brands.record(&data[offset..end]);
        offset = end;
    }
    brands.resolve()
}
